{-# LANGUAGE BangPatterns, ConstraintKinds, LambdaCase,
             ScopedTypeVariables, TupleSections #-}
-- | Front-end interface to the pre-processor.
module Hpp (parseDefinition, preprocess,
            hppReadFile, hppIO, HppCaps, hppFileContents) where
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Char (isSpace)
import Data.List (isPrefixOf, uncons)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Hpp.Config (Config, curFileNameF, curFileName, includePaths,
                   eraseCComments, spliceLongLines, inhibitLinemarkers,
                   replaceTrigraphs)
import Hpp.Env (deleteKey, insertPair, lookupKey)
import Hpp.Expansion (expandLine)
import Hpp.Expr (evalExpr, parseExpr)
import Hpp.Parser (Parser, ParserT, replace, await, awaitJust, droppingWhile,
                   precede, takingWhile, insertInputSegment, onElements,
                   evalParse)
import Hpp.String (stringify, trimSpaces, unquote, cons, breakOn)
import Hpp.Tokens (Token(..), importants, isImportant, newLine, trimUnimportant,
                   detokenize, notImportant, tokenize, skipLiteral)
import Hpp.Types
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- * Trigraphs

-- | The first component of each pair represents the end of a known
-- trigraph sequence (each trigraph begins with two consecutive
-- question marks (@"??"@). The second component is the
-- single-character equivalent that we substitute in for the trigraph.
trigraphs :: [(Char, Char)]
trigraphs = [ ('=', '#')
            , ('/', '\\')
            , ('\'', '^')
            , ('(', '[')
            , (')', ']')
            , ('!', '|')
            , ('<', '{')
            , ('>', '}')
            , ('-', '~') ]

trigraphReplacement :: String -> String
trigraphReplacement = aux . breakOn "??"
  where aux (s,[]) = s
        aux (h,t) =
          case uncons (drop 2 t) of
            Nothing -> h <> "??"
            Just (c,t') ->
              case lookup c trigraphs of
                Just c' -> h <> cons c' (trigraphReplacement t')
                Nothing -> h <> "?" <> trigraphReplacement (cons '?' (cons c t'))

-- * Line Splicing

-- | If a line ends with a backslash, it is prepended to the following
-- the line.
lineSplicing :: [String] -> [String]
lineSplicing = go id
  where go acc [] = [acc []]
        go acc ([]:lns) = acc [] : go id lns
        go acc (ln:lns)
          | last ln == '\\' = go (acc . (init ln ++)) lns
          | otherwise = acc ln : go id lns
{-# INLINE lineSplicing #-}

-- * C Comments

breakBlockCommentStart :: String -> Maybe (String, String)
breakBlockCommentStart = go id
  where go _ [] = Nothing
        go acc ('"' : ts) = skipLiteral (go . (acc .)) ts
        go acc ('/' : '*' : t) = Just (acc [], t)
        go acc (c:cs) = go (acc . (c:)) cs

breakBlockCommentEnd :: String -> Maybe String
breakBlockCommentEnd [] = Nothing
breakBlockCommentEnd (_:'"':cs) = skipLiteral (const breakBlockCommentEnd) cs
breakBlockCommentEnd ('*':'/':t) = Just (' ':t)
breakBlockCommentEnd (_:cs) = breakBlockCommentEnd cs

dropOneLineBlockComments :: String -> String
dropOneLineBlockComments [] = []
dropOneLineBlockComments (c:'"':cs) =
  c : skipLiteral (\x y -> x [] ++ dropOneLineBlockComments y) cs
dropOneLineBlockComments ('/':'*':cs) = go cs
  where go [] = "/*"
        go ('*':'/':t)
          | all isSpace t = t
          | otherwise = ' ' : dropOneLineBlockComments t
        go (_:t) = go t
dropOneLineBlockComments (c:cs) = c : dropOneLineBlockComments cs

-- dropLineComments :: String -> String
-- dropLineComments = fst . breakOn "//"

removeMultilineComments :: Int -> [String] -> [String]
removeMultilineComments !lineStart = goStart lineStart
  where goStart _ [] = []
        goStart !curLine (ln:lns) =
          case breakBlockCommentStart ln of
            Nothing -> ln : goStart (curLine+1) lns
            Just (pre,_) -> goEnd (curLine+1) pre lns
        goEnd _ _ [] = error "Unmatched /*"
        goEnd !curLine pre (ln:lns) =
          case breakBlockCommentEnd ln of
            Nothing -> goEnd (curLine+1) pre lns
            Just pos
              | all isSpace (pre++pos) ->
                ("#line "++show (curLine+1)) : goStart (curLine + 1) lns
              | otherwise -> (pre++pos)
                             : ("#line "++show (curLine+1))
                             : goStart (curLine+1) lns
{-# INLINE removeMultilineComments #-}

commentRemoval :: [String] -> [String]
-- commentRemoval = map dropLineComments . removeMultilineComments 1
--                . map dropOneLineBlockComments
commentRemoval = removeMultilineComments 1
               . map dropOneLineBlockComments

-- * Token Splices

-- | Deal with the two-character '##' token pasting/splicing
-- operator. We do so eliminating spaces around the @##@
-- operator.
prepTokenSplices :: [Token] -> [Token]
prepTokenSplices = dropSpaces [] . mergeTokens []
  where -- Merges ## tokens, and reverses the input list
        mergeTokens acc [] = acc
        mergeTokens acc (Important "#" : Important "#" : ts) =
          mergeTokens (Important "##" : acc) (dropWhile (not . isImportant) ts)
        mergeTokens acc (t:ts) = mergeTokens (t : acc) ts
        -- Drop trailing spaces and re-reverse the list
        dropSpaces acc [] = acc
        dropSpaces acc (t@(Important "##") : ts) =
          dropSpaces (t : acc) (dropWhile (not . isImportant) ts)
        dropSpaces acc (t:ts) = dropSpaces (t : acc) ts

-- * Function-like macros as Haskell functions

-- | @functionMacro parameters body arguments@ substitutes @arguments@
-- for @parameters@ in @body@ and performs stringification for uses of
-- the @#@ operator and token concatenation for the @##@ operator.
functionMacro :: [String] -> [Token] -> [([Scan],String)] -> [Scan]
functionMacro params body = paste
                          . subst body'
                          -- . M.fromList
                          . zip params
  where subst toks gamma = go toks
          where go [] = []
                go (p@(Important "##"):t@(Important s):ts) =
                  case lookupKey s gamma of
                    Nothing -> Rescan p : Rescan t : go ts
                    Just ((_,arg),_) ->
                      Rescan p : Rescan (Important arg) : go ts
                go (t@(Important s):p@(Important "##"):ts) =
                  case lookupKey s gamma of
                    Nothing -> Rescan t : go (p:ts)
                    Just ((_,arg),_) -> Rescan (Important arg) : go (p:ts)
                go (t@(Important "##"):ts) = Rescan t : go ts
                go (t@(Important ('#':s)) : ts) =
                  case lookupKey s gamma of
                    Nothing -> Rescan t : go ts
                    Just ((_,arg),_) ->
                      Rescan (Important (stringify arg)) : go ts
                go (t@(Important s) : ts) =
                  case lookupKey s gamma of
                    Nothing -> Rescan t : go ts
                    Just ((arg,_),_) -> arg ++ go ts
                go (t:ts) = Rescan t : go ts
        prepStringify [] = []
        prepStringify (Important "#" : ts) =
          case dropWhile (not . isImportant) ts of
            (Important t : ts') -> Important ('#':t) : prepStringify ts'
            _ -> Important "#" : ts
        prepStringify (t:ts) = t : prepStringify ts

        body' = prepStringify . prepTokenSplices $
                dropWhile (not . isImportant) body
        paste [] = []
        paste (Rescan (Important s) : Rescan (Important "##")
              : Rescan (Important t) : ts) =
          paste (Rescan (Important (trimSpaces s ++ dropWhile isSpace t)) : ts)
        paste (t:ts) = t : paste ts

-- * Pre-Processor Capabilities

modifyState :: (Monad m, HasHppState m) => (HppState -> HppState) -> m ()
modifyState f = getState >>= setState . f

-- | Run a Stream with a configuration for a new file.
streamNewFile :: (Monad m, HasHppState m)
              => FilePath -> [[Token]] -> Parser m [Token] ()
streamNewFile fp s =
  do (oldCfg,oldLine) <- do st <- getState
                            let cfg = hppConfig st
                                cfg' = cfg { curFileNameF = pure fp }
                                ln = hppLineNum st
                            setState (st {hppConfig = cfg', hppLineNum = 1})
                            return (cfg, ln)
     insertInputSegment
       s (modifyState (setL lineNum oldLine . setL config oldCfg))

-- * Finding @include@ files

includeCandidates :: [FilePath] -> String -> Maybe [FilePath]
includeCandidates searchPath nm =
  case nm of
    '<':nm' -> Just $ sysSearch (init nm')
    '"':nm' -> let nm'' = init nm'
               in Just $ nm'' : sysSearch nm''
    _ -> Nothing
  where sysSearch f = map (</> f) searchPath

searchForInclude :: [FilePath] -> String -> IO (Maybe FilePath)
searchForInclude paths = maybe (return Nothing) aux . includeCandidates paths
  where aux [] = return Nothing
        aux (f:fs) = do exists <- doesFileExist f
                        if exists then return (Just f) else aux fs

searchForNextInclude :: [FilePath] -> String -> IO (Maybe FilePath)
searchForNextInclude paths = maybe (return Nothing) (aux False)
                           . includeCandidates paths
  where aux _ [] = return Nothing
        aux n (f:fs) = do exists <- doesFileExist f
                          if exists
                          then if n
                               then return (Just f)
                               else aux True fs
                          else aux n fs

-- * Running an Hpp Action

runHpp :: forall m a src. (MonadIO m, HasHppState m)
       => (FilePath -> m src)
       -> (src -> m ())
       -> HppT src m a
       -> m (Either (FilePath,Error) a)
runHpp source sink m = runHppT m >>= go
  where go :: FreeF (HppF src) a (HppT src m a)
           -> m (Either (FilePath, Error) a)
        go (PureF x) = return $ Right x
        go (FreeF s) = case s of
          ReadFile ln file k ->
            (includePaths <$> use config)
            >>= liftIO . flip searchForInclude file
            >>= readAux ln file k
          ReadNext ln file k ->
            (includePaths <$> use config)
            >>= liftIO . flip searchForNextInclude file
            >>= readAux ln file k
          WriteOutput output k -> sink output >> runHppT k >>= go

        readAux ln file _ Nothing =
          Left . (, IncludeDoesNotExist ln file) . curFileName <$> use config
        readAux _ln _file k (Just file') =
          source file' >>= runHppT . k >>= go
{-# SPECIALIZE runHpp ::
    (FilePath -> Parser (StateT HppState (ExceptT Error IO)) [Token] [String])
 -> ([String] -> Parser (StateT HppState (ExceptT Error IO)) [Token] ())
 -> HppT [String] (Parser (StateT HppState (ExceptT Error IO)) [Token]) a
 -> Parser (StateT HppState (ExceptT Error IO)) [Token] (Either (FilePath,Error) a) #-}

-- * Preprocessor

-- | Parse the definition of an object-like or function macro.
parseDefinition :: [Token] -> Maybe (String, Macro)
parseDefinition toks =
  case dropWhile (not . isImportant) toks of
    (Important name:Important "(":rst) ->
      let params = takeWhile (/= ")") $ filter (/= ",") (importants rst)
          body = trimUnimportant . tail $ dropWhile (/= Important ")") toks
          macro = Function (length params) (functionMacro params body)
      in Just (name, macro)
    (Important name:_) ->
      let rhs = case dropWhile (/= Important name) toks of
                  [] -> [Important ""]
                  str@(_:t)
                    | all (not . isImportant) str -> [Important ""]
                    | otherwise -> trimUnimportant t
      in Just (name, Object rhs)
    _ -> Nothing

-- | Returns everything up to the next newline. The newline character
-- itself is consumed.
takeLine :: (Monad m, HasError m, HasHppState m) => Parser m [Token] [Token]
takeLine = (onElements $ do
              ln <- takingWhile (not . newLine)
              eat <- awaitJust "takeLine" -- Eat the newline character
              case eat of
                Other "\n" -> return ()
                wat -> error $ "Expected newline: "++show wat++" after "++show ln
              return ln)
           <* (lineNum %= (+1))

dropLine :: (Monad m, HasError m, HasHppState m) => Parser m [Token] ()
dropLine = do onElements $ do
                droppingWhile (not . newLine)
                eat <- awaitJust "dropLine" -- Eat the newline character
                case eat of
                  Other "\n" -> return ()
                  wat -> error $ "Expected dropped newline: "++show wat
              lineNum %= (+1)

-- * State Zooming

expandLineP :: (Monad m, HasHppState m, HasEnv m, HasError m)
            => Parser m [Token] [Token]
expandLineP =
  do st <- getState
     let ln = hppLineNum st
         cfg = hppConfig st
     expandLine cfg ln

hppReadFile :: Monad m => Int -> FilePath -> HppT src m src
hppReadFile n file = HppT (pure (FreeF (ReadFile n file return)))

hppReadNext :: Monad m => Int -> FilePath -> HppT src m src
hppReadNext n file = HppT (pure (FreeF (ReadNext n file return)))

-- * Directive Processing

-- | Handle preprocessor directives (commands prefixed with an octothorpe).
directive :: forall m. (Monad m, HppCaps m)
          => HppT [String] (Parser m [Token]) Bool
directive = lift (onElements (awaitJust "directive")) >>= aux
  where aux :: Token -> HppT [String] (Parser m [Token]) Bool
        aux (Important cmd) = case cmd of
          "pragma" -> True <$ lift dropLine -- Ignored
          "define" -> True <$
                      (lift $ fmap parseDefinition takeLine >>= \case
                        Nothing -> use lineNum >>=
                                   throwError . BadMacroDefinition
                        Just def -> env %= insertPair def)
          "undef" -> do name <- lift . onElements $ do
                          droppingWhile (not . isImportant)
                          Important name <- awaitJust "undef"
                          return name
                        lift dropLine
                        env %= deleteKey name
                        return True
          "include" -> True <$ includeAux hppReadFile
          "include_next" -> True <$ includeAux hppReadNext
          "line" -> do lift (onElements droppingSpaces)
                       toks <- lift (init <$> expandLineP)
                       case toks of
                         Important n:optFile ->
                           case readMaybe n of
                             Nothing -> use lineNum >>=
                                        throwError . flip BadLineArgument n
                             Just ln' -> do
                               unless (null optFile) $ do
                                 let fn = unquote . detokenize
                                        . dropWhile (not . isImportant)
                                        $ optFile
                                 config %= (\cfg -> cfg { curFileNameF = pure fn })
                               lineNum .= ln'
                               return True
                         _ -> use lineNum >>=
                              throwError . flip BadLineArgument (detokenize toks)
          "ifdef" ->
            do toks <- lift (onElements droppingSpaces >> takeLine)
               ln <- use lineNum
               case takeWhile isImportant toks of
                 [Important t] ->
                   lookupMacro t >>= \case
                     Nothing ->
                       lift (dropBranchLine ln >>= replace . fst)
                     Just _ ->
                       lift (takeBranch ln >>= precede)
                 _ -> throwError . UnknownCommand ln $
                      "ifdef "++detokenize toks
               return True
          "ifndef" ->
            do toks <- lift (onElements droppingSpaces >> takeLine)
               ln <- use lineNum
               case takeWhile isImportant toks of
                 [Important t] ->
                   lookupMacro t >>= \case
                      Nothing -> lift (takeBranch ln >>= precede)
                      Just _ -> lift (dropBranchLine ln >>= replace . fst)
                 _ -> throwError . UnknownCommand ln $
                      "ifndef "++detokenize toks
               return True
          "else" -> True <$ lift dropLine
          "if" -> True <$ ifAux
          "elif" -> True <$ ifAux
          "endif" -> True <$ lift dropLine
          "error" -> do toks <- lift (onElements droppingSpaces >> takeLine)
                        ln <- subtract 1 <$> use lineNum
                        curFile <- curFileName <$> use config
                        throwError $ UserError ln (detokenize toks++" ("++curFile++")")
          "warning" -> True <$ lift dropLine -- warnings not yet supported
          -- t -> do toks <- lift takeLine
          --         ln <- subtract 1 <$> use lineNum
          --         throwError $ UnknownCommand ln (detokenize (Important t:toks))
          _ -> return False -- Ignore unknown command
        aux _ = error "Impossible unimportant directive"
        includeAux :: (LineNum -> FilePath -> HppT src (Parser m [Token]) [String])
                   -> HppT src (Parser m [Token]) ()
        includeAux readFun =
          do fileName <- lift (detokenize . trimUnimportant . init <$> expandLineP)
             ln <- use lineNum
             src <- readFun ln fileName
             lineNum .= ln+1
             fmap ($src) prepareInput >>= lift . streamNewFile (unquote fileName)
        ifAux =
          do toks <- lift (onElements droppingSpaces >> takeLine)
             e <- use env
             ln <- use lineNum
             lineNum .= ln - 1 -- takeLine incremented the line count
             ex <- lift (lift (evalParse expandLineP [squashDefines e toks]))
             let res = evalExpr <$> parseExpr ex
             lineNum .= ln
             if maybe False (/= 0) res
               then lift (takeBranch ln >>= precede)
               else lift (dropBranchLine ln >>= replace . fst)

-- | We want to expand macros in expressions that must be evaluated
-- for conditionals, but we want to take special care when dealing
-- with the meta @defined@ operator of the expression language that is
-- a predicate on the evaluation environment.
squashDefines :: Env -> [Token] -> [Token]
squashDefines _ [] = []
squashDefines env' (Important "defined" : ts) = go ts
  where go (t@(Other _) : ts') = t : go ts'
        go (t@(Important "(") : ts') = t : go ts'
        go (Important t : ts') =
          case lookupKey t env' of
            Nothing -> Important "0" : squashDefines env' ts'
            Just (_,env'') -> Important "1" : squashDefines env'' ts'
        go [] = []
squashDefines env' (t : ts) = t : squashDefines env' ts

getCmd :: [Token] -> Maybe String
getCmd = aux . dropWhile notImportant
  where aux (Important "#" : ts) = case dropWhile notImportant ts of
                                     (Important cmd:_) -> Just cmd
                                     _ -> Nothing
        aux _ = Nothing

droppingSpaces ::(Monad m) => ParserT m src Token ()
droppingSpaces = droppingWhile notImportant

-- | Take an entire conditional expression (e.g. @#if
-- ... #endif@). All the lines of the taken branch are returned, in
-- reverse order.
takeConditional :: (HasError m, Monad m)
                => LineNum
                -> Parser m [Token] ([[Token]], LineNum)
takeConditional !n0 = go (1::Int) id n0
  where go 0 acc !n = return (acc [], n)
        go nesting acc !n =
          do ln <- awaitJust "takeConditional"
             case getCmd ln of
               Just cmd
                 | cmd == "endif" ->
                   go (nesting-1) (acc . (ln:)) (n+1)
                 | cmd `elem` ["if","ifdef","ifndef"] ->
                   go (nesting+1) (acc . (ln:)) (n+1)
               _ -> go nesting (acc . (ln:)) (n+1)

-- | Take everything up to the end of this branch, drop all remaining
-- branches (if any).
takeBranch :: (HasError m, Monad m) => LineNum -> Parser m [Token] [[Token]]
takeBranch = go id
  where go acc !n = do ln <- awaitJust "takeBranch"
                       case getCmd ln of
                         Just cmd
                           | cmd `elem` ["if","ifdef","ifndef"] ->
                             do (lns, n') <- takeConditional (n+1)
                                go (acc . ((ln:lns)++)) n'
                           | cmd == "endif" -> return (acc [yieldLineNum n])
                           | cmd `elem` ["else","elif"] ->
                             do numSkipped <- dropAllBranches
                                return (acc [yieldLineNum (n+1+numSkipped)])
                         _ -> go (acc . (ln:)) (n+1)

yieldLineNum :: LineNum -> [Token]
yieldLineNum !ln = [Important ("#line "++show ln), Other "\n"]

dropAllBranches :: (HasError m, Monad m) => Parser m [Token] Int
dropAllBranches = dropBranch >>= uncurry (aux 0)
  where aux !acc Nothing !numDropped = return (acc+numDropped)
        aux !acc _ !numDropped = dropBranch >>= uncurry (aux (acc+numDropped))

dropBranchLine :: (HasError m, Monad m)
               => LineNum -> Parser m [Token] ([Token], LineNum)
dropBranchLine !ln = do (el, numSkipped) <- dropBranch
                        let ln' = ln + numSkipped
                        return (yieldLineNum ln' ++ fromMaybe [] el, ln')

-- | Skip to the end of a conditional branch. Returns the 'Just' the
-- token that ends this branch if it is an @else@ or @elif@, or
-- 'Nothing' otherwise, and the number of lines skipped.
dropBranch :: (HasError m, Monad m) => Parser m [Token] (Maybe [Token], Int)
dropBranch = go (1::Int) 0
  where go !nesting !n =
          do ln <- awaitJust "dropBranch"
             case getCmd ln of
               Just cmd
                 | cmd == "endif" -> if nesting == 1
                                     then return (Nothing, n+1)
                                     else go (nesting-1) (n+1)
                 | cmd `elem` ["if","ifdef","ifndef"] ->
                   go (nesting+1) (n+1)
                 | cmd `elem` ["else", "elif"] -> if nesting == 1
                                                  then return (Just ln, n+1)
                                                  else go nesting (n+1)
               _ -> go nesting (n+1)

-- | Expands an input line producing a stream of output lines.
macroExpansion :: (Monad m, HppCaps m)
               => HppT [String] (Parser m [Token]) (Maybe [Token])
macroExpansion = do
  lift await >>= \case
    Nothing -> return Nothing
    Just ln ->
      -- when (not (all isSpace (detokenize ln)))
      --      (trace ("macro expand: "++detokenize ln) (return ())) >>
      case dropWhile notImportant ln of
        [] -> Just ln <$ (lineNum %= (+1))
        Important "#":rst -> do lift (replace (dropWhile notImportant rst))
                                processed <- directive
                                if processed
                                then macroExpansion
                                else Just ln <$ lift takeLine
        _ -> lift (replace ln >> (Just <$> expandLineP)) <* (lineNum %= (+1))

-- | The dynamic capabilities offered by HPP
type HppCaps t = (HasError t, HasHppState t, HasEnv t)

parseStreamHpp :: Monad m
               => HppT t (Parser m i) (Maybe t) -> HppT t (Parser m i) ()
parseStreamHpp m = go
  where go = m >>= \case
          Nothing -> return ()
          Just o -> writeOutput o >> go

-- * HPP configurations

-- | Standard CPP settings for processing C files.
normalCPP :: [String] -> [[Token]]
normalCPP = map ((++ [Other "\n"]) . tokenize)
          . lineSplicing
          -- . map dropLineComments
          . removeMultilineComments 1
          . map (dropOneLineBlockComments . trigraphReplacement)

-- | For Haskell we do not want trigraph replacement.
haskellCPP :: [String] -> [[Token]]
haskellCPP = map ((++[Other "\n"]) . tokenize)
           . lineSplicing
           . commentRemoval

-- | If we don't have a predefined processor, we build one based on a
-- 'Config' value.
genericConfig :: Config -> [String] -> [[Token]]
genericConfig cfg = map ((++ [Other "\n"]) . tokenize)
                  . (if spliceLongLines cfg then lineSplicing else id)
                  . (if eraseCComments cfg then commentRemoval else id)
                  . (if replaceTrigraphs cfg then map trigraphReplacement else id)

-- * Front End

prepareInput :: (Monad m, HppCaps m) => m ([String] -> [[Token]])
prepareInput =
  do cfg <- getL config <$> getState
     case () of
       _ | eraseCComments cfg && spliceLongLines cfg
           && not (inhibitLinemarkers cfg) -> pure normalCPP
       _ | (eraseCComments cfg && spliceLongLines cfg
            && (not (replaceTrigraphs cfg))) ->
           pure haskellCPP
       _ | otherwise -> pure (genericConfig cfg)

-- | Run a stream of lines through the preprocessor.
preprocess :: (Monad m, HppCaps m)
           => ([String] -> src) -> [String] -> HppT [String] (Parser m [Token]) ()
preprocess _convertOutput src =
  do cfg <- getL config <$> getState
     prep <- prepareInput
     let prepOutput = if inhibitLinemarkers cfg then aux else pure
     lift (precede (prep src))
     parseStreamHpp (fmap (prepOutput . detokenize) <$> macroExpansion)
  where aux xs | isPrefixOf "#line" xs = []
               | otherwise = [xs]

-- | General hpp runner against input source file lines; can return an
-- 'Error' value if something goes wrong.
hppIO' :: Config -> Env -> ([String] -> IO ()) -> [String] -> IO (Maybe Error)
hppIO' cfg env' snk src =
  runExceptT'
    (evalStateT
       (evalParse
          ((>>= either (throwError . snd) return)
           (runHpp (liftIO . fmap lines . readFile)
                   (liftIO . snk)
                   (preprocess id src)))
          [])
       initialState) >>= return . either Just (const Nothing)
  where initialState = setL env env' $ emptyHppState cfg
        runExceptT' = runExceptT :: ExceptT Error m a -> m (Either Error a)

-- | General hpp runner against input source file lines. Any errors
-- encountered are thrown with 'error'.
hppIO :: Config -> Env -> ([String] -> IO ()) -> [String] -> IO ()
hppIO cfg env' snk = fmap (maybe () (error . show)) . hppIO' cfg env' snk

-- | hpp runner that returns output lines.
hppFileContents :: Config -> Env ->  FilePath -> [String] -> IO (Either Error [String])
hppFileContents cfg env' fileName src = do
  r <- newIORef id
  let snk xs = modifyIORef r (. (xs++))
  hppIO' (cfg {curFileNameF = pure fileName}) env' snk src >>= \case
    Nothing -> Right . ($ []) <$> readIORef r
    Just e -> return (Left e)
