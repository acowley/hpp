{-# LANGUAGE BangPatterns, ConstraintKinds, LambdaCase, OverloadedStrings,
             ScopedTypeVariables, TupleSections, ViewPatterns #-}
-- | Mid-level interface to the pre-processor.
module Hpp.RunHpp (parseDefinition, preprocess, runHpp, expandHpp,
                   hppIOSink, HppCaps, hppIO, HppResult(..)) where
import Control.Arrow (first)
import Control.Exception (throwIO)
import Control.Monad (unless, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, State)
import Data.Char (isSpace)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Hpp.Config (Config, curFileNameF, curFileName, includePaths,
                   eraseCComments, spliceLongLines, inhibitLinemarkers,
                   replaceTrigraphs)
import Hpp.Env (deleteKey, insertPair, lookupKey)
import Hpp.Expansion (expandLine)
import Hpp.Expr (evalExpr, parseExpr)
import Hpp.Parser (Parser, ParserT, replace, await, awaitJust, droppingWhile,
                   precede, takingWhile, insertInputSegment, onElements,
                   evalParse, onInputSegment)
import Hpp.StringSig
import Hpp.Tokens (Token(..), importants, isImportant, newLine, trimUnimportant,
                   detokenize, notImportant, tokenize, skipLiteral)
import Hpp.Types
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)
import Prelude hiding (String)
import qualified Prelude as P

-- * Trigraphs

-- | The first component of each pair represents the end of a known
-- trigraph sequence (each trigraph begins with two consecutive
-- question marks (@\"??\"@). The second component is the
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

trigraphReplacement :: Stringy s => s -> s
trigraphReplacement s = aux (breakOn [("??", ())] s)
  where aux Nothing = s
        aux (Just (_, pre, pos)) =
          case uncons pos of
            Nothing -> pre <> "??"
            Just (c,t) ->
              case lookup c trigraphs of
                Just c' -> snoc pre c' <> trigraphReplacement t
                Nothing -> snoc pre '?' <> trigraphReplacement (cons '?' pos)

-- * Line Splicing

-- | If a line ends with a backslash, it is prepended to the following
-- the line.
lineSplicing :: Stringy s => [s] -> [s]
lineSplicing = go id
  where go acc [] = [acc mempty]
        go acc (ln:lns) = case unsnoc ln of
                            Nothing -> acc mempty : go id lns
                            Just (ini, '\\') -> go (acc . (ini<>)) lns
                            Just _ -> acc ln : go id lns
{-# INLINE lineSplicing #-}

-- * C Comments

breakBlockCommentStart :: Stringy s => s -> Maybe (s, s)
breakBlockCommentStart s =
  case breakCharOrSub '"' "/*" s of
    NoMatch -> Nothing
    CharMatch pre pos -> let (lit, rest) = skipLiteral pos
                         in first ((pre <> lit) <>) <$>
                            breakBlockCommentStart rest
    SubMatch pre pos -> Just (pre, pos)

breakBlockCommentEnd :: Stringy s => s -> Maybe s
breakBlockCommentEnd s =
  case breakCharOrSub '"' "*/" s of
    NoMatch -> Nothing
    CharMatch _ pos -> let (_, rest) = skipLiteral pos
                       in breakBlockCommentEnd rest
    SubMatch _ pos -> Just pos

dropOneLineBlockComments :: Stringy s => s -> s
dropOneLineBlockComments s =
  case breakCharOrSub '"' "/*"s of
    NoMatch -> s
    CharMatch pre pos ->
      let (lit,rest) = skipLiteral pos
      in snoc pre '"' <> lit <> dropOneLineBlockComments rest
    SubMatch pre pos ->
      case breakOn [("*/", ())] pos of
        Nothing -> pre <> "/*"
        Just (_,_,pos') -> snoc pre ' ' <> dropOneLineBlockComments pos'

removeMultilineComments :: Stringy s => Int -> [s] -> [s]
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
              | sall isSpace (pre<>pos) ->
                ("#line "<> fromString (show (curLine+1))) : goStart (curLine + 1) lns
              | otherwise -> (pre<>pos)
                             : ("#line "<> fromString (show (curLine+1)))
                             : goStart (curLine+1) lns

commentRemoval :: Stringy s => [s] -> [s]
commentRemoval = removeMultilineComments 1 . map dropOneLineBlockComments

-- * TOKEN Splices

-- | Deal with the two-character '##' token pasting/splicing
-- operator. We do so eliminating spaces around the @##@
-- operator.
prepTOKENSplices :: [TOKEN] -> [TOKEN]
prepTOKENSplices = map (fmap copy) . dropSpaces [] . mergeTOKENs []
  where -- Merges ## tokens, and reverses the input list
        mergeTOKENs acc [] = acc
        mergeTOKENs acc (Important "#" : Important "#" : ts) =
          mergeTOKENs (Important "##" : acc) (dropWhile (not . isImportant) ts)
        mergeTOKENs acc (t:ts) = mergeTOKENs (t : acc) ts
        -- Drop trailing spaces and re-reverse the list
        dropSpaces acc [] = acc
        dropSpaces acc (t@(Important "##") : ts) =
          dropSpaces (t : acc) (dropWhile (not . isImportant) ts)
        dropSpaces acc (t:ts) = dropSpaces (t : acc) ts

-- * Function-like macros as Haskell functions

-- | @functionMacro parameters body arguments@ substitutes @arguments@
-- for @parameters@ in @body@ and performs stringification for uses of
-- the @#@ operator and token concatenation for the @##@ operator.
functionMacro :: [String] -> [TOKEN] -> [([Scan],String)] -> [Scan]
functionMacro params body = paste
                          . subst body'
                          -- . M.fromList
                          . zip params'
  where params' = map copy params
        subst toks gamma = go toks
          where go [] = []
                go (p@(Important "##"):t@(Important s):ts) =
                  case lookup s gamma of
                    Nothing -> Rescan p : Rescan t : go ts
                    Just (_,arg) ->
                      Rescan p : Rescan (Important arg) : go ts
                go (t@(Important s):p@(Important "##"):ts) =
                  case lookup s gamma of
                    Nothing -> Rescan t : go (p:ts)
                    Just (_,arg) -> Rescan (Important arg) : go (p:ts)
                go (t@(Important "##"):ts) = Rescan t : go ts
                go (t@(Important ('#':.s)) : ts) =
                  case lookup s gamma of
                    Nothing -> Rescan t : go ts
                    Just (_,arg) ->
                      Rescan (Important (stringify arg)) : go ts
                go (t@(Important s) : ts) =
                  case lookup s gamma of
                    Nothing -> Rescan t : go ts
                    Just (arg,_) -> arg ++ go ts
                go (t:ts) = Rescan t : go ts
        prepStringify [] = []
        prepStringify (Important "#" : ts) =
          case ts of
            (Important t : ts') -> Important (cons '#' t) : prepStringify ts'
            _ -> Important "#" : ts
        prepStringify (t:ts) = t : prepStringify ts

        body' = prepStringify . prepTOKENSplices $
                dropWhile (not . isImportant) body
        paste [] = []
        paste (Rescan (Important s) : Rescan (Important "##")
              : Rescan (Important t) : ts) =
          paste (Rescan (Important (trimSpaces s <> sdropWhile isSpace t)) : ts)
        paste (t:ts) = t : paste ts

-- * Pre-Processor Capabilities

modifyState :: (Monad m, HasHppState m) => (HppState -> HppState) -> m ()
modifyState f = getState >>= setState . f

-- | Run a Stream with a configuration for a new file.
streamNewFile :: (Monad m, HasHppState m)
              => FilePath -> [[TOKEN]] -> Parser m [TOKEN] ()
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

includeCandidates :: [FilePath] -> P.String -> Maybe [FilePath]
includeCandidates searchPath nm =
  case nm of
    '<':nm' -> Just $ sysSearch (init nm')
    '"':nm' -> let nm'' = init nm'
                in Just $ nm'' : sysSearch nm''
    _ -> Nothing
  where sysSearch f = map (</> f) searchPath

searchForInclude :: [FilePath] -> P.String -> IO (Maybe FilePath)
searchForInclude paths = maybe (return Nothing) aux . includeCandidates paths
  where aux [] = return Nothing
        aux (f:fs) = do exists <- doesFileExist f
                        if exists then return (Just f) else aux fs

searchForNextInclude :: [FilePath] -> P.String -> IO (Maybe FilePath)
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

data HppResult a = HppResult { hppFilesRead :: [FilePath]
                             , hppResult :: a }

-- | Interpret the IO components of the preprocessor. This
-- implementation relies on IO for the purpose of checking search
-- paths for included files.
runHpp :: forall m a src. (MonadIO m, HasHppState m)
       => (FilePath -> m src)
       -> (src -> m ())
       -> HppT src m a
       -> m (Either (FilePath,Error) (HppResult a))
runHpp source sink m = runHppT m >>= go []
  where go :: [FilePath]
           -> FreeF (HppF src) a (HppT src m a)
           -> m (Either (FilePath, Error) (HppResult a))
        go files (PureF x) = return $ Right (HppResult files x)
        go files (FreeF s) = case s of
          ReadFile ln file k ->
            (includePaths <$> use config)
            >>= liftIO . flip searchForInclude file
            >>= readAux (file:files) ln file k
          ReadNext ln file k ->
            (includePaths <$> use config)
            >>= liftIO . flip searchForNextInclude file
            >>= readAux (file:files) ln file k
          WriteOutput output k -> sink output >> runHppT k >>= go files

        readAux _files ln file _ Nothing =
          Left . (, IncludeDoesNotExist ln file) . curFileName <$> use config
        readAux files _ln _file k (Just file') =
          source file' >>= runHppT . k >>= go files
{-# SPECIALIZE runHpp ::
    (FilePath -> Parser (StateT HppState (ExceptT Error IO)) [TOKEN] [String])
 -> ([String] -> Parser (StateT HppState (ExceptT Error IO)) [TOKEN] ())
 -> HppT [String] (Parser (StateT HppState (ExceptT Error IO)) [TOKEN]) a
 -> Parser (StateT HppState (ExceptT Error IO)) [TOKEN] (Either (FilePath,Error) (HppResult a)) #-}

-- | Like ’runHpp’, but any @#include@ directives are skipped. These
-- ignored inclusions are tracked in the returned list of files, but
-- note that since extra source files are not opened, any files they
-- might wish to include are not discovered.
expandHpp :: forall m a src. (Monad m, HasHppState m, Monoid src)
          => (src -> m ())
          -> HppT src m a
          -> m (Either (FilePath,Error) (HppResult a))
expandHpp sink m = runHppT m >>= go []
  where go :: [FilePath]
           -> FreeF (HppF src) a (HppT src m a)
           -> m (Either (FilePath, Error) (HppResult a))
        go files (PureF x) = pure $ Right (HppResult files x)
        go files (FreeF s) = case s of
          ReadFile _ln file k -> runHppT (k mempty) >>= go (file:files)
          ReadNext _ln file k -> runHppT (k mempty) >>= go (file:files)
          WriteOutput output k -> sink output >> runHppT k >>= go files
{-# SPECIALIZE expandHpp ::
    ([String] -> Parser (StateT HppState
                                (ExceptT Error
                                         (State ([String] -> [String]))))
                        [TOKEN] ())
 -> HppT [String] (Parser (StateT HppState
                                  (ExceptT Error
                                           (State ([String] -> [String]))))
                          [TOKEN]) a
 -> Parser (StateT HppState
                   (ExceptT Error (State ([String] -> [String]))))
           [TOKEN] (Either (FilePath,Error) (HppResult a)) #-}

-- * Preprocessor

-- | Parse the definition of an object-like or function macro.
parseDefinition :: [TOKEN] -> Maybe (String, Macro)
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
      in Just (copy name, Object (map (fmap copy) rhs))
    _ -> Nothing

-- | Returns everything up to the next newline. The newline character
-- itself is consumed.
takeLine :: (Monad m, HasError m, HasHppState m) => Parser m [TOKEN] [TOKEN]
takeLine = (onElements $ do
              ln <- takingWhile (not . newLine)
              eat <- awaitJust "takeLine" -- Eat the newline character
              case eat of
                Other "\n" -> return ()
                wat -> error $ "Expected newline: "++show wat++" after "++show ln
              return ln)
           <* (lineNum %= (+1))

dropLine :: (Monad m, HasError m, HasHppState m) => Parser m [TOKEN] ()
dropLine = do onElements $ do
                droppingWhile (not . newLine)
                eat <- awaitJust "dropLine" -- Eat the newline character
                case eat of
                  Other "\n" -> return ()
                  wat -> error $ "Expected dropped newline: "++show wat
              lineNum %= (+1)

-- * State Zooming

expandLineP :: (Monad m, HasHppState m, HasEnv m, HasError m)
            => Parser m [TOKEN] [TOKEN]
expandLineP =
  do st <- getState
     let ln = hppLineNum st
         cfg = hppConfig st
     expandLine cfg ln

-- | @hppReadFile lineNumber fileName@ introduces an @#include
-- <fileName>@ as if it occurred at the given line number.
hppReadFile :: Monad m => Int -> FilePath -> HppT src m src
hppReadFile n file = HppT (pure (FreeF (ReadFile n file return)))

-- | @hppReadNext lineNumber fileName@ introduces an @#include_next
-- <fileName>@ as if it occurred at the given line number.
hppReadNext :: Monad m => Int -> FilePath -> HppT src m src
hppReadNext n file = HppT (pure (FreeF (ReadNext n file return)))

-- * Directive Processing

-- | Handle preprocessor directives (commands prefixed with an octothorpe).
directive :: forall m. (Monad m, HppCaps m)
          => HppT [String] (Parser m [TOKEN]) Bool
directive = lift (onElements (awaitJust "directive")) >>= aux
  where aux :: TOKEN -> HppT [String] (Parser m [TOKEN]) Bool
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
                         Important (toChars -> n):optFile ->
                           case readMaybe n of
                             Nothing -> use lineNum >>=
                                        throwError . flip BadLineArgument n
                             Just ln' -> do
                               unless (null optFile) $ do
                                 let fn = toChars . unquote . detokenize
                                        . dropWhile (not . isImportant)
                                        $ optFile
                                 config %= (\cfg -> cfg { curFileNameF = pure fn })
                               lineNum .= ln'
                               return True
                         _ -> use lineNum >>=
                              throwError
                              . flip BadLineArgument (toChars (detokenize toks))
          "ifdef" ->
            do toks <- lift (onElements droppingSpaces >> takeLine)
               ln <- use lineNum
               case takeWhile isImportant toks of
                 [Important t] ->
                   lookupMacro t >>= \case
                     Nothing ->
                       lift (dropBranchLine ln >>= replace . fst)
                     Just _ ->
                       lift (onInputSegment (takeBranchFun ln)) -- (takeBranch ln >>= precede)
                 _ -> throwError . UnknownCommand ln $
                      "ifdef "++ toChars (detokenize toks)
               return True
          "ifndef" ->
            do toks <- lift (onElements droppingSpaces >> takeLine)
               ln <- use lineNum
               case takeWhile isImportant toks of
                 [Important t] ->
                   lookupMacro t >>= \case
                      Nothing -> lift (onInputSegment (takeBranchFun ln)) -- takeBranch ln >>= precede)
                      Just _ -> lift (dropBranchLine ln >>= replace . fst)
                 _ -> throwError . UnknownCommand ln $
                      "ifndef "++ toChars (detokenize toks)
               return True
          "else" -> True <$ lift dropLine
          "if" -> True <$ ifAux
          "elif" -> True <$ ifAux
          "endif" -> True <$ lift dropLine
          "error" -> do toks <- lift (onElements droppingSpaces >> takeLine)
                        ln <- subtract 1 <$> use lineNum
                        curFile <- curFileName <$> use config
                        let tokStr = toChars (detokenize toks)
                        throwError $ UserError ln (tokStr++" ("++curFile++")")
          "warning" -> True <$ lift dropLine -- warnings not yet supported
          -- t -> do toks <- lift takeLine
          --         ln <- subtract 1 <$> use lineNum
          --         throwError $ UnknownCommand ln (detokenize (Important t:toks))
          _ -> return False -- Ignore unknown command
        aux _ = error "Impossible unimportant directive"
        includeAux :: (LineNum -> FilePath -> HppT src (Parser m [TOKEN]) [String])
                   -> HppT src (Parser m [TOKEN]) ()
        includeAux readFun =
          do fileName <- lift (toChars . detokenize . trimUnimportant . init
                               <$> expandLineP)
             ln <- use lineNum
             src <- prepareInput <*> readFun ln fileName
             lineNum .= ln+1
             lift (streamNewFile (unquote fileName) src)
        {- SPECIALIZE includeAux ::
            (LineNum -> FilePath -> HppT [String] (Parser (StateT HppState (ExceptT Error IO)) [TOKEN]) [String])
            -> HppT [String] (Parser (StateT HppState (ExceptT Error IO)) [TOKEN]) () #-}
        ifAux =
          do toks <- lift (onElements droppingSpaces >> takeLine)
             e <- use env
             ln <- use lineNum
             lineNum .= ln - 1 -- takeLine incremented the line count
             ex <- lift (lift (evalParse expandLineP [squashDefines e toks]))
             let res = evalExpr <$> parseExpr (map (fmap toChars) ex)
             lineNum .= ln
             if maybe False (/= 0) res
               then lift (onInputSegment (takeBranchFun ln)) -- (takeBranch ln >>= precede)
               else lift (dropBranchLine ln >>= replace . fst)

{-# SPECIALIZE directive ::
    HppT [String] (Parser (StateT HppState (ExceptT Error IO)) [TOKEN]) Bool #-}

-- | We want to expand macros in expressions that must be evaluated
-- for conditionals, but we want to take special care when dealing
-- with the meta @defined@ operator of the expression language that is
-- a predicate on the evaluation environment.
squashDefines :: Env -> [TOKEN] -> [TOKEN]
squashDefines _ [] = []
squashDefines env' (Important "defined" : ts) = go ts
  where go (t@(Other _) : ts') = t : go ts'
        go (t@(Important "(") : ts') = t : go ts'
        go (Important t : ts') =
          case lookupKey t env' of
            Nothing -> Important "0" : squashDefines env' ts'
            -- Just (_,env'') -> Important "1" : squashDefines env'' ts'
            Just _ -> Important "1" : squashDefines env' ts'
        go [] = []
squashDefines env' (t : ts) = t : squashDefines env' ts

getCmd :: [TOKEN] -> Maybe String
getCmd = aux . dropWhile notImportant
  where aux (Important "#" : ts) = case dropWhile notImportant ts of
                                     (Important cmd:_) -> Just cmd
                                     _ -> Nothing
        aux _ = Nothing

droppingSpaces ::(Monad m) => ParserT m src TOKEN ()
droppingSpaces = droppingWhile notImportant

dropBranchFun :: [[TOKEN]] -> (Int, [[TOKEN]])
dropBranchFun = go (1::Int) 0
  where go _ !n [] = (n,[])
        go !nesting !n (ln:lns) =
          case getCmd ln of
            Just cmd
              | cmd == "endif" -> if nesting == 1
                                  then (n, ln:lns)
                                  else go (nesting-1) (n+1) lns
              | cmd `elem` ["if","ifdef","ifndef"] ->
                go (nesting+1) (n+1) lns
              | cmd `elem` ["else","elif"] -> if nesting == 1
                                              then (n, ln : lns)
                                              else go nesting (n+1) lns
            _ -> go nesting (n+1) lns

-- | Take everything up to the end of this branch, drop all remaining
-- branches (if any).
takeBranchFun :: LineNum -> [[TOKEN]] -> [[TOKEN]]
takeBranchFun = go (1::Int)
  where go _ _ [] = [] -- error: unterminated conditional
        go 0 !n lns = yieldLineNum n : lns
        go !nesting !n (ln:lns) =
          case getCmd ln of
            Just cmd
              | cmd `elem` ["if","ifdef","ifndef"] ->
                ln : go (nesting+1) (n+1) lns
              | cmd == "endif" -> ln : go (nesting - 1) (n + 1) lns
              | nesting == 1 && cmd `elem` ["else","elif"] ->
                let (numSkipped, lns') = dropBranchFun lns
                in go 1 (n+1+numSkipped) lns'
            _ -> ln : go nesting (n+1) lns


yieldLineNum :: LineNum -> [TOKEN]
yieldLineNum !ln = [Important ("#line " <> fromString (show ln)), Other "\n"]

dropBranchLine :: (HasError m, Monad m)
               => LineNum -> Parser m [TOKEN] ([TOKEN], LineNum)
dropBranchLine !ln = do (el, numSkipped) <- dropBranch
                        let ln' = ln + numSkipped
                        return (yieldLineNum ln' ++ fromMaybe [] el, ln')

-- | Skip to the end of a conditional branch. Returns the 'Just' the
-- token that ends this branch if it is an @else@ or @elif@, or
-- 'Nothing' otherwise, and the number of lines skipped.
dropBranch :: (HasError m, Monad m) => Parser m [TOKEN] (Maybe [TOKEN], Int)
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
               => HppT [String] (Parser m [TOKEN]) (Maybe [TOKEN])
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
normalCPP :: [String] -> [[TOKEN]]
normalCPP = map ((++ [Other "\n"]) . tokenize)
          . lineSplicing
          -- . map dropLineComments
          . removeMultilineComments 1
          . map (dropOneLineBlockComments . trigraphReplacement)
{-# INLINABLE normalCPP #-}

-- | For Haskell we do not want trigraph replacement.
haskellCPP :: [String] -> [[TOKEN]]
haskellCPP = map ((++[Other "\n"]) . tokenize)
           . lineSplicing
           . commentRemoval
{-# INLINABLE haskellCPP #-}

-- | If we don't have a predefined processor, we build one based on a
-- 'Config' value.
genericConfig :: Config -> [String] -> [[TOKEN]]
genericConfig cfg = map ((++ [Other "\n"]) . tokenize)
                  . (if spliceLongLines cfg then lineSplicing else id)
                  . (if eraseCComments cfg then commentRemoval else id)
                  . (if replaceTrigraphs cfg then map trigraphReplacement else id)

-- * Front End

prepareInput :: (Monad m, HppCaps m) => m ([String] -> [[TOKEN]])
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
           => [String] -> HppT [String] (Parser m [TOKEN]) ()
preprocess src =
  do cfg <- getL config <$> getState
     prep <- prepareInput
     let prepOutput = if inhibitLinemarkers cfg then aux else pure
     lift (precede (prep src))
     parseStreamHpp (fmap (prepOutput . detokenize) <$> macroExpansion)
  where aux xs | sIsPrefixOf "#line" xs = []
               | otherwise = [xs]

-- Note: `preprocess` is the workhorse of the library. We run the
-- value it returns in `hppIO'` by interleaving interpretation of
-- `HppT` with binds of types providing the `HppCaps`
-- capabilities. When making things concrete, we specialize to
-- `ExceptT`, `StateT`, and `Parser` (note that `Parser` is actually
-- just another `StateT`).

-- | A concreate choice of types to satisfy `HppCaps` as required by
-- `preprocess`.
dischargeHppCaps :: Monad m
                 => Config -> Env
                 -> Parser (StateT HppState (ExceptT Error m))
                           i
                           (Either (a, Error) b)
                 -> m (Either Error b)
dischargeHppCaps cfg env' m =
  runExceptT
    (evalStateT
       (evalParse (m >>= either (throwError . snd) return) [])
       initialState)
  where initialState = setL env env' $ emptyHppState cfg

-- | General hpp runner against input source file lines; can return an
-- 'Error' value if something goes wrong.
hppIOSink' :: Config -> Env -> ([String] -> IO ()) -> [String]
           -> IO (Either Error [FilePath])
hppIOSink' cfg env' snk src =
  fmap (fmap hppFilesRead)
  . dischargeHppCaps cfg env' $
  runHpp (liftIO . readLines) (liftIO . snk) (preprocess src)

-- | General hpp runner against input source file lines. Output lines
-- are fed to the caller-supplied sink function. Any errors
-- encountered are thrown with 'error'.
hppIOSink :: Config -> Env -> ([String] -> IO ()) -> [String] -> IO [FilePath]
hppIOSink cfg env' snk = hppIOSink' cfg env' snk >=> either throwIO return

-- | hpp runner that returns output lines.
hppIO :: Config -> Env ->  FilePath -> [String]
      -> IO (Either Error ([FilePath], [String]))
hppIO cfg env' fileName src = do
  r <- newIORef id
  let snk xs = modifyIORef r (. (xs++))
  hppIOSink' (cfg {curFileNameF = pure fileName}) env' snk src >>= \case
    Left e -> return (Left e)
    Right files -> Right . (files,) . ($ []) <$> readIORef r
