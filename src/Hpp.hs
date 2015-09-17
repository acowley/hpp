{-# LANGUAGE BangPatterns, ConstraintKinds, LambdaCase, RankNTypes,
             ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
-- | Front-end interface to the pre-processor.
module Hpp (parseDefinition, preprocess, yield, before, source,
            hppReadFile, hppIO, hppRegisterCleanup,
            streamHpp, sinkToFile, sinkToStdOut, (~>), HppCaps) where
import Control.Applicative (empty)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Functor.Constant
import Data.Functor.Identity
import Data.List (isPrefixOf, uncons)
import Data.Monoid ((<>))
import Data.Void (Void)
import Hpp.Config (Config, curFileNameF, curFileName, includePaths,
                   eraseCComments, spliceLongLines, inhibitLinemarkers)
import Hpp.Env (deleteKey, emptyEnv, insertPair, lookupKey)
import Hpp.Expansion (expandLine)
import Hpp.Expr (evalExpr, parseExpr)
import Hpp.Parser (Parser(..), zoomParseChunks, replace, awaitP, awaitJust,
                   droppingWhile, liftP, parse, onParserSource, precede,
                   takingWhile)
import Hpp.StreamIO (sinkToFile, sinkToStdOut, sourceFile)
import Hpp.Streamer (Streamer(..), Chunky(..), metamorph, done, yields, mapping,
                     (~>), Source, before, liftS, source, encase, StreamStep(..),
                     yield, filtering, run)
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

-- | Yields full lines of input.
-- lineStream :: Monad m => Streamer m String String ()
-- lineStream = go id
--   where go acc = awaitMaybe (yield (acc [])) (yieldLines acc)
--         yieldLines acc s = case break (== '\n') s of
--                              (h,t) -> case t of
--                                         [] -> go (acc . (h++))
--                                         ['\n'] -> encase $ Yield (acc h) (go id)
--                                         (_:t') -> encase $
--                                                   Yield (acc h)
--                                                         (yieldLines id t')

-- | If a line ends with a backslash, it is prepended to the following
-- the line.
lineSplicing :: Monad m => Streamer m String String ()
lineSplicing = metamorph (Chunky go)
  where go [] = yields [] (done $ Chunky go)
        go ln = case last ln of
                  '\\' -> done . Chunky $ go . (init ln ++)
                  _ -> yields ln (done $ Chunky go)
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

dropLineComments :: String -> String
dropLineComments = fst . breakOn "//"

removeMultilineComments :: Monad m => Int -> Streamer m String String ()
removeMultilineComments !lineStart = metamorph (Chunky $ goStart lineStart)
  where goStart !curLine ln =
          case breakBlockCommentStart ln of
            Nothing -> yields ln (done . Chunky $ goStart (curLine+1))
            Just (pre,_) -> done . Chunky $ goEnd (curLine+1) pre
        goEnd !curLine pre ln =
          case breakBlockCommentEnd ln of
            Nothing -> done (Chunky $ goEnd (curLine+1) pre)
            Just pos
              | all isSpace (pre++pos) ->
                yields ("#line "++show (curLine+1))
                       (done . Chunky . goStart $ curLine + 1)
              | otherwise -> yields (pre++pos) $
                             yields ("#line "++show (curLine+1))
                                    (done . Chunky $ goStart (curLine+1))

              -- FIXME: The #line command interferes here, but the
              -- strategy above fails when multi-line comments end and
              -- begin on the same line.

              -- yields ("#line "++show curLine)
              --        (goStart (curLine+1) (pre++pos))
{-# INLINE removeMultilineComments #-}

commentRemoval :: Monad m => Streamer m String String ()
commentRemoval =  mapping dropOneLineBlockComments
               ~> removeMultilineComments 1
               ~> mapping dropLineComments

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

config :: Lens HppState Config
config f (HppState cfg ln cln e) = (\cfg' -> HppState cfg' ln cln e) <$> f cfg

lineNum :: Lens HppState LineNum
lineNum f (HppState cfg ln cln e) = (\ln' -> HppState cfg ln' cln e) <$> f ln

cleanups :: Lens HppState [Cleanup]
cleanups f (HppState cfg ln cln e) = (\cln' -> HppState cfg ln cln' e) <$> f cln

env :: Lens HppState Env
env f (HppState cfg ln cln e) = (\e' -> HppState cfg ln cln e') <$> f e

modifyState :: (Monad m, HasHppState m) => (HppState -> HppState) -> m ()
modifyState f = getState >>= setState . f

-- | Run a Stream with a configuration for a new file.
streamNewFile :: (Monad m, HasHppState m)
              => FilePath
              -> Source m o ()
              -> Source m o ()
streamNewFile fp s = Streamer $
  do (oldCfg,oldLine) <- do st <- getState
                            let cfg = hppConfig st
                                cfg' = cfg { curFileNameF = pure fp }
                                ln = hppLineNum st
                            setState (st {hppConfig = cfg', hppLineNum = 1})
                            return (cfg, ln)
     runStream $
       before s (liftS $ modifyState (setL lineNum oldLine . setL config oldCfg))

-- * Running an Hpp Action

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

runHpp :: forall m a. MonadIO m
       => (FilePath -> HppStream m (InputStream (HppStream m)))
       -> HppState
       -> HppStream m a
       -> m (Either (FilePath,Error) (a, HppState))
runHpp readInput !st (HppStream m) = runHppT m >>= go
  where go :: FreeF (HppF (Source (HppStream m) String ()))
                    a
                    (HppT (InputStream (HppStream m)) m a)
           -> m (Either (FilePath, Error) (a, HppState))
        go (PureF x) = return $ Right (x,st)
        go (FreeF s) = case s of
          ReadFile ln file k ->
            liftIO (searchForInclude (includePaths cfg) file)
            >>= readAux ln file (HppStream . k)
          ReadNext ln file k ->
            liftIO (searchForNextInclude (includePaths cfg) file)
            >>= readAux ln file (HppStream . k)
          GetState k -> runHpp readInput st (HppStream $ k st)
          SetState st' k -> runHpp readInput st' (HppStream k)
          ThrowError e -> return $ Left (curFile, e)
        curFile = curFileName cfg
        readAux ln file _ Nothing =
          return $ Left (curFile, IncludeDoesNotExist ln file)
        readAux _ln _file k (Just file') = runHpp readInput st (readInput file' >>= k)
        cfg = hppConfig st

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
takeLine :: (Monad m, HasError m, HasHppState m) => Parser m Token [Token]
takeLine = do ln <- takingWhile (not . newLine)
              eat <- awaitJust "takeLine" -- Eat the newline character
              case eat of
                Other "\n" -> return ()
                wat -> error $ "Expected newline: "++show wat++" after "++show ln
              ln <$ incLine

dropLine :: (Monad m, HasError m, HasHppState m) => Parser m Token ()
dropLine = do droppingWhile (not . newLine)
              eat <- awaitJust "dropLine" -- Eat the newline character
              case eat of
                Other "\n" -> return ()
                wat -> error $ "Expected dropped newline: "++show wat
              incLine

-- * Nano-lens

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

setL :: Lens s a -> a -> s -> s
setL l x = runIdentity . l (const $ Identity x)

getL :: Lens s a -> s -> a
getL l = getConstant . l Constant

over :: Lens s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

-- * State Lenses

emptyHppState :: Config -> HppState
emptyHppState cfg = HppState cfg 1 [] emptyEnv

getL' :: (Monad m, HasHppState m) => Lens HppState a -> Parser m i a
getL' l = liftP (getL l <$> getState)

setL' :: (Monad m, HasHppState m) => Lens HppState a -> a -> m ()
setL' l x = getState >>= setState . setL l x

over' :: (Monad m, HasHppState m)
      => Lens HppState a -> (a -> a) -> Parser m i ()
over' l f = liftP $ do st <- getState
                       setState $ over l f st

-- * State Zooming

expandLineP :: (HppCaps m, Monad m) => Parser m Token [Token]
expandLineP =
  do st <- liftP getState
     let ln = hppLineNum st
         cfg = hppConfig st
     expandLine cfg ln

lookupEnv :: (Monad m, HasHppState m) => String -> Parser m i (Maybe Macro)
lookupEnv s = liftP $
              do st <- getState
                 case lookupKey s (getL env st) of
                   Nothing -> return Nothing
                   Just (m,env') -> Just m <$ setState (setL env env' st)

-- | Register a 'Cleanup' in a threaded 'HppState'.
hppRegisterCleanup :: (HasHppState m, Monad m) => Cleanup -> m ()
hppRegisterCleanup c = modifyState $ over cleanups (c:)

type InputStream m = Source m String ()

class HasHppFileIO m where
  -- | Read a file as an 'Hpp' action
  hppReadFile :: Int -> FilePath -> m (InputStream m)

  -- | Read a file available on the search path after the path
  -- containing the current file.
  hppReadNext :: Int -> FilePath -> m (InputStream m)

-- | Lets us fix 'HppT''s input type to a 'Source' whose context is
-- the type we are defining.
newtype HppStream m a = HppStream ( HppT (InputStream (HppStream m)) m a )
  deriving (Functor, Applicative, Monad, MonadIO, HasHppState, HasError, HasEnv)

instance Monad m => HasHppFileIO (HppStream m) where
  hppReadFile n file = HppStream . HppT . return . FreeF $ ReadFile n file return
  hppReadNext n file = HppStream . HppT . return . FreeF $ ReadNext n file return

incLine :: (Monad m, HasHppState m) => Parser m i ()
incLine = over' lineNum (+1)

-- * Directive Processing

-- | Handle preprocessor directives (commands prefixed with an octothorpe).
directive :: forall m. (Monad m, HppCaps m) => Parser m [Token] ()
directive = zoomParseChunks (awaitJust "directive" >>= aux) >>=
            either onParserSource (maybe (return ()) precede)
  where aux :: Token -> Parser m Token (Either (Streamer m [Token] [Token] ())
                                               (Maybe (Source m [Token] ())))
        aux (Important cmd) = case cmd of
          "pragma" -> Right Nothing <$ dropLine -- Ignored
          "define" -> fmap parseDefinition takeLine >>= \case
                        Nothing -> getL' lineNum
                                   >>= throwError . BadMacroDefinition
                        Just def -> Right Nothing <$ over' env (insertPair def)
          "undef" -> do droppingWhile (not . isImportant)
                        Important name <- awaitJust "undef"
                        dropLine
                        Right Nothing <$ over' env (deleteKey name)
          "include" -> fmap (Right . Just) $ includeAux hppReadFile
          "include_next" -> fmap (Right . Just) $ includeAux hppReadNext
          "line" -> do toks <- droppingSpaces >> fmap init expandLineP
                       case toks of
                         Important n:optFile ->
                           case readMaybe n of
                             Nothing -> getL' lineNum >>=
                                        throwError . flip BadLineArgument n
                             Just ln' -> do
                               unless (null optFile) $ do
                                 let fn = unquote . detokenize 
                                        . dropWhile (not . isImportant) $ optFile
                                 over' config $ \cfg ->
                                   cfg { curFileNameF = pure fn }
                               Right Nothing <$ setL' lineNum ln'
                         _ -> getL' lineNum >>=
                              throwError . flip BadLineArgument (detokenize toks)
          "ifdef" -> do ln <- getL' lineNum
                        toks <- droppingSpaces >> takeLine
                        case takeWhile isImportant toks of
                          [Important t] ->
                            lookupEnv t >>= \case
                              Nothing -> return . Left $ dropBranchLine (ln+1)
                              Just _ -> return . Left $ takeBranch (ln+1)
                          _ -> throwError . UnknownCommand ln $
                               "ifdef "++detokenize toks
          "ifndef" -> do toks <- droppingSpaces >> takeLine
                         ln <- getL' lineNum
                         case takeWhile isImportant toks of
                           [Important t] -> lookupEnv t >>= \case
                                               Nothing -> return . Left $
                                                          takeBranch (ln+1)
                                               Just _ -> return . Left $
                                                         dropBranchLine (ln+1)
                           _ -> throwError . UnknownCommand ln $
                                "ifndef "++detokenize toks
          "else" -> Right Nothing <$ dropLine
          "if" -> ifAux
          "elif" -> ifAux
          "endif" -> Right Nothing <$ dropLine
          "error" -> do ln <- getL' lineNum
                        curFile <- liftP $ curFileName . hppConfig <$> getState
                        toks <- droppingSpaces >> takeLine
                        throwError $
                          UserError ln (detokenize toks++" ("++curFile++")")
          "warning" -> Right Nothing <$ dropLine -- warnings not yet supported
          t -> do ln <- getL' lineNum
                  toks <- takeLine
                  throwError $ UnknownCommand ln (detokenize (Important t:toks))
        aux _ = error "Impossible unimportant directive"
        includeAux readFun =
          do fileName <- init <$> expandLineP
             ln <- getL' lineNum
             let fileName' = detokenize $ trimUnimportant fileName
             src <- liftP $ readFun ln fileName'
             setL' lineNum (ln+1)
             return $ streamNewFile (unquote fileName') (src ~> prepareInput)
        ifAux :: Parser m Token (Either (Streamer m [Token] [Token] ()) b)
        ifAux = do droppingSpaces
                   toks <- takeLine
                   e <- getL' env
                   ln <- getL' lineNum
                   setL' lineNum (ln - 1) -- takeLine incremented the line count
                   ex <- liftP . parse expandLineP $
                         source (squashDefines e toks)
                   let res = evalExpr <$> parseExpr ex
                   setL' lineNum ln
                   if maybe False (/= 0) res
                     then return . Left $ takeBranch ln
                     else return . Left $ dropBranchLine ln

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

droppingSpaces :: Monad m => Parser m Token ()
droppingSpaces = droppingWhile notImportant

-- | Take an entire conditional expression (e.g. @#if
-- ... #endif@). All the lines of the taken branch are returned, in
-- reverse order.
takeConditional :: Monad m
                => LineNum 
                -> (Int -> Streamer m [Token] [Token] r)
                -> Streamer m [Token] [Token] r
takeConditional !n0 k = go (1::Int) n0
  where go 0 !n = k n
        go nesting !n = encase $ Await aux empty
          where aux ln = case getCmd ln of
                           Just cmd
                             | cmd == "endif" ->
                               encase $ Yield ln (go (nesting-1) (n+1))
                             | cmd `elem` ["if","ifdef","ifndef"] ->
                               encase $ Yield ln (go (nesting+1) (n+1))
                           _ -> encase $ Yield ln (go nesting (n+1))

-- | Take everything up to the end of this branch, drop all remaining
-- branches (if any).
takeBranch :: Monad m => LineNum -> Streamer m [Token] [Token] ()
takeBranch = go
  where go !n = encase $ Await aux empty
          where aux ln = case getCmd ln of
                           Just cmd
                             | cmd `elem` ["if","ifdef","ifndef"] ->
                               encase $ Yield ln (takeConditional (n+1) go)
                             | cmd == "endif" -> yieldLineNum n (done ())
                             | cmd `elem` ["else","elif"] ->
                               dropAllBranches $ \numSkipped ->
                                 yieldLineNum (n+1+numSkipped) empty
                           _ -> encase $ Yield ln (go (n+1))

yieldLineNum :: Monad m => LineNum -> Streamer m i [Token] r -> Streamer m i [Token] r
yieldLineNum !ln k = encase $ Yield [Important ("#line "++show ln), Other "\n"] k

dropAllBranches :: Monad m
                => (Int -> Streamer m [Token] [Token] r)
                -> Streamer m [Token] [Token] r
dropAllBranches k = dropBranch (aux 0)
  where aux !acc Nothing !numDropped = k (acc+numDropped)
        aux !acc _ !numDropped = dropBranch (aux (acc+numDropped))

dropBranchLine :: Monad m => LineNum -> Streamer m [Token] [Token] ()
dropBranchLine !ln = dropBranch $ \el numSkipped ->
                       yieldLineNum (ln + numSkipped) (traverse_ yield el)

-- | Skip to the end of a conditional branch. Returns the 'Just' the
-- token that ends this branch if it is an @else@ or @elif@, or
-- 'Nothing' otherwise, and the number of lines skipped.
dropBranch :: Monad m
           => (Maybe [Token] -> Int -> Streamer m [Token] [Token] r)
           -> Streamer m [Token] [Token] r
dropBranch k = go (1::Int) 0
  where go !nesting !n = encase . flip Await empty $ \ln ->
          case getCmd ln of
            Just cmd
              | cmd == "endif" -> if nesting == 1
                                  then k Nothing (n+1)
                                  else go (nesting-1) (n+1)
              | cmd `elem` ["if","ifdef","ifndef"] ->
                go (nesting+1) (n+1)
              | cmd `elem` ["else", "elif"] -> if nesting == 1
                                               then k (Just ln) (n+1)
                                               else go nesting (n+1)
            _ -> go nesting (n+1)

-- | Expands an input line producing a stream of output lines.
macroExpansion :: (HppCaps m, Monad m) => Parser m [Token] (Maybe [Token])
macroExpansion = do
  awaitP >>= \case
    Nothing -> return Nothing
    Just ln ->
      case dropWhile notImportant ln of
        [] -> incLine >> return (Just ln)
        Important "#":rst -> do replace (dropWhile notImportant rst)
                                directive
                                macroExpansion
        _ -> do replace ln
                zoomParseChunks (Just <$> expandLineP) <* incLine

-- | The dynamic capabilities offered by HPP
type HppCaps t = (HasError t, HasHppState t, HasHppFileIO t, HasEnv t)

parseStreamHpp :: Monad m
               => Parser m i (Maybe o)
               -> Source m i ()
               -> Source m o ()
parseStreamHpp (Parser m) = go
  where go src = Streamer $
                 do (o,src') <- runStateT m src
                    case o of
                      Nothing -> return $ Done (Just ())
                      Just o' -> return $ Yield o' (go src')

-- * HPP configurations

-- | Standard CPP settings for processing C files.
normalCPP :: Monad m => Streamer m String [Token] ()
normalCPP = mapping trigraphReplacement
          ~> mapping dropOneLineBlockComments
          ~> removeMultilineComments 1
          ~> mapping dropLineComments
          ~> lineSplicing
          ~> mapping ((++[Other "\n"]) . tokenize)

-- | For Haskell we often want to ignore C-style comments and long
-- line splicing.
haskellCPP :: Monad m => Streamer m String [Token] ()
haskellCPP = mapping ((++[Other "\n"]) . tokenize)

-- | If we don't have a predefined processor, we build one based on a
-- 'Config' value.
genericConfig :: Monad m => Config -> Streamer m String [Token] ()
genericConfig cfg = mapping trigraphReplacement
                  ~> (if eraseCComments cfg then commentRemoval else idS)
                  ~> (if spliceLongLines cfg then lineSplicing else idS)
                  ~> mapping ((++[Other "\n"]) . tokenize)
  where idS :: Monad m => Streamer m i i r
        idS = encase $ Await (encase . flip Yield idS) empty

-- * Front End

prepareInput :: (Monad m, HppCaps m)
             => Streamer m String [Token] ()
prepareInput = Streamer $
  do cfg <- getL config <$> getState
     case () of
       _ | eraseCComments cfg && spliceLongLines cfg 
           && not (inhibitLinemarkers cfg) -> runStream normalCPP
       _ | not (eraseCComments cfg || spliceLongLines cfg) ->
           runStream haskellCPP
       _ | otherwise -> runStream $ genericConfig cfg
{-# SPECIALIZE prepareInput :: Streamer (HppStream IO) String [Token] () #-}

-- | Run a stream of lines through the preprocessor.
preprocess :: (Monad m, HppCaps m)
           => Source m String ()
           -> Source m String ()
preprocess src = Streamer $
  do cfg <- getL config <$> getState
     runStream $ if inhibitLinemarkers cfg
                 then go ~> filtering (not . isPrefixOf "#line")
                 else go
  where {-# INLINE go #-}
        go = parseStreamHpp macroExpansion (src ~> prepareInput)
           ~> mapping detokenize

-- | Preprocess the given file producing line by line output.
streamHpp :: (Monad m, HasHppFileIO m)
          => FilePath -> Source m String ()
streamHpp f = Streamer $
              hppReadFile 0 ('"':f++"\"") >>= runStream

-- | Monad morphism between Hpp and IO.
hppIO :: (MonadIO m) => Config -> Env
      -> Streamer (HppStream m) Void b r
      -> Streamer (HppStream m) b Void ()
      -> m (Maybe ())
hppIO cfg env' s snk = runHpp (sourceFile hppRegisterCleanup)
                              initialState
                              (run (s ~> snk))
                       >>= either (error .show) cleanup
  where cleanup (e, s') = e <$ (liftIO $ mapM_ runCleanup (getL cleanups s'))
        initialState = setL env env' $ emptyHppState cfg
