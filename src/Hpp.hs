{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
-- | Front-end interface to the pre-processor.
module Hpp (parseDefinition, preprocess,
            liftHpp, errorHpp, getConfig, setConfig, hppReadFile,
            runErrHppIO) where
import Control.Arrow (second)
import Control.Exception (catch, IOException)
import Control.Monad ((<=<))
import Data.Char (isSpace)
import Data.Functor.Identity
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)

import Hpp.Config
import Hpp.Env
import Hpp.Expansion
import Hpp.Expr
import Hpp.String
import Hpp.Tokens
import Hpp.Types

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

-- | Count the prefix @?@ characters as we go.
data TrigraphPrefix = TP0 | TP1 | TP2 deriving (Enum, Show)

trigraphReplacement :: String -> String
trigraphReplacement = go TP0
  where go :: TrigraphPrefix -> String -> String
        go n [] = replicate (fromEnum n) '?'
        go TP2 ('?':xs) = '?' : go TP2 xs
        go TP2 (x:xs) = case lookup x trigraphs of
                          Just x' -> x' : go TP0 xs
                          Nothing -> "??" ++ x : go TP0 xs
        go i ('?':xs) = go (succ i) xs
        go TP0 (x:xs) = x : go TP0 xs
        go TP1 (x:xs) = '?' : x : go TP0 xs

-- * Line Splicing

lineSplicing :: [String] -> [String]
lineSplicing [] = []
lineSplicing [x] = [x]
lineSplicing ([]:t) = [] : lineSplicing t
lineSplicing (x:t@(y:xs))
  | last x == '\\' = lineSplicing ((init x++y) : xs)
  | otherwise = x : lineSplicing t

-- FIXME: This doesn't work! we can also end or start a line with an
-- operator! A #pragma line shouldn't be spliced. But note also that
-- treating a close parenthesis is risky.

-- | Applications can run across multiple lines. We join those lines
-- to simplify subsequent parsing. We simply look for trailing or
-- leading commas to determine which lines to splice.
spliceApplications :: [String] -> [String]
spliceApplications = go Nothing
  where go :: Maybe String -> [String] -> [String]
        go prev [] = maybe [] (:[]) prev
        go prev (l:ls)
          | headIs '#' l = let p = maybe [] (:[]) prev
                           in p ++ l : go Nothing ls
        go (Just prev) (l:ls)
          | headOp opStarts l = go Nothing ((prev++l) : ls)
          | otherwise = prev : go Nothing (l:ls)
        go Nothing (l1:l2:ls)
          | headOp opEnds $ reverse l1 = go Nothing ((l1++l2):ls)
        go Nothing (l:ls) = go (Just l) ls
 
        opEnds = [',','+','-','*','/','(','=','%','&','|','^']
        opStarts = [',','+','-','*','/','(',')','=','%','&','|','^']
        headOp ops xs = case dropWhile isSpace xs of
                          c:_ -> c `elem` ops
                          _ -> False
        headIs x xs = case dropWhile isSpace xs of
                        (y:_) -> y == x
                        _ -> False

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
        go ('*':'/':t) = ' ' : dropOneLineBlockComments t
        go (_:t) = go t
dropOneLineBlockComments (c:cs) = c : dropOneLineBlockComments cs

dropLineComments :: String -> String
dropLineComments [] = []
dropLineComments ('/':'/':_) = []
dropLineComments (c:cs) = c : dropLineComments cs

removeMultilineComments :: Int -> [String] -> [String]
removeMultilineComments _ [] = []
removeMultilineComments lineNum (l:ls) =
  case breakBlockCommentStart l of
    Nothing -> l : removeMultilineComments (lineNum+1) ls
    Just (pre,_) ->
      case go 0 ls of
        (numSkipped, []) -> pre : replicate (lineNum+numSkipped) []
        (numSkipped, (l':ls')) -> 
          let lineNum' = lineNum + numSkipped
          in (pre ++ l') : ("#line " ++ show (lineNum'+1))
             : removeMultilineComments lineNum' ls'
  where go :: Int -> [String] -> (Int, [String])
        go numSkipped [] = (numSkipped, [])
        go numSkipped (l':ls') =
          case breakBlockCommentEnd l' of
            Nothing -> go (numSkipped + 1) ls'
            Just rst -> (numSkipped+1, rst : ls')

commentRemoval :: [String] -> [String]
commentRemoval = map dropLineComments
               . removeMultilineComments 1
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
                    Nothing -> Rescan t : Rescan p : go ts
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

-- | Raise an error condition.
errorHpp :: Error -> ErrHpp a
errorHpp e = do f <- liftHpp . fmap curFileName $ getConfig
                ErrHpp . pure $ Left (f, e)

-- | Lift an 'Either' into an 'ErrHpp'
liftEither :: Either Error a -> ErrHpp a
liftEither = either errorHpp pure

-- | Lift an 'Hpp' into an 'ErrHpp'
liftHpp :: Hpp a -> ErrHpp a
liftHpp = ErrHpp . fmap Right

-- | Read a file as an 'Hpp' action
hppReadFile :: Int -> FilePath -> Hpp String
hppReadFile n file = ReadFile n file return

-- | Read a file available on the search path after the path
-- containing the current file.
hppReadNext :: Int -> FilePath -> Hpp String
hppReadNext n file = ReadNext n file return

-- | Obtain the current 'Config'
getConfig :: Hpp Config
getConfig = GetConfig return

-- | Set the current 'Config'
setConfig :: Config -> Hpp ()
setConfig = flip SetConfig (return ())

-- | Run an action with a substitute 'Config'
withConfig :: Config -> ErrHpp a -> ErrHpp a
withConfig cfg m = do oldCfg <- liftHpp getConfig
                      liftHpp $ setConfig cfg
                      r <- m
                      liftHpp $ setConfig oldCfg
                      return r

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

runHpp :: Config -> Hpp a -> IO (Either (FilePath,Error) a)
runHpp cfg = go
  where go (Pure x) = return (Right x)
        go (ReadFile ln file k) = searchForInclude (includePaths cfg) file
                                  >>= readAux ln file k
        go (ReadNext ln file k) = searchForNextInclude (includePaths cfg) file
                                  >>= readAux ln file k
        go (GetConfig k) = go (k cfg)
        go (SetConfig cfg' k) = runHpp cfg' k
        curFile = curFileName cfg
        readAux ln file _ Nothing =
          return $ Left (curFile, IncludeDoesNotExist ln file)
        readAux ln file k (Just file') =
          catch (Just <$> readFile file')
                (\(_::IOException) -> return Nothing)
          >>= maybe (return . Left $ (curFile, FailedInclude ln file))
                    (go . k)

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

-- | Trim 'Other' 'Token's from both ends of a list of 'Token's.
trimUnimportant :: [Token] -> [Token]
trimUnimportant = aux id . dropWhile (not .isImportant)
  where aux _ [] = []
        aux acc (t@(Important _) : ts) = acc (t : aux id ts)
        aux acc (t@(Other _) : ts) = aux (acc . (t:)) ts

-- | Handle preprocessor directives (commands prefixed with an octothorpe).
directive :: Config -> LineNum -> String
          -> ErrHpp ((LineNum -> [String] -> Config -> Env
                       -> [String] -> ErrHpp r)
                     -> Env -> [String] -> ErrHpp r)
directive cfg ln s =
  case importants toks of
    "pragma":_ -> pure $ \k -> k ln' [] cfg -- Pragmas are ignored
    "define":_ -> case parseDefinition . tail $
                       dropWhile (/= Important "define") toks of
                    Nothing -> errorHpp $ BadMacroDefinition ln
                    Just def -> pure $ \k -> k ln' [] cfg . (def :)
    ["undef",name] -> pure $ \k -> k ln' [] cfg . deleteKey name

    "include":_ -> includeAux hppReadFile . trimUnimportant . tail
                   $ dropWhile (/= Important "include") toks

    "include_next":_ -> includeAux hppReadNext . trimUnimportant . tail
                        $ dropWhile (/= Important "include_next") toks
            
    ("line":_) ->
      pure $ \k env lns ->
        do (env',rst) <- liftEither . expandLine cfg ln env . tail
                         $ dropWhile (/= Important "line") toks
           case dropWhile (not . isImportant) rst of
             Important n:optFile ->
               case readMaybe n of
                 Nothing -> errorHpp $ BadLineArgument ln n
                 Just ln'' ->
                   let cfg' = case optFile of
                                [] -> cfg
                                _ -> let f = unquote
                                           . detokenize
                                           . dropWhile (not . isImportant)
                                           . tail -- line number token
                                           . dropWhile (not . isImportant)
                                           $ rst
                                     in cfg { curFileNameF = pure f }
                   in k ln'' [] cfg' env' lns
             _ -> errorHpp $ BadLineArgument ln s
    ["ifdef", x] -> pure $ \k env lns ->
                    case lookupKey x env of
                      Nothing -> do lns' <- liftEither $ dropBranch lns
                                    k ln [] cfg env lns'
                      Just _ -> liftEither (takeBranch lns) >>= k ln [] cfg env
    ["ifndef", x] -> pure $ \k env lns ->
                     case lookupKey x env of
                       Nothing -> liftEither (takeBranch lns)
                                  >>= k ln [] cfg env
                       Just (_,env') -> do lns' <- liftEither $ dropBranch lns
                                           k ln [] cfg env' lns'
    ["else"] -> pure $ \k env lns ->
                liftEither (takeBranch lns) >>= k ln [] cfg env

    "if":_ -> pure $ ifAux "if"
    "elif":_ -> pure $ ifAux "elif"
    ["endif"] -> pure $ \k env -> k ln [] cfg env
    "error":_ -> errorHpp . UserError ln . detokenize
                 . dropWhile (not . isImportant) . tail
                 $ dropWhile (/= Important "error") toks
    "warning":_ -> pure $ \k env -> k ln' [] cfg env -- FIXME
    _ -> errorHpp $ UnknownCommand ln s
  where toks = tokenize s
        ln' = ln + 1
        toksAfterCommand cmd = tail $ dropWhile (/= Important cmd) toks

        ifAux c k env lns =
          do (env',ex) <- liftEither . expandLine cfg ln env . squashDefines env
                          $ toksAfterCommand c
             let res = evalExpr <$> parseExpr ex
                 -- res' = (if curFileName cfg == "test"
                 --          then trace ("Eval "++show ex++" => "++show res)
                 --          else id) res
             if maybe False (/= 0) res
               then either errorHpp (k ln [] cfg env') (takeBranch lns)
               else either errorHpp (k ln [] cfg env') (dropBranch lns)
        includeAux readFun fileToks = pure $ \k env lns ->
         do (env', fileName) <- liftEither $ expandLine cfg ln env fileToks
            let fileName' = detokenize $ trimUnimportant fileName
                cfg' = -- trace ("Including "++show fileName') $
                       cfg { curFileNameF = pure $ unquote fileName' }
            (env'',inc) <- liftHpp (readFun ln fileName')
                           >>= withConfig cfg' . preprocess env'
            k ln' [inc] cfg env'' lns

-- | We want to expand macros in expressions that must be evaluated
-- for condtionalals, but we want to take special care when dealing
-- with the meta @defined@ operator of the expression language that is
-- a predicate on the evaluation environment.
squashDefines :: Env -> [Token] -> [Token]
squashDefines _ [] = []
squashDefines env (Important "defined" : ts) = go ts
  where go (t@(Other _) : ts') = t : go ts'
        go (Important "(" : ts') = Important "(" : go ts'
        go (Important t : ts') =
          case lookupKey t env of
            Nothing -> Important "0" : squashDefines env ts'
            Just (_,env') -> Important "1" : squashDefines env' ts'
        go [] = []
squashDefines env (t : ts) = t : squashDefines env ts

getCmd :: String -> Maybe (String,[Token])
getCmd = aux . dropWhile (not . isImportant) . tokenize
  where aux (Important "#" : ts) =
            let (Important cmd:toks) = dropWhile (not . isImportant) ts
            in Just (cmd, toks)
        aux _ = Nothing

-- | Feed an entire conditional block (bounded by @#if@/@#endif@) as
-- the first argument to the given continuation, and the remaining
-- input as the second argument.
takeConditional :: [String] -> (DList String -> [String] -> r) -> r
takeConditional lns0 k = go id lns0
  where go acc [] = k acc []
        go acc (ln:lns) =
          case getCmd ln of
            Nothing -> go (acc . (ln:)) lns
            Just (cmd,_)
              | cmd == "endif" -> k (acc . (ln:)) lns
              | cmd `elem` ["if","ifdef","ifndef"] ->
                  takeConditional lns $
                  \acc' lns' -> go (acc . (ln:) . acc') lns'
              | otherwise -> go (acc . (ln:)) lns

-- | Take everything up to the end of this branch, drop all remaining
-- branches (if any), and inject a @line@ update command in the
-- remaining stream. The first argument is the first line of branch
-- being taken. This is supplied so branches not taken can be
-- discounted from the running line count.
takeBranch :: [String] -> Either Error [String]
takeBranch = go id
  where go _ [] = Left UnterminatedBranch
        go acc (ln:lns) =
          case getCmd ln of
            Just (cmd,_)
              | cmd `elem` ["if","ifdef","ifndef"] ->
                  takeConditional lns $
                  \acc' lns' -> go (acc . (ln:) . acc') lns'
              | cmd == "endif" -> Right (acc [] ++ lns)
              | cmd `elem` ["else","elif"] ->
                case dropAllBranches lns of
                  Right lns' -> Right $ acc [] ++ lns'
                  Left err -> Left err
            _ -> go (acc . (ln:)) lns

dropAllBranches :: [String] -> Either Error [String]
dropAllBranches = aux <=< dropBranch
  where aux :: [String] -> Either Error [String]
        aux [] = Left UnterminatedBranch
        aux (ln:lns) = case getCmd ln of
                         Just ("endif",_) -> Right lns
                         _ -> dropAllBranches lns

-- | Skip to the end of a conditional branch, returning the remaining
-- input.
dropBranch :: [String] -> Either Error [String]
dropBranch = go
  where go [] = Left UnterminatedBranch
        go (l:ls) = case getCmd l of
                      Just (cmd,_)
                        -- Drop nested conditional blocks
                        | cmd `elem` ["if","ifdef","ifndef"] ->
                          dropAllBranches ls >>= go
                        | cmd `elem` ["else","elif","endif"] -> Right (l:ls)
                        | otherwise -> go ls
                      Nothing -> go ls

-- | Returns a new macro binding environment and the result of
-- expanding the input string.
macroExpansion :: Config
               -- ^ Options controlling the preprocessor
               -> Env
               -- ^ Macro binding environment
               -> [String]
               -- ^ Input lines
               -> ErrHpp (Env, [String])
macroExpansion cfg0 macros = go 1 cfg0 macros id
  where go :: Int -> Config -> [(String, Macro)]
           -> DList String -> [String]
           -> ErrHpp (Env, [String])
        go _ _ ms acc [] = return (ms, acc [])
        go lineNum cfg ms acc (x:xs) =
          case dropWhile isSpace x of
            [] -> go (lineNum + 1) cfg ms (acc . (x:)) xs
            ('#':cmd) -> do k <- directive cfg lineNum cmd
                            k (\lineNum' newLines cfg' ms' remainingInput ->
                                 go lineNum' cfg' ms' (acc . (newLines++))
                                    remainingInput)
                              ms xs
            _ -> do (ms',x') <- either errorHpp pure $
                                expandLine cfg lineNum ms (tokenize x)
                    go (lineNum+1) cfg ms' (acc . (detokenize x':)) xs

-- | @preprocess env src@ runs the pre-processor over source code
-- @src@ beginning with macro binding environment @env@.
preprocess :: Env -> String -> ErrHpp (Env, String)
preprocess env inp =
  do cfg <- liftHpp $ GetConfig return
     let splicer = if spliceLongLines cfg then lineSplicing else id
         decomment = if eraseCComments cfg then commentRemoval else id
         appSplicer = if runIdentity (spliceApplicationsF cfg)
                        then spliceApplications else id
         go = macroExpansion cfg env
              -- (\lns -> return (env, lns))
            . appSplicer . splicer . decomment
            . map trigraphReplacement
     fmap (second unlines) . go $ lines inp

-- | Run an 'Hpp' action that might fail with a given initial
-- configuration.
runErrHppIO :: Config -> ErrHpp a -> IO a
runErrHppIO cfg = fmap (either err (either err id)) . runHpp cfg . runErrHpp
  where err :: (FilePath,Error) -> a
        err = error . show
