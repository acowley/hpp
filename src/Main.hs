import Control.Arrow (first, second)
import Data.Char (isSpace)
import Data.List
import Data.Function (on)
import System.Environment

data Config = Config { fileName :: FilePath
                     , includePaths :: FilePath }

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
trigraphReplacement = go 0
  where go :: Int -> String -> String
        go _ [] = []
        go 2 ('?':xs) = '?' : go 2 xs
        go 2 (x:xs) = case lookup x trigraphs of
                        Just x' -> x' : go 0 xs
                        Nothing -> "??" ++ x : go 0 xs
        go i ('?':xs) = go (i+1) xs

lineSplicing :: String -> String
lineSplicing = unlines . go . lines
  where go :: [String] -> [String]
        go [] = []
        go [x] = [x]
        go (x:t@(y:xs))
          | last x == '\\' = go ((x++y) : xs)
          | otherwise = x : go t

data Macro = Object String
           | Function ([String] -> String)

nextTok :: String -> (String, String)
nextTok = break (\c -> isSpace c || c == '(' || c == ')') . dropWhile isSpace

deleteKey :: Eq a => a -> [(a,b)] -> [(a,b)]
deleteKey k = go
  where go [] = []
        go (h@(x,_) : xs) = if x == k then xs else h : go xs

directive :: Config -> Int -> String
          -> Maybe ((Int -> [(String,Macro)] -> [String] -> r)
                     -> [(String,Macro)] -> [String] -> r)
directive cfg i ln =
  case cmd of
    "define" -> case rst of
                  ('(':rst') -> let f = _
                                in Just $ \k ->
                                   k i'  . ((name, Function f):)
                  _ -> Just $ \k ->
                       k i' . ((name, Object (dropWhile isSpace rst)):)
    "undef" -> Just $ \k -> \ms -> 
               k i' (deleteKey name ms)

    -- We need to drop the branch not taken, and insert a #line
    -- command to fixup line counting.
    "ifdef" -> Just $ \k -> \ms lns ->
               if name `elem` map fst ms
                 then _
                 else _
    "ifndef" -> _
    "if" -> _
    "elif" -> _
    "line" -> _
  where (cmd,cmdArg) = nextTok ln
        (name,rst) = nextTok cmdArg
        i' = i + 1

data Error = UnterminatedBranch
           | BadMacroFunction
           | BadIfPredicate
           | BadLineArgument

getCmd :: String -> Maybe (String,String)
getCmd ln = case dropWhile isSpace ln of
              ('#':rst) -> Just (nextTok rst)
              _ -> Nothing

takeBranch :: [String] -> Either Error ([String], [String])
takeBranch = _

dropAllBranches :: [String] -> Either Error (Int, [String])
dropAllBranches = fmap aux . dropBranch
  where aux (n,[]) = _
        aux (n, ln:lns) = case getCmd ln of
                           Just ("endif",_) -> Right (n+1,lns)
                           _ -> fmap (first ((n+1)+)) $ dropAllBranches lns

-- | Skip to the end of a conditional branch, returning how many lines
-- we've skipped, and the remaining input.
dropBranch :: [String] -> Either Error (Int, [String])
dropBranch = go 0
  where go _ [] = Left UnterminatedBranch
        go n (l:ls) = case getCmd l of
                        Just (cmd,_) ->
                          if cmd `elem` ["else","elif","endif"]
                            then Right (n, l:ls)
                            else go (n+1) ls
                        Nothing -> go (n+1) ls

-- Do we really want to do this? A main purpose is removing C
-- comments, which we perhaps don't care about when dealing with
-- Haskell code.
tokenization = id

macroExpansion :: Config -> [(String, Macro)] -> String
               -> ([(String,Macro)], String)
macroExpansion cfg macros = second unlines . go 1 macros . lines
  where go :: Int -> [(String, Macro)] -> [String]
           -> ([(String, Macro)], [String])
        go lineNum ms [] = (ms, [])
        go lineNum ms (x:xs) =
          case dropWhile isSpace x of
            [] -> second (x :) $ go (lineNum + 1) ms xs
            ('#':cmd) ->
              case takeWhile (not . isSpace) cmd of
                "ifdef" -> let tok = dropWhile isSpace $ drop 5 cmd
                           in if tok `elem` map fst ms
                              then _
                              else _
                "ifndef" -> let tok = dropWhile isSpace $ drop 6 cmd
                            in if tok `elem` map fst ms
                               then _
                               else _
                "if" -> _
                "line" -> _
                "include" -> _
                "undef" -> let tok = dropWhile isSpace $ drop 5 cmd
                               ms' = deleteBy ((==) `on` fst)
                                              (tok, Object "")
                                              ms
                           in go (lineNum+1) ms' xs
                "define" -> _
            _ -> second (expandLine x :) $ go (lineNum+1) ms xs
          where expandLine [] = []
                expandLine l = _

runCPP :: Config -> String -> String
runCPP _ = id

main :: IO ()
main = return ()
