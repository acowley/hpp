module AdhocTests where
import Data.Char (isSpace)
import Hpp
import Hpp.Config
import Hpp.Env
import Hpp.Tokens
import Hpp.Types

testProg :: Int -> String
testProg n = "mcpp-2.7.2/test-c/n_" ++ show n++".c"

testProg' :: String -> String
testProg' n = "mcpp-2.7.2/test-c/n_" ++ n++".c"

squashBlanks :: String -> String
squashBlanks = unlines . go . lines
  where go [] = []
        go (l:ls)
          | all isSpace l = "" : go (dropWhile (all isSpace) ls)
          | otherwise = l : go ls

runTestProg :: String -> IO (Env, String)
runTestProg f =
  do src <- readFile f
     runErrHppIO
         cfg
         (preprocess [ ("__x86_64__", Object [Important "1"])
                     , ("__GNUC__", Object [Important "4"])
                     , ("__STDC__", Object [Important "1"])
                     , ("__DARWIN_ONLY_UNIX_CONFORMANCE", Object [Important "1"])
                     , ("_POSIX_C_SOURCE", Object [Important "1"])
                     ]
                     src)
  where Just cfg =
          realizeConfig $
          defaultConfigF { curFileNameF = Just "n_28.c" -- "test"
                         , eraseCCommentsF = Just True
                         , spliceLongLinesF = Just True
                         , spliceApplicationsF = Just True
                         , includePathsF = Just [ "/usr/include"
                                                , "mcpp-2.7.2/test-c"] }
