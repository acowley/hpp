{-# LANGUAGE LambdaCase #-}
import Control.Monad (join, unless)
import Hpp
import Hpp.Config
import Hpp.Env (Env, deleteKey)
import Hpp.Tokens
import Hpp.Types (Error(..))
import System.Directory (doesFileExist, setCurrentDirectory)
import System.Environment
import System.FilePath (splitFileName)

usage :: IO ()
usage = mapM_ putStrLn
  [ "Usage: hpp [options] inputFile [outputFile]"
  , ""
  , "Options:"
  , "-D name"
  , "  Define name as an object macro defined as 1."
  , "-D name=definition"
  , "  Define name as an object macro defined as definition."
  , "-U name"
  , "  Remove any previous definition of name."
  , "-I dir"
  , "  Add directory dir to the search path for includes."
  , "-o file"
  , "  Write output to file."
  , "-include file"
  , "  Acts as if #include \"file\" were the first line "
  , "  in the primary source file. -include options are "
  , "  processed after -D and -U options."
  , "-imacros file"
  , "  Like -include, except that output is discarded. Only"
  , "  macro definitions are kept."
  , "-c-compat"
  , "  C98 compatibility. Implies: -fline-splice -ferase-comments"
  , "  -fapplication-splice -D __STDC__ "
  , "  -D __STDC_VERSION__=199409L -D _POSIX_C_SOURCE=200112L"
  , "-fline-splice"
  , "  Enable continued line splicing."
  , "-ferase-comments"
  , "  Remove all C-style comments before processing."
  , "-fapplication-splice"
  , "  Support multi-line function applications." ]

breakEqs :: String -> [String]
breakEqs = aux . break (== '=')
  where aux (h,[]) = [h]
        aux (h,'=':t) = [h,"=",t]
        aux _ = error "breakEqs broke"

-- | If no space is included between a switch and its argument, break
-- it into two tokens to simplify parsing.
splitSwitches :: String -> [String]
splitSwitches ('-':'I':t@(_:_)) = ["-I",t]
splitSwitches ('-':'D':t@(_:_)) = ["-D",t]
splitSwitches ('-':'U':t@(_:_)) = ["-U",t]
splitSwitches ('-':'o':t@(_:_)) = ["-o",t]
splitSwitches x = [x]

-- FIXME: Defining function macros probably doesn't work here.
parseArgs :: ConfigF Maybe -> [String]
          -> IO (Either Error (Env, [String], Config, Maybe FilePath))
parseArgs cfg0 = go [] id cfg0 Nothing . concatMap breakEqs
  where go env acc cfg out [] =
          case realizeConfig cfg of
            Just cfg' -> return (Right (env, acc [], cfg', out))
            Nothing -> return (Left NoInputFile)
        go env acc cfg out ("-D":name:"=":body:rst) =
          case parseDefinition (Important name : Other " " : tokenize body) of
            Nothing -> return . Left $ BadMacroDefinition 0
            Just def -> go (def:env) acc cfg out rst
        go env acc cfg out ("-D":name:rst) =
          case parseDefinition ([Important name, Other " ", Important "1"]) of
            Nothing -> return . Left $ BadMacroDefinition 0
            Just def -> go (def:env) acc cfg out rst
        go env acc cfg out ("-U":name:rst) =
          go (deleteKey name env) acc cfg out rst
        go env acc cfg out ("-I":dir:rst) =
          let cfg' = cfg { includePathsF = fmap (++[dir]) (includePathsF cfg) }
          in go env acc cfg' out rst
        go env acc cfg out ("-include":file:rst) =
          let ln = "include \"" ++ file ++ "\""
          in go env (acc . (ln:)) cfg out rst
        go env acc cfg out ("-c-compat":rst) =
          let cfg' = cfg { spliceLongLinesF = Just True
                         , eraseCCommentsF = Just True
                         , spliceApplicationsF = Just True }
              defs = concatMap ("-D":)
                       [ ["__STDC__"]
                         -- __STDC_VERSION__ is only defined in C94 and later
                       , ["__STDC_VERSION__","=","199409L"]
                       , ["_POSIX_C_SOURCE","=","200112L"] ]
          in go env acc cfg' out (defs ++ rst)
        go env acc cfg out ("-fline-splice":rst) =
          go env acc (cfg { spliceLongLinesF = Just True }) out rst
        go env acc cfg out ("-ferase-comments":rst) =
          go env acc (cfg { eraseCCommentsF = Just True }) out rst
        go env acc cfg out ("-fapplication-splice":rst) =
          go env acc (cfg { spliceApplicationsF = Just True }) out rst
        go env acc cfg _ ("-o":file:rst) =
          go env acc cfg (Just file) rst
        go env acc cfg out ("-x":_lang:rst) =
          go env acc cfg out rst -- We ignore source language specification
        go env acc cfg Nothing (file:rst) =
          case curFileNameF cfg of
            Nothing -> go env acc (cfg { curFileNameF = Just file }) Nothing rst
            Just _ -> go env acc cfg (Just file) rst
        go _ _ _ (Just _) _ =
          return . Left $ BadCommandLine "Multiple output files given"

main :: IO ()
main = do getArgs >>= \case
            [] -> usage
            args -> do cfgNow <- defaultConfigFNow
                       let args' = concatMap splitSwitches args
                       (env,lns,cfg,outPath) <- fmap (either (error . show) id)
                                                     (parseArgs cfgNow args')
                       exists <- doesFileExist (curFileName cfg)
                       unless exists . error $
                         "Couldn't open input file: "++curFileName cfg
                       let (dir,fileName) = splitFileName $ curFileName cfg
                       setCurrentDirectory dir
                       let cfg' = cfg { curFileNameF = pure fileName }
                       join . runErrHppIO cfg' $
                         do src <- liftHpp . hppReadFile 0
                                   $ '"' : curFileName cfg ++ "\""
                            
                            (_,r) <- preprocess env (unlines lns ++ src)
                            case outPath of
                              Nothing -> return (putStrLn r)
                              Just f -> return (writeFile f r)


{-

For testing against C:

hpp -I/usr/local/include -I/usr/include -fline-splice -ferase-comments -fapplication-splice -D __x86_64__ -D __GNUC__ -D _POSIX_C_SOURCE n_1.c

For the mcpp validation suite

../tool/cpp_test HPP "../../dist/build/hpp/hpp -I/usr/local/include -I/usr/include -fline-splice -ferase-comments -fapplication-splice -D __x86_64__ -D __GNUC__=4 -D __STDC__ -D __STDC_VERSION__=199409L -D _POSIX_C_SOURCE -D __DARWIN_ONLY_UNIX_CONFORMANCE %s.c | gcc -o %s -x c -" "rm %s" < n_i_.lst 


-}
