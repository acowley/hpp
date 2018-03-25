{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | A front-end to run Hpp with textual arguments as from a command
-- line invocation.
module Hpp.CmdLine (runWithArgs) where
import Control.Monad (unless)
import Control.Monad.Trans.Except (runExceptT)
import Data.String (fromString)
import Hpp
import Hpp.Config
import Hpp.Env (deleteKey, emptyEnv, insertPair)
import Hpp.StringSig (readLines, putStringy)
import Hpp.Types (Env, Error(..))
import System.Directory (doesFileExist, makeAbsolute)
import System.IO (openFile, IOMode(..), hClose, stdout)

-- | Break a string on an equals sign. For example, the string @x=y@
-- is broken into @[x,"=",y]@.
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
splitSwitches ('-':'i':'n':'c':'l':'u':'d':'e':t@(_:_)) = ["-include",t]
splitSwitches x = [x]

-- FIXME: Defining function macros probably doesn't work here.
parseArgs :: ConfigF Maybe -> [String]
          -> IO (Either Error (Env, [String], Config, Maybe FilePath))
parseArgs cfg0 = go emptyEnv id cfg0 Nothing . concatMap breakEqs
  where go env acc cfg out [] =
          case realizeConfig cfg of
            Just cfg' -> return (Right (env, acc [], cfg', out))
            Nothing -> return (Left NoInputFile)
        go env acc cfg out ("-D":name:"=":body:rst) =
          case parseDefinition (fromString name) (fromString body) of
            Nothing -> return . Left $ BadMacroDefinition 0
            Just def -> go (insertPair def env) acc cfg out rst
        go env acc cfg out ("-D":name:rst) =
          case parseDefinition (fromString name) "1" of
            Nothing -> return . Left $ BadMacroDefinition 0
            Just def -> go (insertPair def env) acc cfg out rst
        go env acc cfg out ("-U":name:rst) =
          go (deleteKey (fromString name) env) acc cfg out rst
        go env acc cfg out ("-I":dir:rst) =
          let cfg' = cfg { includePathsF = fmap (++[dir]) (includePathsF cfg) }
          in go env acc cfg' out rst
        go env acc cfg out ("-include":file:rst) =
          let ln = "#include \"" ++ file ++ "\""
          in go env (acc . (ln:)) cfg out rst
        go env acc cfg out ("-P":rst) =
          let cfg' = cfg { inhibitLinemarkersF = Just True }
          in go env acc cfg' out rst
        go env acc cfg out ("--cpp":rst) =
          let cfg' = cfg { spliceLongLinesF = Just True
                         , eraseCCommentsF = Just True
                         , inhibitLinemarkersF = Just False
                         , replaceTrigraphsF = Just True }
              defs = concatMap ("-D":)
                       [ ["__STDC__"]
                         -- __STDC_VERSION__ is only defined in C94 and later
                       , ["__STDC_VERSION__","=","199409L"] ]
                       -- , ["_POSIX_C_SOURCE","=","200112L"] ]
          in go env acc cfg' out (defs ++ rst)
        go env acc cfg out ("--fline-splice":rst) =
          go env acc (cfg { spliceLongLinesF = Just True }) out rst
        go env acc cfg out ("--ferase-comments":rst) =
          go env acc (cfg { eraseCCommentsF = Just True }) out rst
        go env acc cfg out ("--freplace-trigraphs":rst) =
          go env acc (cfg { replaceTrigraphsF = Just True }) out rst
        go env acc cfg _ ("-o":file:rst) =
          go env acc cfg (Just file) rst
        go env acc cfg out ("-x":_lang:rst) =
          go env acc cfg out rst -- We ignore source language specification
        go env acc cfg out ("-traditional":rst) =
          go env acc cfg out rst -- Ignore the "-traditional" flag
        go env acc cfg Nothing (file:rst) =
          case curFileNameF cfg of
            Nothing -> go env acc (cfg { curFileNameF = Just file }) Nothing rst
            Just _ -> go env acc cfg (Just file) rst
        go _ _ _ (Just _) _ =
          return . Left $ BadCommandLine "Multiple output files given"

-- | Run Hpp with the given commandline arguments.
runWithArgs :: [String] -> IO (Maybe Error)
runWithArgs args =
  do cfgNow <- defaultConfigFNow
     let args' = concatMap splitSwitches args
     (env,lns,cfg,outPath) <- fmap (either (error . show) id)
                                   (parseArgs cfgNow args')
     exists <- doesFileExist (curFileName cfg)
     unless exists . error $
       "Couldn't open input file: "++curFileName cfg
     let fileName = curFileName cfg
         cfg' = cfg { curFileNameF = pure fileName }
     (snk, closeSnk) <- case outPath of
                          Nothing -> return (mapM_ (putStringy stdout) , return ())
                          Just f ->
                            do h <- makeAbsolute f >>=
                                    flip openFile WriteMode
                               return ( \os -> mapM_ (putStringy h) os
                                      , hClose h )
     result <- readLines fileName
           >>= runExceptT
               . streamHpp (initHppState cfg' env) snk
               . preprocess
               . (map fromString lns ++)
     closeSnk
     case result of
       Left error -> return $ Just error
       _ -> return Nothing
