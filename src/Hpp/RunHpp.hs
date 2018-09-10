{-# LANGUAGE BangPatterns, ConstraintKinds, LambdaCase, OverloadedStrings,
             ScopedTypeVariables, TupleSections, ViewPatterns #-}
-- | Mid-level interface to the pre-processor.
module Hpp.RunHpp (preprocess, runHpp, expandHpp,
                   hppIOSink, hppIO, HppResult(..)) where
import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, State)
import Data.IORef
import Hpp.Config (Config, curFileNameF, curFileName, includePaths, inhibitLinemarkers)
import Hpp.Directive (macroExpansion)
import Hpp.Parser (Parser, precede, evalParse)
import Hpp.Preprocessing
import Hpp.StringSig
import Hpp.String (stripAngleBrackets)
import Hpp.Tokens (detokenize)
import Hpp.Types
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Prelude hiding (String)
import qualified Prelude as P

-- * Finding @include@ files

includeCandidates :: FilePath -> [FilePath] -> P.String -> Maybe [FilePath]
includeCandidates curDir searchPath nm =
  case nm of
    '<':nm' -> Just $ sysSearch   (init nm')
    '"':nm' -> let nm'' = init nm'
               in Just $ nm'' : localSearch nm''
    _ -> Nothing
  where sysSearch   f = map (</> f) searchPath
        localSearch f = map (</> f) $ curDir : searchPath

searchForInclude :: FilePath -> [FilePath] -> P.String -> IO (Maybe FilePath)
searchForInclude curDir paths =
  maybe (return Nothing) aux . includeCandidates curDir paths
  where aux [] = return Nothing
        aux (f:fs) = do exists <- doesFileExist f
                        if exists then return (Just f) else aux fs

searchForNextInclude :: FilePath -> [FilePath] -> P.String -> IO (Maybe FilePath)
searchForNextInclude curDir paths =
  maybe (return Nothing) (aux False) . includeCandidates curDir paths
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
          ReadFile ln file k -> do
            cfg    <- use config
            curDir <- use dir
            let ipaths = includePaths cfg
            mFound <- liftIO $ searchForInclude curDir ipaths file
            readAux (file:files) ln file k mFound
          ReadNext ln file k -> do
            cfg    <- use config
            curDir <- use dir
            let ipaths = includePaths cfg
            mFound <- liftIO $ searchForNextInclude curDir ipaths file
            readAux (file:files) ln file k mFound
          WriteOutput output k -> sink output >> runHppT k >>= go files

        readAux _files ln file _ Nothing =
          Left . (, IncludeDoesNotExist ln (stripAngleBrackets file))
               . curFileName <$> use config
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

parseStreamHpp :: Monad m
               => HppT t (Parser m i) (Maybe t) -> HppT t (Parser m i) ()
parseStreamHpp m = go
  where go = m >>= \case
          Nothing -> return ()
          Just o -> hppWriteOutput o >> go

-- * Front End

-- | Run a stream of lines through the preprocessor.
preprocess :: (Monad m, HasHppState m, HasError m, HasEnv m)
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

-- | A concreate choice of types to satisfy the constraints of
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
