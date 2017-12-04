{-# LANGUAGE BangPatterns, ConstraintKinds, LambdaCase, OverloadedStrings,
             ScopedTypeVariables, TupleSections, ViewPatterns #-}
-- | Front-end interface to the pre-processor.
module Hpp (parseDefinition, preprocess,
            T.HppState, emptyHppState, initHppState,
            runHpp, streamHpp, HppT, HppOutput(..)) where
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Hpp.Config as C
import qualified Hpp.RunHpp as R
import qualified Hpp.Types as T
import Hpp.Parser (evalParse, Parser)
import Hpp.StringSig (readLines)
import Hpp.Tokens (tokenize)

newtype HppT m a =
  HppT (T.HppT [ByteString]
               (Parser (StateT T.HppState (ExceptT T.Error m))
                       [T.TOKEN])
               a)

data HppOutput = HppOutput { hppFilesRead :: [FilePath]
                           , hppOutput    :: [ByteString] }

preprocess :: Monad m => [ByteString] -> HppT m ()
preprocess = HppT . R.preprocess

runHpp :: MonadIO m
       => T.HppState
       -> HppT m a
       -> ExceptT T.Error m (HppOutput, T.HppState)
runHpp st h =
  do r <- liftIO (newIORef id)
     let snk xs = modifyIORef r (. (xs++))
     let fin (x, st') = do outLines <- ($ []) <$> readIORef r
                           return (HppOutput x outLines, st')
     streamHpp st (liftIO . snk) h >>= liftIO . fin

streamHpp :: MonadIO m
          => T.HppState
          -> ([ByteString] -> m ())
          -> HppT m a
          -> ExceptT T.Error m ([FilePath], T.HppState)
streamHpp st snk (HppT h) =
  do (a, st') <- runStateT
                   (evalParse
                      (R.runHpp (liftIO . readLines)
                                (lift . lift . lift . snk) h)
                      [])
                   st
     either (throwE . snd) (return . (,st') . R.hppFilesRead) a

emptyHppState :: T.HppState
emptyHppState = T.emptyHppState
              $ fromMaybe (error "emptyHppState assumption wrong")
                          (C.realizeConfig cfg)
  where cfg = C.defaultConfigF { C.curFileNameF = Just "NoFile" }


initHppState :: C.Config -> T.Env -> T.HppState
initHppState c e = T.HppState c 1 e

parseDefinition :: ByteString -> Maybe (ByteString, T.Macro)
parseDefinition = R.parseDefinition . tokenize
