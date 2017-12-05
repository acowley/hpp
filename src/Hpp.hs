{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
-- | Front-end interface to the pre-processor.
module Hpp ( -- * Running the Preprocessor
            preprocess, runHpp, streamHpp, expand,
            -- * Preprocessor State
            T.HppState, emptyHppState, initHppState,
            -- * Adding Definitions
            parseDefinition, addDefinition,
            -- * Core Types
             HppT, HppOutput(..)
            ) where
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except (ExceptT, Except, throwE, runExceptT)
import qualified Control.Monad.Trans.State.Strict as S
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Hpp.Config as C
import qualified Hpp.Env as E
import qualified Hpp.RunHpp as R
import qualified Hpp.Types as T
import Hpp.Parser (evalParse, Parser)
import Hpp.StringSig (readLines)
import Hpp.Tokens (tokenize)

-- | The type of preprocessor actions. Created with 'preprocess' and
-- executed with 'runHpp' or 'streamHpp'.
newtype HppT m a =
  HppT (T.HppT [ByteString]
               (Parser (S.StateT T.HppState (ExceptT T.Error m))
                       [T.TOKEN])
               a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans HppT where
  lift = HppT . lift . lift . lift . lift

-- | The result of running hpp
data HppOutput = HppOutput { hppFilesRead :: [FilePath]
                           , hppOutput    :: [ByteString] }

-- | Preprocess lines of input.
preprocess :: Monad m => [ByteString] -> HppT m ()
preprocess = HppT . R.preprocess

-- | Run a preprocessor action with some initial state. Returns the
-- result of preprocessing as well as an updated preprocessor state
-- representation.
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

-- | @streamHpp state sink action@ runs a preprocessor @action@ with
-- some initial @state@. Output is streamed to the caller-provided
-- output @sink@ as it is generated. The list of files read during
-- preprocessing is returned along with an updated preprocessor state
-- representation.
streamHpp :: MonadIO m
          => T.HppState
          -> ([ByteString] -> m ())
          -> HppT m a
          -> ExceptT T.Error m ([FilePath], T.HppState)
streamHpp st snk (HppT h) =
  do (a, st') <- S.runStateT
                     (evalParse
                        (R.runHpp (liftIO . readLines)
                                  (lift . lift . lift . snk) h)
                        [])
                     st
     either (throwE . snd) (return . (,st') . R.hppFilesRead) a

-- | Like 'runHpp', but does not access the filesystem. Run a
-- preprocessor action with some initial state. Returns the result of
-- preprocessing as well as an updated preprocessor state
-- representation. Since this operation performs no IO, @#include@ directives are ignored in terms of the generated output lines, but the files named in those directive are available in the 'HppOutput' value returned.
expand :: T.HppState
       -> HppT (S.State ([ByteString] -> [ByteString])) a
       -> Except T.Error (HppOutput, T.HppState)
expand st (HppT h) =
  case result of
    Left e -> throwE e
    Right (Left (_, e), _) -> throwE e
    Right (Right x, st') ->
      return (HppOutput (R.hppFilesRead x) (outDlist []), st')
  where snk xs = S.modify (. (xs++))
        expanded = (S.runStateT
                      (evalParse
                         (R.expandHpp (lift . lift . lift . snk) h)
                         [])
                      st)
        (result, outDlist) = S.runState (runExceptT expanded) id

-- | An 'T.HppState' containing no macro definitions, and default
-- values for the starting configuration: the name of the current file
-- is @\"NoFile\"@, there are no paths to be searched for included
-- files, etc. See 'C.Config' for more information on available
-- configuration.
emptyHppState :: T.HppState
emptyHppState = T.emptyHppState
              $ fromMaybe (error "emptyHppState assumption wrong")
                          (C.realizeConfig cfg)
  where cfg = C.defaultConfigF { C.curFileNameF = Just "NoFile" }

-- | Create a 'T.HppState' with the given 'C.Config' and 'T.Env'.
initHppState :: C.Config -> T.Env -> T.HppState
initHppState c e = T.HppState c 1 e

-- | @addDefinition name expression@ adds a binding of @name@ to
-- @expression@ in the preprocessor’s internal state.
addDefinition :: ByteString -> ByteString -> T.HppState -> Maybe T.HppState
addDefinition name val s = flip (T.over T.env) s . E.insertPair
                           <$> parseDefinition name val

-- | Lower level parsing of macro definitions. Will typically be used
-- with 'E.insertPair' for manual construction of a 'T.Env' binding
-- environment.
parseDefinition :: ByteString -> ByteString -> Maybe (ByteString, T.Macro)
parseDefinition name val = R.parseDefinition (tokenize name ++ tokenize val)
