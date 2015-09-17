{-# LANGUAGE FlexibleInstances, LambdaCase #-}
-- | The core types involved used by the pre-processor.
module Hpp.Types where
import Control.Monad (ap, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
-- import qualified Data.Map as M
import Hpp.Config
import Hpp.Tokens

-- | Line numbers are represented as 'Int's
type LineNum = Int

-- | A macro binding environment.
type Env = [(String, Macro)]
-- type Env = M.Map String Macro

-- * Errors

-- | Error conditions we may encounter.
data Error = UnterminatedBranch
           | BadMacroDefinition LineNum
           | BadIfPredicate
           | BadLineArgument LineNum String
           | IncludeDoesNotExist LineNum FilePath
           | FailedInclude LineNum FilePath
           | UserError LineNum String
           | UnknownCommand LineNum String
           | TooFewArgumentsToMacro LineNum String
           | BadMacroArguments LineNum String
           | NoInputFile
           | BadCommandLine String
           | RanOutOfInput
             deriving (Eq, Ord, Show)

-- | Hpp can raise various parsing errors.
class HasError m where
  throwError :: Error -> m a

instance Monad m => HasError (ExceptT Error m) where
  throwError = throwE

instance (Monad m, HasHppState m) => HasHppState (ExceptT e m) where
  getState = lift getState
  {-# INLINE getState #-}
  setState = lift . setState
  {-# INLINE setState #-}

-- * Resource cleanup

-- | A cleanup action that is run at most once. To be used as an
-- abstract type with only 'runCleanup' and 'mkCleanup' as interface.
newtype Cleanup  = Cleanup (IORef (IO ()))

-- | Runs an action and replaces it with a nop
runCleanup :: Cleanup -> IO ()
runCleanup (Cleanup r) = join (readIORef r) >> writeIORef r (return ())

-- | @mkCleanup cleanup@ returns two things: a 'Cleanup' value, and an
-- action to neutralize that 'Cleanup'. In this way, the 'Cleanup'
-- value can be registered with a resource manager so that, in the
-- event of an error, the cleanup action is run, while the neutralizer
-- may be used to ensure that the registered 'Cleanup' action has no
-- effect if it is run. Typically one would neutralize a registered
-- cleanup action before performing a manual cleanup that subsumes the
-- registered cleanup.
mkCleanup :: IO () -> IO (Cleanup, IO ())
mkCleanup m = do r <- newIORef m
                 return $ (Cleanup r, writeIORef r (return ()))

-- * Free Monad Transformers

-- | Base functor for a free monad transformer
data FreeF f a r = PureF a | FreeF (f r)

instance Functor f => Functor (FreeF f a) where
  fmap _ (PureF x) = PureF x
  fmap f (FreeF x) = FreeF $ fmap f x
  {-# INLINE fmap #-}

-- * Pre-processor Actions

-- | Dynamic state of the preprocessor engine.
data HppState = HppState { hppConfig :: Config
                         , hppLineNum :: LineNum
                         , hppCleanups :: [Cleanup]
                         , hppEnv :: Env }

-- | A free monad construction to strictly delimit what capabilities
-- we need to perform pre-processing.
data HppF t r = ReadFile Int FilePath (t -> r)
              | ReadNext Int FilePath (t -> r)
              | GetState (HppState -> r)
              | SetState HppState r
              | ThrowError Error

instance Functor (HppF t) where
  fmap f (ReadFile ln file k) = ReadFile ln file (f . k)
  fmap f (ReadNext ln file k) = ReadNext ln file (f . k)
  fmap f (GetState k) = GetState (f . k)
  fmap f (SetState cfg k) = SetState cfg (f k)
  fmap _ (ThrowError e) = ThrowError e
  {-# INLINE fmap #-}

-- * Hpp Monad Transformer

-- | A free monad transformer specialized to HppF as the base functor.
newtype HppT t m a = HppT { runHppT :: m (FreeF (HppF t) a (HppT t m a)) }

instance Functor m => Functor (HppT t m) where
  fmap f (HppT x) = HppT $ fmap f' x
    where f' (PureF y) = PureF (f y)
          f' (FreeF y) = FreeF $ fmap (fmap f) y
  {-# INLINE fmap #-}

instance Monad m => Applicative (HppT t m) where
  pure = HppT . pure . PureF
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad m => Monad (HppT t m) where
  return = pure
  {-# INLINE return #-}
  HppT ma >>= fb = HppT $ ma >>= \case
                     PureF x -> runHppT $ fb x
                     FreeF x -> return . FreeF $ fmap (>>= fb) x
  {-# INLINE (>>=) #-}

instance MonadTrans (HppT t) where
  lift = HppT . fmap PureF
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (HppT t m) where
  liftIO = HppT . fmap PureF . liftIO
  {-# INLINE liftIO #-}

-- | An interpreter capability to modify dynamic state.
class HasHppState m where
  getState :: m HppState
  setState :: HppState -> m ()

instance Monad m => HasHppState (HppT t m) where
  getState = HppT . pure . FreeF $ GetState pure
  {-# INLINE getState #-}
  setState s = HppT . pure . FreeF $ SetState s (pure ())
  {-# INLINE setState #-}

-- | An interpreter capability of threading a binding environment.
class HasEnv m where
  getEnv :: m Env
  setEnv :: Env -> m ()

instance Monad m => HasEnv (HppT t m) where
  getEnv = fmap hppEnv getState
  {-# INLINE getEnv #-}
  setEnv e = getState >>= setState . (\s -> s { hppEnv = e })
  {-# INLINE setEnv #-}

instance Applicative m => HasError (HppT t m) where
  throwError = HppT . pure . FreeF . ThrowError
  {-# INLINE throwError #-}

instance (HasEnv m, Monad m) => HasEnv (ExceptT e m) where
  getEnv = lift getEnv
  {-# INLINE getEnv #-}
  setEnv = lift . setEnv
  {-# INLINE setEnv #-}

-- * Expansion

-- | Macro expansion involves treating tokens differently if they
-- appear in the original source for or as the result of a previous
-- macro expansion. This distinction is used to prevent divergence by
-- masking out definitions that could be used recursively.
-- 
-- Things are made somewhat more complicated than one might expect due
-- to the fact that the scope of this masking is /not/ structurally
-- recursive. A object-like macro can expand into a fragment of a
-- macro function application, one of whose arguments is a token
-- matching the original object-like macro. That argument should /not/
-- be expanded.
data Scan = Unmask String
          | Mask String
          | Scan Token
          | Rescan Token
            deriving (Eq, Show)

-- * Macros

-- | There are object-like macros and function-like macros.
data Macro = Object [Token]
           -- ^ An object-like macro is replaced with its definition
           | Function Int ([([Scan],String)] -> [Scan])
           -- ^ A function-like macro of some arity taks
           -- macro-expanded and raw versions of its arguments, then
           -- substitutes them into a body producing a new set of
           -- tokens.

instance Show Macro where
  show (Object ts) = "Object "++ detokenize ts
  show (Function n _) = "Fun<"++show n++">" 
