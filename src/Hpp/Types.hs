{-# LANGUAGE FlexibleInstances, LambdaCase, Rank2Types #-}
-- | The core types involved used by the pre-processor.
module Hpp.Types where
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Constant
import Data.Functor.Identity
-- import qualified Data.Map as M
import qualified Data.Trie as T
import Hpp.Config
import Hpp.Env (emptyEnv, lookupKey)
import Hpp.StringSig (toChars)
import Hpp.Tokens
import Prelude hiding (String)
import qualified Prelude as P

-- | Line numbers are represented as 'Int's
type LineNum = Int

-- | A macro binding environment.
-- type Env = [(String, Macro)]
-- type Env = M.Map String Macro
type Env = T.Trie Macro

-- * Changing the underlying string type
type String = ByteString
type TOKEN = Token ByteString

-- * Errors

-- | Error conditions we may encounter.
data Error = UnterminatedBranch
           | BadMacroDefinition LineNum
           | BadIfPredicate
           | BadLineArgument LineNum P.String
           | IncludeDoesNotExist LineNum FilePath
           | FailedInclude LineNum FilePath
           | UserError LineNum P.String
           | UnknownCommand LineNum P.String
           | TooFewArgumentsToMacro LineNum P.String
           | BadMacroArguments LineNum P.String
           | NoInputFile
           | BadCommandLine P.String
           | RanOutOfInput
             deriving (Eq, Ord, Show)

-- | Hpp can raise various parsing errors.
class HasError m where
  throwError :: Error -> m a

instance Monad m => HasError (ExceptT Error m) where
  throwError = throwE
  {-# INLINE throwError #-}

instance (Monad m, HasHppState m) => HasHppState (ExceptT e m) where
  getState = lift getState
  {-# INLINE getState #-}
  setState = lift . setState
  {-# INLINE setState #-}

instance (Monad m, HasError m) => HasError (StateT s m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}

instance (Monad m, HasError m) => HasError (HppT t m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}

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
                         , hppEnv :: Env }

-- | A free monad construction to strictly delimit what capabilities
-- we need to perform pre-processing.
data HppF t r = ReadFile Int FilePath (t -> r)
              | ReadNext Int FilePath (t -> r)
              | WriteOutput t r

instance Functor (HppF t) where
  fmap f (ReadFile ln file k) = ReadFile ln file (f . k)
  fmap f (ReadNext ln file k) = ReadNext ln file (f . k)
  fmap f (WriteOutput o k) = WriteOutput o (f k)
  {-# INLINE fmap #-}

-- * Hpp Monad Transformer

-- | A free monad transformer specialized to HppF as the base functor.
newtype HppT t m a = HppT { runHppT :: m (FreeF (HppF t) a (HppT t m a)) }

writeOutput :: Monad m => t -> HppT t m ()
writeOutput = HppT . return . FreeF . flip WriteOutput (return ())
{-# INLINE writeOutput #-}

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

instance {-# OVERLAPS #-} Monad m => HasHppState (StateT HppState m) where
  getState = get
  {-# INLINE getState #-}
  setState = put
  {-# INLINE setState #-}

instance (Monad m, HasHppState m) => HasHppState (StateT s m) where
  getState = lift getState
  {-# INLINE getState #-}
  setState = lift . setState
  {-# INLINE setState #-}

instance (Monad m, HasHppState m) => HasHppState (HppT t m) where
  getState = lift getState
  {-# INLINE getState #-}
  setState = lift . setState
  {-# INLINE setState #-}

-- | An interpreter capability of threading a binding environment.
class HasEnv m where
  getEnv :: m Env
  setEnv :: Env -> m ()

instance (Monad m, HasHppState m) => HasEnv (HppT t m) where
  getEnv = fmap hppEnv (lift getState)
  {-# INLINE getEnv #-}
  setEnv e = lift getState >>= lift . setState . (\s -> s { hppEnv = e })
  {-# INLINE setEnv #-}

instance Monad m => HasEnv (StateT HppState m) where
  getEnv = hppEnv <$> get
  {-# INLINE getEnv #-}
  setEnv = (env .=)
  {-# INLINE setEnv #-}

instance Monad m => HasEnv (StateT Env m) where
  getEnv = get
  {-# INLINE getEnv #-}
  setEnv = put
  {-# INLINE setEnv #-}

instance (HasEnv m, Monad m) => HasEnv (ExceptT e m) where
  getEnv = lift getEnv
  {-# INLINE getEnv #-}
  setEnv = lift . setEnv
  {-# INLINE setEnv #-}

-- * Expansion

-- | Macro expansion involves treating tokens differently if they
-- appear in the original source or as the result of a previous macro
-- expansion. This distinction is used to prevent divergence by
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
          | Scan (Token String)
          | Rescan (Token String)
            deriving (Eq, Show)

-- * Macros

-- | There are object-like macros and function-like macros.
data Macro = Object [Token String]
           -- ^ An object-like macro is replaced with its definition
           | Function Int ([([Scan], String)] -> [Scan])
           -- ^ A function-like macro of some arity taks
           -- macro-expanded and raw versions of its arguments, then
           -- substitutes them into a body producing a new set of
           -- tokens.

instance Show Macro where
  show (Object ts) = "Object "++ toChars (detokenize ts)
  show (Function n _) = "Fun<"++show n++">"

-- | Looks up a 'Macro' in the current environment. If the 'Macro' is
-- found, the environment is juggled so that subsequent lookups of the
-- same 'Macro' may evaluate more quickly.
lookupMacro :: (HasEnv m, Monad m) => String -> m (Maybe Macro)
lookupMacro s = lookupKey s <$> getEnv
{-# INLINE lookupMacro #-}

-- * Nano-lens

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

setL :: Lens s a -> a -> s -> s
setL l x = runIdentity . l (const $ Identity x)
{-# INLINE setL #-}

getL :: Lens s a -> s -> a
getL l = getConstant . l Constant
{-# INLINE getL #-}

over :: Lens s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

-- * State Lenses

emptyHppState :: Config -> HppState
emptyHppState cfg = HppState cfg 1 emptyEnv

config :: Lens HppState Config
config f (HppState cfg ln e) = (\cfg' -> HppState cfg' ln e) <$> f cfg
{-# INLINE config #-}

lineNum :: Lens HppState LineNum
lineNum f (HppState cfg ln e) = (\ln' -> HppState cfg ln' e) <$> f ln
{-# INLINE lineNum #-}

env :: Lens HppState Env
env f (HppState cfg ln e) = (\e' -> HppState cfg ln e') <$> f e
{-# INLINE env #-}

use :: (HasHppState m, Functor m) => Lens HppState a -> m a
use l = getL l <$> getState
{-# INLINE use #-}

(.=) :: (HasHppState m, Monad m) => Lens HppState a -> a -> m ()
l .= x = getState >>= setState . setL l x
infix 4 .=
{-# INLINE (.=) #-}

(%=) :: (HasHppState m, Monad m) => Lens HppState a -> (a -> a) -> m ()
l %= f = getState >>= setState . over l f
infix 4 %=
{-# INLINE (%=) #-}
