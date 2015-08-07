{-# LANGUAGE LambdaCase #-}
-- | The core types involved used by the pre-processor.
module Hpp.Types where
import Hpp.Config
import Hpp.Tokens

-- | Line numbers are represented as 'Int's
type LineNum = Int

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
             deriving (Eq, Ord, Show)

-- * Pre-processor Actions

-- | A free monad construction to strictly delimit what capabilities
-- we need to perform pre-processing.
data Hpp a = Pure a
           | ReadFile Int FilePath (String -> Hpp a)
           | ReadNext Int FilePath (String -> Hpp a)
           | GetConfig (Config -> Hpp a)
           | SetConfig Config (Hpp a)

instance Functor Hpp where
  fmap f (Pure a) = Pure (f a)
  fmap f (ReadFile ln file k) = ReadFile ln file (fmap f . k)
  fmap f (ReadNext ln file k) = ReadNext ln file (fmap f . k)
  fmap f (GetConfig k) = GetConfig (fmap f . k)
  fmap f (SetConfig cfg k) = SetConfig cfg (fmap f k)

instance Applicative Hpp where
  pure = Pure
  Pure f <*> Pure x = Pure (f x)
  Pure f <*> ReadFile ln file k = ReadFile ln file (fmap f . k)
  Pure f <*> ReadNext ln file k = ReadNext ln file (fmap f . k)
  Pure f <*> GetConfig k = GetConfig (fmap f . k)
  Pure f <*> SetConfig cfg k = SetConfig cfg (fmap f k)
  ReadFile ln file k <*> x = ReadFile ln file ((<*> x) . k)
  ReadNext ln file k <*> x = ReadNext ln file ((<*> x) . k)
  GetConfig k <*> x = GetConfig ((<*> x) . k)
  SetConfig cfg k <*> x = SetConfig cfg (k <*> x)

instance Monad Hpp where
  return = pure
  Pure x >>= f = f x
  ReadFile ln file k >>= f = ReadFile ln file ((>>= f) . k)
  ReadNext ln file k >>= f = ReadNext ln file ((>>= f) . k)
  GetConfig k >>= f = GetConfig ((>>= f) . k)
  SetConfig cfg k >>= f = SetConfig cfg (k >>= f)

-- | An 'Hpp' action that can fail.
newtype ErrHpp a = ErrHpp { runErrHpp :: Hpp (Either (FilePath,Error) a) }

instance Functor ErrHpp where
  fmap f = ErrHpp . fmap (fmap f) . runErrHpp

instance Applicative ErrHpp where
  pure = ErrHpp . pure . pure
  ErrHpp f <*> ErrHpp x =
    ErrHpp $
    do f >>= \case
         Left err -> return (Left err)
         Right f' -> do x >>= \case
                          Left err' -> return (Left err')
                          Right x' -> return (Right $ f' x')

instance Monad ErrHpp where
  return = pure
  ErrHpp x >>= fb = ErrHpp $ do x >>= \case
                                  Left err -> return (Left err)
                                  Right x' -> runErrHpp (fb x')

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
            deriving Show

-- | A difference list is a list representation with @O(1)@ @snoc@'ing at
-- the end of the list.
type DList a = [a] -> [a]

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
