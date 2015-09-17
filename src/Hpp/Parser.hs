{-# LANGUAGE BangPatterns, LambdaCase, RankNTypes #-}
-- | Parsers over streaming input.
module Hpp.Parser (Parser(..), parse,
                   awaitP, awaitJust, replace, droppingWhile, liftP,
                   onParserSource, precede, takingWhile,
                   zoomParse, zoomParseChunks) where
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.State.Strict
import Hpp.Streamer (Source, Streamer(..), yield, before, processPrefix,
                     nextOutput, flattenTil, StreamStep(..))
import Hpp.Types (HasError(..), HasHppState(..), Error(UserError))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- * Parsers

type ParserR m r i = StateT (Source m i r) m

-- | A 'Parser' is a 'Streamer' whose monadic context is a bit of
-- state carrying a source input stream.
newtype Parser m i o = Parser { runParser :: forall r. ParserR m r i o }

-- | Run a 'Parser' with a given input stream.
parse :: Monad m => Parser m i o -> Source m i r -> m o
parse (Parser m) s = evalStateT m s
{-# INLINE parse #-}

instance Functor m => Functor (Parser m i) where
  fmap f (Parser p) = Parser (fmap f p)
  {-# INLINE fmap #-}

instance Monad m => Applicative (Parser m i) where
  pure x = Parser (pure x)
  {-# INLINE pure #-}
  Parser f <*> Parser x = Parser (f <*> x)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (Parser m i) where
  return = pure
  {-# INLINE return #-}
  Parser ma >>= fb = Parser $ ma >>= runParser . fb
  {-# INLINE (>>=) #-}

instance MonadPlus m => Alternative (Parser m i) where
  empty = Parser empty
  {-# INLINE empty #-}
  Parser a <|> Parser b = Parser (a <|> b)
  {-# INLINE (<|>) #-}

instance (Monad m, HasError m) => HasError (Parser m i) where
  throwError = liftP . throwError
  {-# INLINE throwError #-}

instance (Monad m, HasHppState m) => HasHppState (Parser m i) where
  getState = liftP getState
  setState = liftP . setState

instance MonadIO m => MonadIO (Parser m i) where
  liftIO = liftP . liftIO

-- * Operations on Parsers          

-- | Lift a monadic action into a Parser.
liftP :: Monad m => m o -> Parser m i o
liftP m = Parser (lift m)
{-# INLINE liftP #-}

-- | @onParserSource proc@ feeds the 'Parser' source through @proc@
-- using 'processPrefix'. This means that when @proc@ finishes, the
-- remaining source continues unmodified.
onParserSource :: Monad m => Streamer m i i () -> Parser m i ()
onParserSource s = Parser (modify' (flip processPrefix s))
-- onParserSource s = Parser (get >>= lift . flip processPrefix s >>= put)
{-# INLINE onParserSource #-}

-- | Waits for a value from upstream. Returns 'Nothing' if upstream is
-- empty.
awaitP :: Monad m => Parser m i (Maybe i)
awaitP = Parser $ get >>= lift . nextOutput >>= \case
  Left _ -> put empty >> return Nothing
  Right !(!i, !src') -> put src' >> return (Just i)
{-# INLINABLE awaitP #-}

-- | 'awaitP' that throws an error with the given message if no more
-- input is available. This may be used to locate where in a
-- processing pipeline input was unexpectedly exhausted.
awaitJust :: (Monad m, HasError m) => String -> Parser m i i
awaitJust s = awaitP >>= maybe (liftP $ throwError err) return
  where err = UserError 0 ("awaitJust: " ++ s)

-- | Push a value back into a parser's source.
replace :: Monad m => i -> Parser m i ()
replace x = Parser $ modify' (before (yield x))
{-# INLINE replace #-}

-- | Push a stream of values back into a parser's source.
precede :: Monad m => Source m i r -> Parser m i ()
precede m = Parser (modify' (before m))
{-# INLINE precede #-}

-- | Discard all values until one fails to satisfy a predicate. At
-- that point, the failing value is 'replace'd, and the
-- 'droppingWhile' stream stops.
droppingWhile :: Monad m => (i -> Bool) -> Parser m i ()
droppingWhile p = go
  where go = awaitP >>= \case
               Nothing -> return ()
               Just x -> if p x then go else replace x
{-# INLINE droppingWhile #-}

-- | Echo all values until one fails to satisfy a predicate. At that
-- point, the failing value is 'replace'd, and the 'takingWhile'
-- stream stops.
takingWhile :: Monad m => (i -> Bool) -> Parser m i [i]
takingWhile p = go id
  where go acc = awaitP >>= \case
                   Nothing -> return (acc [])
                   Just x
                     | p x -> go (acc . (x:))
                     | otherwise -> replace x >> return (acc [])
{-# INLINE takingWhile #-}

-- * Zooming

-- | This is rather like a Lens zoom, but quite fragile. The idea is
-- that we run a 'Parser' on a transformation of the original
-- source. The transformation of the source is responsible for
-- yielding transformed values, and ending /on demand/ with the rest
-- of the original source. We additionally scoop up any leftover
-- transformed values and prepend them onto the remaining source after
-- inverting the original transformation.
zoomParse :: Monad m
          => (forall r. Source m a r -> Source m b (Source m a r))
          -> Parser m b o
          -> Parser m a o
zoomParse f (Parser p) = Parser $ do
  src <- get
  (r, src') <- lift $ runStateT p (f src)
  lift (runStream src') >>= \case
    Await k _ -> lift (runStream (k undefined)) >>= \case
                   Done (Just src'') -> r <$ put src''
                   _ -> error "zoomParse blew it"
    Done (Just src'') -> r <$ put src''
    Done Nothing -> r <$ put empty
    Yield _ _ -> error "zoomParse blew it by yielding"
{-# INLINABLE zoomParse #-}

-- | Turn a 'Parser' on individual values into a 'Parser' on chunks.
zoomParseChunks :: Monad m => Parser m i r -> Parser m [i] r
zoomParseChunks = zoomParse flattenTil
{-# INLINE zoomParseChunks #-}

