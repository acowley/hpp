{-# LANGUAGE DeriveFunctor, LambdaCase, Rank2Types,
             ScopedTypeVariables, TupleSections #-}
-- | Parsers over streaming input.
module Hpp.Parser (Parser, ParserT, parse, evalParse, await, awaitJust, replace,
                   droppingWhile, precede, takingWhile, onChunks, onElements,
                   onInputSegment, insertInputSegment, onIsomorphism,
                   runParser) where
import Control.Arrow (second, (***))
import Control.Monad.Trans.State.Strict
import Hpp.Types (HasError(..), Error(UserError))
import Control.Monad.Trans.Class (lift)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))

-- * Parsers

-- | A single pre-processor input is either an action or a value
data InputItem m a = Action (m ()) | Value a deriving Functor

-- | Our input is a list of values each of which is either an action
-- or a value.
type Input m a = [InputItem m a]

-- | Functions for working with input sources.
data Source m src i =  Source { srcAwait :: src -> m (Maybe (i, src))
                              , srcPrecede :: [i] -> src -> src }

-- data Source m i where
--   Source :: { srcSrc :: src
--             , srcAwait :: src -> m (Maybe (i, src))
--             , srcPrecede :: [i] -> src -> src } -> Source m i


-- | A 'ParserT' is a bit of state that carries a source of input.
type ParserT m src i = StateT (Source m src i, src) m

-- class CanParse i m where
--   getSource :: m (Source m src i)
--   setSource :: src -> m ()

-- srcAwait' :: CanParse i m => m (Maybe i)
-- srcAwait' = do s <- getSource
--                x <- srcAwait s (srcSrc src)
--                mapM_ (\(i,src') -> i <$ setSource src')


-- | A 'Parser' is a bit of state that carries a source of input
-- consisting of a list of values which are either actions in an
-- underlying monad or sequences of inputs. Thus we have chunks of
-- input values with interspersed effects.
type Parser m i = ParserT m (Input m [i]) i

-- | Pop the head non-effect element from a list.
unconsM :: Applicative m => Input m a -> m (Maybe (a, Input m a))
unconsM [] = pure Nothing
unconsM (Action m : ms) = m *> unconsM ms
unconsM (Value x : ms) = pure (Just (x, ms))

-- | Pop the first non-null, non-effect element from a list.
unconsMNonEmpty :: Monad m => Input m [a] -> m (Maybe (NonEmpty a, Input m [a]))
unconsMNonEmpty r = unconsM r >>= \case
  Nothing -> pure Nothing
  Just ([], rst) -> unconsMNonEmpty rst
  Just (x:xs, rst) -> return (Just (x :| xs, rst))

unconsSource :: Monad m => Source m (Input m [i]) i
unconsSource = Source aw ropePrecede
  where aw r = unconsMNonEmpty r >>= \case
          Nothing -> return Nothing
          Just (x :| xs, r') -> return (Just (x, Value xs : r'))

flattenSource :: Monad m => Source m (Input m [[i]]) i
flattenSource = Source aw pr
  where aw r = unconsMNonEmpty r >>= \case
          Nothing -> return Nothing
          Just ([] :| ys, r') -> aw (Value ys : r')
          Just ((x:xs) :| ys, r') -> return (Just (x, Value (xs:ys) : r'))
        pr xs [] = [Value [xs]]
        pr xs (Value (ys:zs) : ms) = Value ((xs++ys) : zs) : ms
        pr xs (Value [] : ms) = Value [xs] : ms
        pr xs ms@(Action _ : _) = Value [xs] : ms

chunkSource :: (Monoid src, Applicative m) => Source m (Input m src) src
chunkSource = Source unconsM pr
  where pr xs [] = [Value (mconcat xs)]
        pr xs (Value ys : ms) = Value (mconcat xs <> ys) : ms
        pr xs ms@(Action _ : _) = Value (mconcat xs) : ms

await :: Monad m => ParserT m src i (Maybe i)
await = do (hs, st) <- get
           lift (srcAwait hs st) >>= \case
             Nothing -> return Nothing
             Just (x,st') -> Just x <$ put (hs,st')
{-# INLINE await #-}

-- | Push a value back into a parser's source.
replace :: (Monad m) => i -> ParserT m src i ()
replace = precede . pure

ropePrecede :: [i] -> Input m [i] -> Input m [i]
ropePrecede xs [] = [Value xs]
ropePrecede xs ms@(Action _ : _) = Value xs : ms
ropePrecede xs (Value ys : ms) = Value (xs++ys) : ms

-- | Push a stream of values back into a parser's source.
precede :: Monad m => [i] -> ParserT m src i ()
precede xs = do (hs,st) <- get
                put (hs, srcPrecede hs xs st)
{-# INLINE precede #-}

-- | Run a 'Parser' with a given input stream.
parse :: Monad m => Parser m i o -> [i] -> m (o, Input m [i])
parse m xs = second snd <$> runStateT m (unconsSource, [Value xs])
{-# INLINE parse #-}

runParser :: Monad m => Parser m i o -> Input m [i] -> m (o, Input m [i])
runParser m xs = second snd <$> runStateT m (unconsSource, xs)

evalParse :: Monad m => Parser m i o -> [i] -> m o
evalParse m xs = evalStateT m (unconsSource, [Value xs])

-- * Operations on Parsers

-- | 'awaitP' that throws an error with the given message if no more
-- input is available. This may be used to locate where in a
-- processing pipeline input was unexpectedly exhausted.
awaitJust :: (Monad m, HasError m) => String -> ParserT m src i i
awaitJust s = await >>= maybe (lift $ throwError err) return
  where err = UserError 0 ("awaitJust: " ++ s)
{-# INLINE awaitJust #-}

-- | Discard all values until one fails to satisfy a predicate. At
-- that point, the failing value is 'replace'd, and the
-- 'droppingWhile' stream stops.
droppingWhile :: (Monad m) => (i -> Bool) -> ParserT m src i ()
droppingWhile p = go
  where go = await >>= \case
               Nothing -> return ()
               Just x -> if p x then go else replace x
{-# INLINE droppingWhile #-}

-- | Echo all values until one fails to satisfy a predicate. At that
-- point, the failing value is 'replace'd, and the 'takingWhile'
-- stream stops.
takingWhile :: (Monad m) => (i -> Bool) -> ParserT m src i [i]
takingWhile p = go id
  where go acc = await >>= \case
                   Nothing -> return (acc [])
                   Just x
                     | p x -> go (acc . (x:))
                     | otherwise -> replace x >> return (acc [])
{-# INLINE takingWhile #-}

insertInputSegment :: Monad m => src -> m () -> ParserT m (Input m src) i ()
insertInputSegment xs k = modify' (second ([Value xs, Action k]++))

onInputSegment :: Monad m => (src -> src) -> ParserT m (Input m src) i ()
onInputSegment f = do (hs,st) <- get
                      case st of
                        [] -> return ()
                        (Value xs : ys) -> put (hs, Value (f xs) : ys)
                        (Action m : xs) -> lift m >> put (hs,xs) >> onInputSegment f
{-# INLINABLE onInputSegment #-}

-- * Parser Transformations

onChunks :: Monad m => ParserT m (Input m [i]) [i] r -> Parser m i r
onChunks m = do (hs,st) <- get
                (r, (_,st')) <- lift (runStateT m (chunkSource, st))
                r <$ put (hs,st')

onElements :: Monad m => ParserT m (Input m [[i]]) i r -> Parser m [i] r
onElements m = do (hs,st) <- get
                  (r, (_,st')) <- lift (runStateT m (flattenSource, st))
                  let onHead _ [] = []
                      onHead f (x:xs) = f x : xs
                  r <$ put (hs, onHead (fmap (dropWhile null)) st')
{-# INLINE onElements #-}

onIsomorphism :: forall m a b src r. Monad m
              => (a -> b) -> (b -> Maybe a)
              -> ParserT m ([b],src) b r
              -> ParserT m src a r
onIsomorphism fwd bwd m =
  do (hs,st) <- get
     let aw :: ([b],src) -> m (Maybe (b, ([b], src)))
         aw ([], src) = fmap (fmap (fwd *** ([],))) (srcAwait hs src)
         aw ((b:bs), src) = return (Just (b, (bs,src)))
         pr xs (bs,src) = (xs++bs, src)
         mappedSpring = Source aw pr
     (r, (_, (bs, st'))) <- lift (runStateT m (mappedSpring, ([], st)))
     r <$ put (hs, srcPrecede hs (mapMaybe bwd bs) st')
{-# INLINE onIsomorphism #-}
