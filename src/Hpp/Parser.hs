{-# LANGUAGE LambdaCase, Rank2Types, TupleSections, ScopedTypeVariables #-}
-- | Parsers over streaming input.
module Hpp.Parser (Parser, ParserT, parse, evalParse, await, awaitJust, replace,
                   droppingWhile, precede, takingWhile, onChunks, onElements,
                   insertInputSegment, onIsomorphism, runParser) where
import Control.Arrow (second, (***))
import Control.Monad.Trans.State.Strict
import Hpp.Types (HasError(..), Error(UserError))
import Control.Monad.Trans.Class (lift)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))

-- * Parsers

type RopeM m a = [Either (m ()) a]

-- | A 'Parser' is a bit of state carrying a source of input.
type ParserT m src i = StateT (Headspring m src i, src) m

type Parser m i = ParserT m (RopeM m [i]) i

data Headspring m src i =
  Headspring { hsAwait :: src -> m (Maybe (i, src))
             , hsPrecede :: [i] -> src -> src }

-- | Pop the head non-effect element from a list.
unconsM :: Applicative m => RopeM m a -> m (Maybe (a, RopeM m a))
unconsM [] = pure Nothing
unconsM (Left m : ms) = m *> unconsM ms
unconsM (Right x : ms) = pure (Just (x, ms))

-- | Pop the first non-null, non-effect element from a list.
unconsMNonEmpty :: Monad m => RopeM m [a] -> m (Maybe (NonEmpty a, RopeM m [a]))
unconsMNonEmpty r = unconsM r >>= \case
  Nothing -> pure Nothing
  Just ([], rst) -> unconsMNonEmpty rst
  Just (x:xs, rst) -> return (Just (x :| xs, rst))

unconsSpring :: Monad m => Headspring m (RopeM m [i]) i
unconsSpring = Headspring aw ropePrecede
  where aw r = unconsMNonEmpty r >>= \case
          Nothing -> return Nothing
          Just (x :| xs, r') -> return (Just (x, Right xs : r'))

flattenSpring :: Monad m => Headspring m  (RopeM m [[i]]) i
flattenSpring = Headspring aw pr
  where aw r = unconsMNonEmpty r >>= \case
          Nothing -> return Nothing
          Just ([] :| ys, r') -> aw (Right ys : r')
          Just ((x:xs) :| ys, r') -> return (Just (x, Right (xs:ys) : r'))
        pr xs [] = [Right [xs]]
        pr xs (Right (ys:zs) : ms) = Right ((xs++ys) : zs) : ms
        pr xs (Right [] : ms) = Right [xs] : ms
        pr xs ms@(Left _ : _) = Right [xs] : ms

chunkSpring :: (Monoid src, Applicative m) => Headspring m (RopeM m src) src
chunkSpring = Headspring unconsM pr
  where pr xs [] = [Right (mconcat xs)]
        pr xs (Right ys : ms) = Right (mconcat xs <> ys) : ms
        pr xs ms@(Left _ : _) = Right (mconcat xs) : ms

await :: Monad m => ParserT m src i (Maybe i)
await = do (hs, st) <- get
           lift (hsAwait hs st) >>= \case
             Nothing -> return Nothing
             Just (x,st') -> Just x <$ put (hs,st')
{-# INLINE await #-}

-- | Push a value back into a parser's source.
replace :: (Monad m) => i -> ParserT m src i ()
replace = precede . pure

ropePrecede :: [i] -> RopeM m [i] -> RopeM m [i]
ropePrecede xs [] = [Right xs]
ropePrecede xs ms@(Left _ : _) = Right xs : ms
ropePrecede xs (Right ys : ms) = Right (xs++ys) : ms

-- | Push a stream of values back into a parser's source.
precede :: Monad m => [i] -> ParserT m src i ()
precede xs = do (hs,st) <- get
                put (hs, hsPrecede hs xs st)
{-# INLINE precede #-}

-- | Run a 'Parser' with a given input stream.
parse :: Monad m => Parser m i o -> [i] -> m (o, RopeM m [i])
parse m xs = second snd <$> runStateT m (unconsSpring, [Right xs])
{-# INLINE parse #-}

runParser :: Monad m => Parser m i o -> RopeM m [i] -> m (o, RopeM m [i])
runParser m xs = second snd <$> runStateT m (unconsSpring, xs)

evalParse :: Monad m => Parser m i o -> [i] -> m o
evalParse m xs = evalStateT m (unconsSpring, [Right xs])

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

insertInputSegment :: Monad m => src -> m () -> ParserT m (RopeM m src) i ()
insertInputSegment xs k = modify' (second ([Right xs, Left k]++))

-- * Parser Transformations

onChunks :: Monad m => ParserT m (RopeM m [i]) [i] r -> Parser m i r
onChunks m = do (hs,st) <- get
                (r, (_,st')) <- lift (runStateT m (chunkSpring, st))
                r <$ put (hs,st')

onElements :: Monad m => ParserT m (RopeM m [[i]]) i r -> Parser m [i] r
onElements m = do (hs,st) <- get
                  (r, (_,st')) <- lift (runStateT m (flattenSpring, st))
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
         aw ([], src) = fmap (fmap (fwd *** ([],))) (hsAwait hs src)
         aw ((b:bs), src) = return (Just (b, (bs,src)))
         pr xs (bs,src) = (xs++bs, src)
         mappedSpring = Headspring aw pr
     (r, (_, (bs, st'))) <- lift (runStateT m (mappedSpring, ([], st)))
     r <$ put (hs, hsPrecede hs (mapMaybe bwd bs) st')
{-# INLINE onIsomorphism #-}
