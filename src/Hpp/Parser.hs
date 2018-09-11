{-# LANGUAGE DeriveFunctor, LambdaCase, TupleSections #-}
-- | Parsers over streaming input.
module Hpp.Parser (Parser, ParserT, evalParse, await, awaitJust, replace,
                   droppingWhile, precede, takingWhile, onElements,
                   onInputSegment, insertInputSegment, onIsomorphism) where
import Control.Arrow ((***))
import Control.Monad.Trans.State.Strict
import Hpp.Types (HasError(..), Error(UserError))
import Control.Monad.Trans.Class (lift)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)

-- * Parsers

-- | A single pre-processor input is either an action or a value
data InputItem m a = Action (m ()) | Value a deriving Functor

-- | Our input is a list of values each of which is either an action
-- or a value.
type Input m a = [InputItem m a]

-- | Functions for working with input sources.
data Source m src i =  Source { srcSrc :: src
                              , _srcAwait :: src -> m (Maybe (i, src))
                              , _srcPrecede :: [i] -> src -> src }

-- | A 'ParserT' is a bit of state that carries a source of input.
type ParserT m src i = StateT (Source m src i) m

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

unconsSource :: Monad m => Input m [i] -> Source m (Input m [i]) i
unconsSource src = Source src aw ropePrecede
  where aw r = unconsMNonEmpty r >>= \case
          Nothing -> return Nothing
          Just (x :| xs, r') -> return (Just (x, Value xs : r'))

flattenSource :: Monad m => Source m (Input m [[i]]) [i] -> Source m (Input m [[i]]) i
flattenSource (Source src0 aw pr) = Source src0 aw' pr'
  where aw' src = aw src >>= \case
          Nothing -> return Nothing
          Just ([], src') -> aw' src'
          Just (x:xs, src') -> return (Just (x, pr' xs src'))
        pr' xs src = pr [xs] src

await :: Monad m => ParserT m src i (Maybe i)
await = do Source src aw pr <- get
           lift (aw src) >>= \case
             Nothing -> return Nothing
             Just (x, src') -> Just x <$ put (Source src' aw pr)
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
precede xs = do Source src aw pr <- get
                put (Source (pr xs src) aw pr)
{-# INLINE precede #-}

-- | Evaluate a 'Parser' with a given input stream.
evalParse :: Monad m => Parser m i o -> [i] -> m o
evalParse m xs = evalStateT m (unconsSource [Value xs])

-- * Operations on Parsers

-- | 'await' that throws an error with the given message if no more
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
insertInputSegment xs k =
  modify' (\s -> s { srcSrc = [Value xs, Action k] ++ srcSrc s})

onInputSegment :: Monad m => (src -> src) -> ParserT m (Input m src) i ()
onInputSegment f =
  do Source src aw pr <- get
     case src of
       [] -> return ()
       (Value xs : ys) -> put (Source (Value (f xs) : ys) aw pr)
       (Action m : xs) -> lift m >> put (Source xs aw pr) >> onInputSegment f
{-# INLINABLE onInputSegment #-}

-- * Parser Transformations

-- | A parser on lists of things can embed a parser on things. For
-- example, if we have a parser on lists of words, we can embed a
-- parser on individual words.
onElements :: Monad m => ParserT m (Input m [[i]]) i r -> Parser m [i] r
onElements m = do s@(Source _ aw pr) <- get
                  (r, Source src' _ _) <- lift (runStateT m (flattenSource s))
                  r <$ put (Source (onHead (fmap (dropWhile null)) src') aw pr)
  where onHead _ [] = []
        onHead f (x:xs) = f x : xs
{-# INLINE onElements #-}

-- | Given a function with type @a -> b@, and a partial inverse, @b ->
-- Maybe a@, we can embed a parser on values of type @b@ in a parser
-- on values of type @a@.
onIsomorphism :: Monad m
              => (a -> b)
              -> (b -> Maybe a)
              -> ParserT m ([b],src) b r
              -> ParserT m src a r
onIsomorphism fwd bwd m =
  do Source src aw pr <- get
     let aw' ([], src') = fmap (fmap (fwd *** ([],))) (aw src')
         aw' ((b:bs), src') = return (Just (b, (bs,src')))
         pr' xs (bs, src') = (xs++bs, src')
     (r, Source (bs, src') _ _) <- lift (runStateT m (Source ([],src) aw' pr'))
     r <$ put (Source (pr (mapMaybe bwd bs) src') aw pr)
{-# INLINE onIsomorphism #-}
