{-# LANGUAGE LambdaCase #-}
-- | Streaming input and output.
module Hpp.Streamer (Streamer(..), StreamStep(..), Source, encase,
                     done, yield, yields, awaits, source, liftS,
                     nextOutput, run,
                     before, (~>), processPrefix, mapping, filtering, mapStream,
                     mappingMaybe, onDone, mapTil, flattenTil,
                     Chunky(..), metamorph) where
import Control.Applicative (Alternative(..))
import Control.Monad ((<=<))
import Data.Foldable (toList)
import Data.Void
import Hpp.Types (HasError(..), HasHppState(..), HasEnv(..))

-- * Streams of Steps

-- | Basic pipe.
data StreamStep r i o f = Await (i -> f) f
                        | Yield !o f
                        | Done (Maybe r)

instance Functor (StreamStep r i o) where
  fmap f (Await g d) = Await (f . g) (f d)
  fmap f (Yield o n) = Yield o (f n)
  fmap _ (Done r) = Done r
  {-# INLINE fmap #-}

-- | A stream of steps in a computational context.
newtype Streamer m i o r =
  Streamer { runStream :: m (StreamStep r i o (Streamer m i o r)) }

-- | A stream of steps that never awaits anything from upstream.
type Source m o r = Streamer m Void o r

-- | Package a step into a 'Streamer'
encase :: Monad m => StreamStep r i o (Streamer m i o r) -> Streamer m i o r
encase = Streamer . return
{-# INLINE encase #-}

instance Monad m => Functor (Streamer m i o) where
  fmap f (Streamer ma) = Streamer . flip fmap ma $ \case
    Await g d -> Await (fmap f . g) (fmap f d)
    Yield o n -> Yield o (fmap f n)
    Done r -> Done (fmap f r)
  {-# INLINE fmap #-}

instance Monad m => Applicative (Streamer m i o) where
  pure = Streamer . return . Done . Just
  {-# INLINE pure #-}
  Streamer ma <*> g = Streamer $ ma >>= \case
    Await f d -> return $ Await ((<*> g) . f) (d <*> g)
    Yield o n -> return $ Yield o (n <*> g)
    Done r -> maybe (runStream empty) (runStream . flip fmap g) r
  {-# INLINE (<*>) #-}

instance Monad m => Alternative (Streamer m r i) where
  empty = Streamer . return $ Done Nothing
  {-# INLINE empty #-}
  Streamer ma <|> b = Streamer . flip fmap ma $ \case
    Await g d -> Await ((<|> b) . g) (b <|> d)
    Yield o n -> Yield o (n <|> b)
    Done r -> Done r
  {-# INLINE (<|>) #-}

{-
-- This instance is quite often not really wanted due to associativity
-- issues.

instance Monad m => Monad (Streamer m r i) where
  return = pure
  {-# INLINE return #-}
  Streamer ma >>= fb = Streamer $ ma >>= \case
    Await f d -> return $ Await (\i -> f i >>= fb) (d >>= fb)
    Yield o n -> return $ Yield o (n >>= fb)
    Done r -> maybe (runStream empty) (runStream . fb) r
  {-# INLINE (>>=) #-}

instance Monad m => MonadPlus (Streamer m r i) where
  mzero = empty
  mplus = (<|>)
-}

instance (Monad m, HasError m) => HasError (Streamer m i o) where
  throwError = liftS . throwError
  {-# INLINE throwError #-}

instance (Monad m, HasHppState m) => HasHppState (Streamer m i o) where
  getState = liftS getState
  {-# INLINE getState #-}
  setState = liftS . setState
  {-# INLINE setState #-}

instance (Monad m, HasEnv m) => HasEnv (Streamer m i o) where
  getEnv = liftS getEnv
  {-# INLINE getEnv #-}
  setEnv = liftS . setEnv
  {-# INLINE setEnv #-}

-- * Builders

-- | Yield a value downstream, then finish.
yield :: Monad m => o -> Streamer m i o ()
yield o = encase $ Yield o (done ())
{-# INLINE yield #-}

-- | Yield a value then continue with another 'Streamer'.
yields :: Monad m => o -> Streamer m i o r -> Streamer m i o r
yields = (encase .) . Yield
{-# INLINE yields #-}

-- | Package a function that returns a 'Streamer' into a 'Streamer'.
awaits :: Monad m => (i -> Streamer m i o r) -> Streamer m i o r
awaits f = encase $ Await f empty
{-# INLINE awaits #-}

-- | The end of a stream.
done :: Monad m => r -> Streamer m i o r
done = pure
{-# INLINE done #-}

-- | Feed values downstream.
source :: (Monad m, Foldable f) => f a -> Streamer m i a ()
source = go . toList
  where go [] = done ()
        go (x:xs) = encase $ Yield x (go xs)
{-# INLINE source #-}

-- | Lift a monadic value into a 'Streamer'
liftS :: Functor m => m a -> Streamer m i o a
liftS = Streamer . fmap (Done . Just)
{-# INLINE liftS #-}

-- * Runners

-- | A source whose outputs have all been sunk may be run for its
-- effects and return value.
run :: Monad m => Source m Void r -> m (Maybe r)
run (Streamer s) = s >>= go
  where go (Done r) = return r
        go (Await _ _) = error "Source is awaiting in exhaustStreamer"
        go (Yield _ _) = error "A capped sink is yielding in exhaustStreamer"

-- | Compute the next step of a 'Streamer'.
nextOutput :: Monad m
           => Streamer m i o r -> m (Either (Maybe r) (o, Streamer m i o r))
nextOutput s = runStream s >>= go
  where go (Await _ n) = runStream n >>= go
        go (Yield o n) = return (Right (o, n))
        go (Done r) = return (Left r)

-- * Combinators

-- | Map a function over the values yielded by a stream.
mapStream :: Monad m => (a -> b) -> Streamer m i a r -> Streamer m i b r
mapStream f = go
  where go (Streamer s) = Streamer $ s >>= \case
          Done r -> pure $ Done r
          Await g e -> pure $ Await (go . g) (go e)
          Yield o n -> pure $ Yield (f o) (go n)
{-# INLINE[1] mapStream #-}

{-# RULES "hpp: mapStream/mapStream"
  forall f g s. mapStream f (mapStream g s) = mapStream (f . g) s #-}

-- | @upstream ~> downstream@ composes two streams such that values
-- flow from upstream to downstream.
(~>) :: Monad m => Streamer m a b r -> Streamer m b c r' -> Streamer m a c r'
src0 ~> Streamer mb = Streamer $ mb >>= goSnk src0
  where goSnk _ (Done r) = return $ Done r
        goSnk src (Yield o n) = return
                              $ Yield o (Streamer $ runStream n >>= goSnk src)
        goSnk src (Await f e) = runStream src >>= goSrc f e
        goSrc _ e (Done _) = runStream e >>= goSnk empty
        goSrc k _ (Yield i n) =  runStream (k i) >>= goSnk n
        goSrc k e (Await f' e') =
          return $ Await (\i -> Streamer $ runStream (f' i) >>= goSrc k e)
                         (e' ~> e)
{-# INLINE[1] (~>) #-}
infixl 9 ~>

_feedStreamer :: Monad m => Streamer m i o r -> [i] -> m ([i], [o], Maybe r)
_feedStreamer s xs0 = runStream s >>= aux xs0 id
  where aux [] acc (Await _ d) = runStream d >>= aux [] acc
        aux (x:xs) acc (Await f _) = runStream (f x) >>= aux xs acc
        aux xs acc (Yield o n) = runStream n >>= aux xs (acc . (o:))
        aux xs acc (Done r) = return (xs, acc [], r)

-- | @x `before` y@ runs @x@ to completion, discards its 'Done' value,
-- then becomes @y@.
before :: Monad m => Streamer m i o q -> Streamer m i o r -> Streamer m i o r
before (Streamer ma) mb = Streamer $ ma >>= go
  where go (Await f d) = return $ Await (\i -> Streamer $ runStream (f i) >>= go)
                                        (Streamer $ runStream d >>= go)
        go (Yield o n) = return $ Yield o (Streamer $ runStream n >>= go)
        go (Done _) = runStream mb

-- | Apply a function to the ending value of a stream.
onDone :: Monad m
       => (Maybe r -> Maybe r')
       -> Streamer m i o r
       -> Streamer m i o r'
onDone f (Streamer m) = Streamer $ m >>= go
  where go (Done r) = return $ Done (f r)
        go (Yield o n) = return $ Yield o (Streamer $ runStream n >>= go)
        go (Await f' e) = return $ Await (\i -> Streamer $ runStream (f' i) >>= go)
                                         (Streamer $ runStream e >>= go)
{-# INLINE[1] onDone #-}

{-# RULES "hpp: onDone/onDone"
  forall g f s. onDone g (onDone f s) = onDone (g . f) s #-}

-- | Apply a function to each value in a stream.
mapping :: Monad m => (a -> b) -> Streamer m a b r
mapping f = go
  where go = awaits (\i -> yields (f i) go)
        {-# INLINABLE go #-}
{-# INLINE[1] mapping #-}

{-# RULES "hpp: mapping/mapping"
  forall f g. mapping f ~> mapping g = mapping (g . f) #-}

-- | Discard all values that do not satisfy a predicate.
filtering :: Monad m => (a -> Bool) -> Streamer m a a r
filtering p = go
  where go = encase $ Await aux empty
        aux x = if p x then encase $ Yield x go else go
{-# INLINE[1] filtering #-}

-- | A combined filter and map.
mappingMaybe :: Monad m => (a -> Maybe b) -> Streamer m a b r
mappingMaybe f = go
  where go = awaits (\i -> maybe go (flip yields go) $ f i)
{-# INLINE[1] mappingMaybe #-}

predicateMap :: (a -> Bool) -> (a -> b) -> a -> Maybe b
predicateMap p f = \x -> if p x then Just (f x) else Nothing
{-# INLINE[1] predicateMap #-}

maybeNot :: (a -> Bool) -> Maybe a -> Maybe a
maybeNot p = \x -> case x of
                     Nothing -> Nothing
                     Just x' -> if p x' then x else Nothing
{-# INLINE[1] maybeNot #-}

{-# RULES "hpp: mapping ~> filtering"
  forall f p. mapping f ~> filtering p = mappingMaybe (maybeNot p . Just . f)
  ; "hpp: filtering ~> mapping"
  forall p f. filtering p ~> mapping f = mappingMaybe (predicateMap p f)
  ; "hpp: mappingMaybe ~> mappingMaybe"
  forall f g. mappingMaybe f ~> mappingMaybe g = mappingMaybe (f <=< g)
  #-}

-- | @processPrefix src snk@ is like '~>' except that when @snk@
-- finishes, the composite 'Streamer' becomes the remaining @src@.
processPrefix :: Monad m => Source m o r -> Streamer m o o r' -> Source m o r
processPrefix src0 snk = Streamer $ runStream snk >>= goSnk src0
  where goSnk src (Done _) = runStream src
        goSnk src (Yield o n) =
          return $ Yield o (Streamer $ runStream n >>= goSnk src)
        goSnk src (Await f _) = runStream src >>= goSrc f
        goSrc _ d@(Done _) = return d
        goSrc k (Yield i n) = runStream (k i) >>= goSnk n
        goSrc k (Await f e) =
          return $ Await (\i -> Streamer $ runStream (f i) >>= goSrc k)
                         (Streamer $ runStream e >>= goSrc k)

-- * Zoom support

-- | This is a left fold over a 'Streamer' with a final step to deal
-- with leftovers represented by whatever state the fold function
-- maintains.
common :: Monad m
       => (s -> Maybe (Streamer m i o ()))
       -> ((s -> Streamer m i o r -> Streamer m i' o' (Streamer m i o r))
            -> s -> Streamer m i o r -> Streamer m i' o' (Streamer m i o r))
       -> s
       -> Streamer m i o r -> Streamer m i' o' (Streamer m i o r)
common fin nxt = go
  where go acc src = encase $ Await (const (fin' acc src)) (nxt go acc src)
        fin' acc src = done . maybe src (`before` src) $ fin acc
{-# INLINABLE common #-}

-- | Flatten out chunks of inputs into individual values. The returned
-- 'Source' smuggles the remaining original 'Source' in an 'Await'
-- constructor, while the flattened source continues on with the
-- \"empty\" part of the 'Await' step. The upshot is that the value
-- may be used a regular 'Source', but it can also be swapped back
-- into the original 'Source'.
flattenTil :: Monad m
           => Source m [i] r -> Source m i (Source m [i] r)
flattenTil = common fin go []
  where fin acc = if null acc then Nothing else Just (yield acc)
        go k [] src = Streamer $ nextOutput src >>= \case
          Left r -> return $ Done (Just (encase (Done r)))
          -- Right (o,n) -> runStream $ go k o n
          Right (o,n) -> runStream $ go k o n
        go k (x:xs) src = encase $ Yield x (k xs src)
{-# INLINABLE flattenTil #-}

-- | See 'flattenTil' for an explanation.
mapTil :: Monad m
       => (a -> b)
       -> Streamer m Void a r
       -> Streamer m Void b (Streamer m Void a r)
mapTil f = common (const Nothing) go ()
  where go k () src = Streamer $ nextOutput src >>= \case
          Left r -> return $ Done (Just (encase (Done r)))
          Right (o, n) -> return $ Yield (f o) (k () n)
{-# INLINABLE mapTil #-}

-- * Chunky metamorphisms

-- | A function that produces an output stream that finishes with
-- another such function. Think of the input to this function as
-- coming from upstream, while the closure of the streamed output may
-- be used to thread state.
newtype Chunky m a b = Chunky (a -> Source m b (Chunky m a b))

-- | Apply a function to a 'Chunky''s output.
chunkMap :: Monad m => (b -> c) -> Chunky m a b -> Chunky m a c
chunkMap g (Chunky f) = Chunky (onDone (fmap (chunkMap g)) . mapStream g . f)
{-# INLINE[1] chunkMap #-}

{-# RULES "hpp: chunkMap/chunkMap"
  forall f g c. chunkMap f (chunkMap g c) = chunkMap (f . g) c #-}

-- | Apply a function to a 'Chunky's input.
chunkConmap :: Monad m => (a -> b) -> Chunky m b c -> Chunky m a c
chunkConmap g (Chunky f) = Chunky (onDone (fmap (chunkConmap g)) . f . g)
{-# INLINE[1] chunkConmap #-}

{-# RULES "hpp: chunkConmap/chunkConmap"
  forall f g c. chunkConmap f (chunkConmap g c) = chunkConmap (f . g) c #-}

-- | This is something like a composition of an unfold with a fold. We
-- fold the upstream values into some state carried by a 'Chunky',
-- then unfold that state in the 'Chunky''s output stream.
metamorph :: Monad m
          => Chunky m a b
          -> Streamer m a b ()
metamorph (Chunky f) = go
  where go = awaits (Streamer . aux . f)
        aux s = runStream s >>= \case
          Done (Just k) -> runStream $ metamorph k
          Done Nothing -> return $ Done Nothing
          Await _ _ -> error "Sources shouldn't await"
          Yield o n -> return $ Yield o (Streamer $ aux n)
{- INLINABLE metamorph #-}
-- inlining metamorph trips a bug in GHC-7.10.2

{-# RULES
    "hpp: metamorph/mapping"
    forall c f. metamorph c ~> mapping f = metamorph (chunkMap f c)
  ; "hpp: mapping/metamorph"
    forall c f. mapping f ~> metamorph c = metamorph (chunkConmap f c)
  #-}
