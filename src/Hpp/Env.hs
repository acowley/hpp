{-# LANGUAGE TupleSections #-}
-- | A name binding context, or environment.
module Hpp.Env where
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

emptyEnv :: HashMap ByteString a
emptyEnv = HashMap.empty

insertPair :: (ByteString, a) -> HashMap ByteString a -> HashMap ByteString a
insertPair = uncurry HashMap.insert

deleteKey :: ByteString -> HashMap ByteString a -> HashMap ByteString a
deleteKey = HashMap.delete

lookupKey :: ByteString -> HashMap ByteString a -> Maybe a
lookupKey = HashMap.lookup

{-
import qualified Data.Map as M

emptyEnv :: M.Map String a
emptyEnv = M.empty
{-# INLINE emptyEnv #-}

insertPair :: (String, a) -> M.Map String a -> M.Map String a
insertPair = uncurry M.insert
{-# INLINE insertPair #-}

deleteKey :: String -> M.Map String a -> M.Map String a
deleteKey = M.delete
{-# INLINE deleteKey #-}

lookupKey :: String -> M.Map String a -> Maybe (a, M.Map String a)
lookupKey k m = fmap (\x -> (x, m)) (M.lookup k m)
{-# INLINE lookupKey #-}
-}
{-
-- | An empty binding environment.
emptyEnv :: [a]
emptyEnv = []
{-# INLINE emptyEnv #-}

-- | Add a @(key,value)@ pair to an environment.
insertPair :: a -> [a] -> [a]
insertPair = (:)
{-# INLINE insertPair #-}

-- | Delete an entry from an association list.
deleteKey :: Eq a => a -> [(a,b)] -> [(a,b)]
deleteKey k = go
  where go [] = []
        go (h@(x,_) : xs) = if x == k then xs else h : go xs
{-# INLINE deleteKey #-}

-- | Looks up a value in an association list. If the key is found, the
-- value is returned along with an updated association list with that
-- key at the front.
lookupKey :: Eq a => a -> [(a,b)] -> Maybe (b, [(a,b)])
lookupKey k = go id
  where go _ [] = Nothing
        go acc (h@(x,v) : xs)
          | k == x = Just (v, h : acc [] ++ xs)
          | otherwise = go (acc . (h:)) xs
{-# INLINE lookupKey #-}
-}
