{-# LANGUAGE TupleSections #-}
-- | A name binding context, or environment.
module Hpp.Env where
import Data.ByteString (ByteString)
import qualified Data.Trie as T

emptyEnv :: T.Trie a
emptyEnv = T.empty

insertPair :: (ByteString, a) -> T.Trie a -> T.Trie a
insertPair = uncurry T.insert

deleteKey :: ByteString -> T.Trie a -> T.Trie a
deleteKey = T.delete

-- lookupKey :: L.ByteString -> T.Trie a -> Maybe (a, T.Trie a)
-- lookupKey k t = (,t) <$> T.lookup (L.toStrict k) t
lookupKey :: ByteString -> T.Trie a -> Maybe a
lookupKey = T.lookup


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
