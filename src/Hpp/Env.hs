-- | A name binding context, or environment.
module Hpp.Env where
import Hpp.Types (Macro)

-- | A macro binding environment.
type Env = [(String, Macro)]

-- | Delete an entry from an association list.
deleteKey :: Eq a => a -> [(a,b)] -> [(a,b)]
deleteKey k = go
  where go [] = []
        go (h@(x,_) : xs) = if x == k then xs else h : go xs

-- | Looks up a value in an association list. If the key is found, the
-- value is returned along with an updated association list with that
-- key at the front.
lookupKey :: Eq a => a -> [(a,b)] -> Maybe (b, [(a,b)])
lookupKey k = go id
  where go _ [] = Nothing
        go acc (h@(x,v) : xs)
          | k == x = Just (v, h : acc [] ++ xs)
          | otherwise = go (acc . (h:)) xs
