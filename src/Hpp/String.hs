{-# LANGUAGE BangPatterns #-}
-- | HELPERS for working with 'String's
module Hpp.String (stringify, unquote, trimSpaces, breakOn, cons) where
import Data.Char (isSpace)
import Data.List (isPrefixOf, find)

-- | Stringification puts double quotes around a string and
-- backslashes before existing double quote characters and backslash
-- characters.
stringify :: String -> String
stringify s = '"' : concatMap aux (strip s) ++ "\""
  where aux '\\' = "\\\\"
        aux '"' = "\\\""
        aux c = [c]
        strip = trimSpaces . dropWhile isSpace

-- | Remove double quote characters from the ends of a string.
unquote :: String -> String
unquote ('"':xs) = go xs
  where go ['"'] = []
        go [] = []
        go (c:cs) = c : go cs
unquote xs = xs

-- | Trim trailing spaces from a 'String'
trimSpaces :: String -> String
trimSpaces = trimEnd isSpace

-- | Remove a suffix of a list all of whose elements satisfy the given
-- predicate.
trimEnd :: (a -> Bool) -> [a] -> [a]
trimEnd p = go id
  where go _ [] = []
        go acc (c:cs)
          | p c = go (acc . (c:)) cs
          | otherwise = acc (c : go id cs)

-- | Similar to the function of the same name in the @text@ package.
--
-- @breakOn needles haystack@ finds the first instance of an element
-- of @needles@ in @haystack@. The first component of the result is
-- the needle tag, the second component is the prefix of @haystack@
-- before the matched needle, the third component is the remainder of
-- the @haystack@ /after/ the needle..
breakOn :: [(String,t)] -> String -> Maybe (t, String, String)
breakOn needles haystack = go 0 haystack
  where go _ [] = Nothing
        go !i xs@(_:xs') =
          case find (flip isPrefixOf xs . fst) needles of
            Nothing -> go (i+1) xs'
            Just (n,tag) -> Just (tag, take i haystack, drop (length n) xs)
{-# INLINE breakOn #-}

-- | Used to make switching to the @text@ package easier.
cons :: a -> [a] -> [a]
cons x xs = x : xs
