{-# LANGUAGE BangPatterns #-}
-- | HELPERS for working with 'String's
module Hpp.String (stringify, unquote, trimSpaces, breakOn, cons) where
import Data.Char (isSpace)
import Data.List (isPrefixOf)

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
-- @breakOn needle haystack@ finds the first instance of @needle@ in
-- @haystack@. The first component of the result is the prefix of
-- @haystack@ before @needle@ is matched. The second is the remainder of
-- @haystack@, starting with the match.
breakOn :: String -> String -> (String, String)
breakOn needle haystack = go 0 haystack
  where go _ [] = (haystack, [])
        go !i xs@(_:xs')
          | needle `isPrefixOf` xs = (take i haystack, xs)
          | otherwise = go (i+1) xs'

-- | Used to make switching to the @text@ package easier.
cons :: a -> [a] -> [a]
cons x xs = x : xs
