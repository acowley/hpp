-- | Helpers for working with 'String's
module Hpp.String where
import Data.Char (isSpace)

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
