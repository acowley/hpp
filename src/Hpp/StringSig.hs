{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings,
             PatternSynonyms, TypeSynonymInstances, ViewPatterns #-}
module Hpp.StringSig where
import Data.Char
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.String (IsString)
import qualified Hpp.String as S
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO (Handle, hPutStr)

class (IsString s, Monoid s) => Stringy s where
  -- | Stringification puts double quotes around a string and
  -- backslashes before existing double quote characters and backslash
  -- characters.
  stringify :: s -> s

  -- | Remove double quote characters from the ends of a string.
  unquote :: s -> s

  -- | Trim trailing spaces from a 'String'
  trimSpaces :: s -> s

  -- | Similar to the function of the same name in the @text@ package.
  --
  -- @breakOn needles haystack@ finds the first instance of an element
  -- of @needles@ in @haystack@. The first component of the result is
  -- the needle tag, the second component is the prefix of @haystack@
  -- before the matched needle, the third component is the remainder of
  -- the @haystack@ /after/ the needle..
  breakOn :: [(s,t)] -> s -> Maybe (t, s, s)
  cons :: Char -> s -> s
  uncons :: s -> Maybe (Char, s)
  snoc :: s -> Char -> s
  unsnoc :: s -> Maybe (s, Char)
  sdrop :: Int -> s -> s
  sbreak :: (Char -> Maybe t) -> s -> Maybe (t,s,s)
  sall :: (Char -> Bool) -> s -> Bool
  sIsPrefixOf :: s -> s -> Bool
  isEmpty :: s -> Bool
  readLines :: FilePath -> IO [s]
  putStringy :: Handle -> s -> IO ()
  toChars :: s -> [Char]
  -- | An opportunity to copy a string to its own storage to help with GC
  copy :: s -> s

instance Stringy String where
  stringify = S.stringify
  {-# INLINE stringify #-}
  unquote = S.unquote
  {-# INLINE unquote  #-}
  trimSpaces = S.trimSpaces
  {-# INLINE trimSpaces #-}
  breakOn = S.breakOn
  {-# INLINE breakOn #-}
  cons = S.cons
  {-# INLINE cons #-}
  uncons = L.uncons
  {-# INLINE uncons #-}
  snoc s c = s ++ [c]
  {-# INLINE snoc #-}
  unsnoc [] = Nothing
  unsnoc s = Just (init s, last s)
  {-# INLINE unsnoc #-}
  sdrop = drop
  {-# INLINE sdrop #-}
  sbreak _ [] =  Nothing
  sbreak p (x:xs') =
    case p x of
      Nothing -> let res = sbreak p xs' in fmap (_2 (x:)) res
      Just t -> Just (t, [], xs')
    where _2 f (a,b,c) = (a, f b, c)
  {-# INLINE sbreak #-}
  sall = all
  {-# INLINE sall #-}
  sIsPrefixOf = L.isPrefixOf
  isEmpty = null
  {-# INLINE isEmpty #-}
  readLines = fmap lines . readFile
  putStringy = hPutStr
  toChars = id
  copy = id

instance Stringy B.ByteString where
  stringify s = B.cons' '"' (B.snoc (B.concatMap aux (strip s)) '"')
    where aux '\\' = "\\\\"
          aux '"' = "\\\""
          aux c = B.singleton c
          strip = trimSpaces . B.dropWhile isSpace
  unquote s = let s' = case B.uncons s of
                         Nothing -> s
                         Just (c, rst) -> if c == '"' then rst else s
              in case B.unsnoc s' of
                   Nothing -> s'
                   Just (ini, c) -> if c == '"' then ini else s'
  trimSpaces s = let go !i = if isSpace (B.index s i)
                             then go (i-1)
                             else B.length s - i - 1
                 in B.drop (go (B.length s - 1)) s
  breakOn needles haystack = go 0 haystack
    where go !i h
            | B.null h = Nothing
            | otherwise =
              case L.find (flip B.isPrefixOf h . fst) needles of
                Nothing -> go (i+1) (B.tail h)
                Just (n,tag) -> let h' = B.drop (B.length n ) h
                                in Just (tag, B.take i haystack, h')
  cons = B.cons
  uncons = B.uncons
  snoc = B.snoc
  unsnoc = B.unsnoc
  sdrop = B.drop . fromIntegral
  sbreak f s = case B.break (isJust . f) s of
                 (h,t) -> case B.uncons t of
                            Nothing -> Nothing
                            Just (c,t') -> fmap (\r -> (r,h,t')) (f c)
  sall = B.all
  sIsPrefixOf = B.isPrefixOf
  isEmpty = B.null
  readLines = fmap B.lines . B.readFile
  putStringy = B.hPutStr
  toChars = B.unpack
  copy = B.copy

sbreak' :: Stringy s => [(Char,t)] -> s -> Maybe (t,s,s)
sbreak' xs = sbreak (flip lookup xs)
{-# INLINE sbreak' #-}

boolJust :: Bool -> Maybe ()
boolJust True = Just ()
boolJust False = Nothing

predicateJust :: (a -> Bool) -> a -> Maybe a
predicateJust f c = if f c then Just c else Nothing

sdropWhile :: Stringy s => (Char -> Bool) -> s -> s
sdropWhile f s = case sbreak (boolJust . f) s of
                   Nothing -> s
                   Just (_, _, s') -> s'

pattern (:.) :: Stringy s => Char -> s -> s
pattern x :. xs <- (uncons -> Just (x,xs)) where
  x:.xs = cons x xs
infixr 5 :.

pattern Nil :: Stringy s => s
pattern Nil <- (isEmpty -> True) where
  Nil = mempty
