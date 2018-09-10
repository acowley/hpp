{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, OverloadedStrings,
             PatternSynonyms, TypeSynonymInstances, ViewPatterns #-}
-- | Defines a signature, 'Stringy', for string-like types that we may
-- want to use.
module Hpp.StringSig where
import Data.Char
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.String (IsString)
import qualified Hpp.String as S
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO (Handle, hPutStr)

data CharOrSub s = CharMatch !s !s | SubMatch !s !s | NoMatch

-- | A collection of operations relating to sequences of characters.
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

  -- | A special case of 'breakOn' in which we are looking for either
  -- a special character or a particular substring.
  breakCharOrSub :: Char -> s -> s -> CharOrSub s
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
  breakCharOrSub c sub str =
    case S.breakOn [([c], True), (sub, False)] str of
      Nothing -> NoMatch
      Just (True, pre, pos) -> CharMatch pre pos
      Just (False, pre, pos) -> SubMatch pre pos
  {-# INLINE breakCharOrSub #-}
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
  stringify s = B.cons '"' (B.snoc (B.concatMap aux (strip s)) '"')
    where aux '\\' = "\\\\"
          aux '"' = "\\\""
          aux c = B.singleton c
          strip = trimSpaces . B.dropWhile isSpace
  {-# INLINE stringify #-}
  unquote s = let s' = case B.uncons s of
                         Nothing -> s
                         Just (c, rst) -> if c == '"' then rst else s
              in case B.unsnoc s' of
                   Nothing -> s'
                   Just (ini, c) -> if c == '"' then ini else s'
  {-# INLINE unquote #-}
  trimSpaces s = let go !i = if isSpace (B.index s i)
                             then go (i-1)
                             else B.length s - i - 1
                 in B.drop (go (B.length s - 1)) s
  {-# INLINE trimSpaces #-}
  breakOn [!(!n1,!t1)] haystack =
    case B.breakSubstring n1 haystack of
      (pre,pos) | B.null pos -> Nothing
                | otherwise -> Just (t1, pre, B.drop (B.length n1) pos)

  breakOn [!(!n1, !t1), !(n2, !t2)] haystack = go2 0 haystack
    where go2 !i !h
            | B.null h = Nothing
            | B.isPrefixOf n1 h = let !h' = B.drop (B.length n1) h
                                      !pre = B.take i haystack
                                  in Just (t1, pre, h')
            | B.isPrefixOf n2 h = let !h' = B.drop (B.length n2) h
                                      !pre = B.take i haystack
                                  in Just (t2, pre, h')
            | otherwise = go2 (i+1) (B.tail h)
  breakOn [!(!n1, !t1), !(n2, !t2), !(!n3, !t3)] haystack = go3 0 haystack
    where go3 !i !h
            | B.null h = Nothing
            | B.isPrefixOf n1 h = let h' = B.drop (B.length n1) h
                                  in Just (t1, B.take i haystack, h')
            | B.isPrefixOf n2 h = let h' = B.drop (B.length n2) h
                                  in Just (t2, B.take i haystack, h')
            | B.isPrefixOf n3 h = let h' = B.drop (B.length n3) h
                                  in Just (t3, B.take i haystack, h')
            | otherwise = go3 (i+1) (B.tail h)
  breakOn needles haystack = go 0 haystack
    where go !i !h
            | B.null h = Nothing
            | otherwise =
              case L.find (flip B.isPrefixOf h . fst) needles of
                Nothing -> go (i+1) (B.tail h)
                Just (n,tag) -> let h' = B.drop (B.length n ) h
                                in Just (tag, B.take i haystack, h')
  {-# INLINE breakOn #-}
  breakCharOrSub c sub str =
    case B.elemIndex c str of
      Nothing -> case B.breakSubstring sub str of
                   (pre,pos)
                     | B.null pos -> NoMatch
                     | otherwise -> SubMatch pre (B.drop (B.length sub) pos)
      Just i ->
        case B.breakSubstring sub str of
          (pre,pos)
            | B.null pos -> CharMatch (B.take i str) (B.drop (i+1) str)
            | B.length pre < i -> SubMatch pre (B.drop (B.length sub) pos)
            | otherwise -> CharMatch (B.take i str) (B.drop (i+1) str)

  {-# INLINE breakCharOrSub #-}
  cons = B.cons
  uncons = B.uncons
  snoc = B.snoc
  unsnoc = B.unsnoc
  sdrop = B.drop . fromIntegral
  sbreak f s = case B.break (isJust . f) s of
                 (h,t) -> case B.uncons t of
                            Nothing -> Nothing
                            Just (c,t') -> fmap (\r -> (r,h,t')) (f c)
  {-# INLINE sbreak #-}
  sall = B.all
  sIsPrefixOf = B.isPrefixOf
  isEmpty = B.null
  readLines = fmap (map stripR . map BL.toStrict . BL.lines) . BL.readFile
  {-# INLINE readLines #-}
  putStringy = B.hPutStr
  toChars = B.unpack
  copy = B.copy

boolJust :: Bool -> Maybe ()
boolJust True = Just ()
boolJust False = Nothing
{-# INLINE boolJust #-}

predicateJust :: (a -> Bool) -> a -> Maybe a
predicateJust f c = if f c then Just c else Nothing
{-# INLINE predicateJust #-}

sdropWhile :: Stringy s => (Char -> Bool) -> s -> s
sdropWhile f s = case sbreak (boolJust . f) s of
                   Nothing -> s
                   Just (_, _, s') -> s'
{-# INLINE sdropWhile #-}

stripR :: ByteString -> ByteString
stripR bs
  | not (B.null bs) && B.last bs == '\r' = B.init bs
  | otherwise = bs
{-# INLINE stripR #-}

#if __GLASGOW_HASKELL__ >= 800
pattern (:.) :: Stringy s => Char -> s -> s
#else
pattern (:.) :: () => Stringy s => Char -> s -> s
#endif
pattern x :. xs <- (uncons -> Just (x,xs)) where
  x:.xs = cons x xs
infixr 5 :.

#if __GLASGOW_HASKELL__ >= 800
pattern Nil :: Stringy s => s
#else
pattern Nil :: () => Stringy s => s
#endif
pattern Nil <- (isEmpty -> True) where
  Nil = mempty
