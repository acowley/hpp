{-# LANGUAGE BangPatterns, OverloadedStrings, ViewPatterns #-}
-- | Tokenization breaks a 'String' into pieces of whitespace,
-- constants, symbols, and identifiers.
module Hpp.Tokens (Token(..), detok, isImportant, notImportant, importants,
                   trimUnimportant, detokenize, tokenize, newLine,
                   skipLiteral) where
import Control.Arrow (first, second)
import Data.Char (isAlphaNum, isDigit, isSpace, isOctDigit, isHexDigit, digitToInt)
import Data.Foldable (foldl')
import Data.Monoid ((<>))
import Data.Maybe (fromJust)
import Data.String (IsString, fromString)
import Hpp.StringSig

-- | Tokenization is 'words' except the white space is tagged rather
-- than discarded.
data Token s = Important s
             -- ^ Identifiers, symbols, and constants
             | Other s
             -- ^ White space, etc.
               deriving (Eq,Ord,Show)

instance Functor Token where
  fmap f (Important s) = Important (f s)
  fmap f (Other s) = Other (f s)
  {-# INLINE fmap #-}

-- | Extract the contents of a 'Token'.
detok :: Token s -> s
detok (Important s) = s
detok (Other s) = s
{-# INLINE detok #-}

-- | 'True' if the given 'Token' is 'Important'; 'False' otherwise.
isImportant :: Token s -> Bool
isImportant (Important _) = True
isImportant _ = False

-- | 'True' if the given 'Token' is /not/ 'Important'; 'False'
-- otherwise.
notImportant :: Token s -> Bool
notImportant (Other _) = True
notImportant _ = False

-- | Return the contents of only 'Important' (non-space) tokens.
importants :: [Token s] -> [s]
importants = map detok . filter isImportant

-- | Trim 'Other' 'Token's from both ends of a list of 'Token's.
trimUnimportant :: [Token s] -> [Token s]
trimUnimportant = aux id . dropWhile (not . isImportant)
  where aux _ [] = []
        aux acc (t@(Important _) : ts) = acc (t : aux id ts)
        aux acc (t@(Other _) : ts) = aux (acc . (t:)) ts

-- | Is a 'Token' a newline character?
newLine :: (Eq s, IsString s) => Token s -> Bool
newLine (Other s) = s == "\n"
newLine _ = False

maybeImp :: Stringy s => s -> [Token s]
maybeImp s = if isEmpty s then [] else [Important s]

digitsFromBase :: Stringy s => Int -> s -> s
digitsFromBase base = fromString . show . foldl' aux 0 . map digitToInt . toChars
  where aux acc d = base * acc + d

escapeChar :: Stringy s => Char -> Maybe s
escapeChar = fmap fromString . flip lookup lut
  where lut = map (second (show :: Int -> String))
                  [ ('a', 0x07), ('b', 0x08), ('f', 0x0C), ('n', 0x0A)
                  , ('r', 0x0D), ('t', 0x09), ('v', 0x0B), ('\\', 0x5C)
                  , ('\'', 0x27), ('"', 0x22), ('?', 0x3F) ]

data TokChar = TokSpace Char | TokQuote | TokDQuote

-- | Break a 'String' into space and non-whitespace runs.
tokWords :: Stringy s => s -> [Token s]
tokWords s =
  case sbreak aux s of
     -- No word breaks
     Nothing -> [Important s]

     -- Word delimited by space
     Just (TokSpace c, pre, pos) ->
       case sbreak (predicateJust (not . isSpace)) pos of
         Nothing -> maybeImp pre ++ [Other (cons c pos)]
         Just (c', spaces, pos') ->
           maybeImp pre ++
           Other (cons c spaces) : tokWords (cons c' pos')

     -- Possible character literal
     Just (TokQuote, pre, pos) ->
       let pre' = snoc pre '\''
       in case pos of
            '\\' :. cs ->
              case sbreak (boolJust . (== '\'')) cs of
                Nothing -> [Important (pre' <> pos)]
                Just (_,esc,pos')
                  -- Deals with the '\'' case
                  | isEmpty esc ->
                    case sbreak (boolJust . (== '\'')) pos' of
                      Just (_,esc', pos'')
                        | isEmpty esc' ->
                          Important ("'\\\''") : tokWords pos''
                          -- Important (fromJust $ escapeChar '\'') : tokWords pos''
                      _ -> [Important (pre' <> pos)]
                  | otherwise ->
                    let esc' = Important ("'\\" <> snoc esc '\'')
                    -- let esc' = if sall isOctDigit esc
                    --           then Important (digitsFromBase 8 esc)
                    --           else case esc of
                    --                   'x' :. hs
                    --                     | sall isHexDigit hs ->
                    --                     Important (digitsFromBase 16 hs)
                    --                   (escapeChar -> Just e) :. Nil -> Important e
                    --                   _ -> Important ("'\\" <> snoc esc '\'')
                    in  maybeImp pre ++ esc' : tokWords pos'
            c:.('\'':.cs) -> maybeImp pre
                                ++ Important (fromString ['\'', c, '\''])
                                : tokWords cs
            _:._ -> let oops = snoc pre '\''
                    in case tokWords pos of
                         (Important t:ts) -> Important (oops<>t) : ts
                         ts -> Important oops : ts
            _ -> [Important (snoc pre '\'')]

     -- String literal
     Just (TokDQuote, pre, pos) ->
       let (lit,pos') = skipLiteral pos
       in (if isEmpty pre then [] else [Important pre])
          ++ Important (cons '"' lit) : tokWords pos'
  where aux c | isSpace c = Just (TokSpace c)
              | c == '\'' = Just TokQuote
              | c == '"' = Just TokDQuote
              | otherwise = Nothing
        {-# INLINE aux #-}
{-# INLINABLE tokWords #-}

data LitStringChar = DBackSlash | EscapedDQuote | DQuote
skipLiteral :: Stringy s => s -> (s,s)
skipLiteral s =
  case breakOn [("\\\\", DBackSlash), ("\\\"", EscapedDQuote), ("\"", DQuote)] s of
    Nothing -> (s, mempty) -- Unmatched double quote?!
    Just (DBackSlash, pre, pos) -> first ((pre <> "\\\\") <>) (skipLiteral pos)
    Just (EscapedDQuote, pre, pos) -> first ((pre <> "\\\"") <>) (skipLiteral pos)
    Just (DQuote, pre, pos) -> (snoc pre '"', pos)
{-# INLINABLE skipLiteral #-}

-- | @splits isDelimiter str@ tokenizes @str@ using @isDelimiter@ as a
-- delimiter predicate. Leading whitespace is also stripped from
-- tokens.
splits :: Stringy s => (Char -> Bool) -> s -> [s]
splits isDelim = filter (not . isEmpty) . go . sdropWhile isSpace
  where go s = case sbreak (\c -> if isDelim c then Just c else Nothing) s of
                  Nothing -> [s]
                  Just (d, pre, pos) ->
                    pre : fromString [d] : go (sdropWhile isSpace pos)
{-# INLINE splits #-}

-- | Predicate on space characters based on something approximating
-- valid identifier syntax. This is used to break apart non-space
-- characters.
validIdentifierChar :: Char -> Bool
validIdentifierChar c = isAlphaNum c || c == '_' || c == '\''

-- | Something like @12E+FOO@ is a single pre-processor token, so
-- @FOO@ should not be macro expanded.
fixExponents :: Stringy s => [Token s] -> [Token s]
fixExponents [] = []
fixExponents (t1'@(Important t1) : ts@(Important t2 : Important t3 : ts')) =
  case (,,,) <$> uncons t1 <*> unsnoc t1 <*> uncons t2 <*> uncons t3 of
    Just !(!(!d1,_), !(_,!e), !(!c,!cs), !(!d2,_))
      | elem c ("-+" :: [Char]) &&
        isEmpty cs && isDigit d1 && isAlphaNum d2 &&
        elem e ("eE" :: [Char]) -> let t = t1 <> t2 <> t3
                                   in t `seq` Important t : fixExponents ts'
    _ -> t1' : fixExponents ts
fixExponents (t:ts) = t : fixExponents ts
{-# INLINABLE fixExponents #-}

-- | Break an input 'String' into a sequence of 'Tokens'. Warning:
-- This may not exactly correspond to your target language's
-- definition of a valid identifier!
tokenize :: Stringy s => s -> [Token s]
tokenize = fixExponents . foldMap seps . tokWords
  where seps t@(Other _) = [t]
        seps t@(Important s) =
          case uncons s of
            Nothing -> []
            Just (c,_)
              | c == '"' -> [t]
              | c == '\'' -> [t]
              | otherwise -> map Important (splits (not . validIdentifierChar) s)
{-# INLINABLE tokenize #-}

-- | Collapse a sequence of 'Tokens' back into a 'String'. @detokenize
-- . tokenize == id@.
detokenize :: Monoid s => [Token s] -> s
detokenize = foldMap detok
{-# INLINE detokenize #-}
