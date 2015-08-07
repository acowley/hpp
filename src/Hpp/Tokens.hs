-- | Tokenization breaks a 'String' into pieces of whitespace,
-- constants, symbols, and identifiers.
module Hpp.Tokens where
import Data.Char (isAlphaNum, isDigit, isSpace)

-- | Tokenization is 'words' except the white space is tagged rather
-- than discarded.
data Token = Important String
           -- ^ Identifiers, symbols, and constants
           | Other String
           -- ^ White space, etc.
             deriving (Eq,Ord,Show)

-- | Extract the contents of a 'Token'.
detok :: Token -> String
detok (Important s) = s
detok (Other s) = s

-- | 'True' if the given 'Token' is 'Important'; 'False' otherwise.
isImportant :: Token -> Bool
isImportant (Important _) = True
isImportant _ = False

-- | Return the contents of only 'Important' (non-space) tokens.
importants :: [Token] -> [String]
importants = map detok . filter isImportant

-- | Break a 'String' into space and non-whitespace runs.
tokWords :: String -> [Token]
tokWords [] = []
tokWords (c:cs)
  | isSpace c = let (spaces,rst) = break (not . isSpace) cs
                in Other (c : spaces) : tokWords rst
  | c == '\'' && isCharLit = goCharLit
  | c == '"' = flip skipLiteral cs $ \str rst ->
               Important (str []) : tokWords rst
  | otherwise = let (chars,rst) = break (not . validIdentifierChar) cs
                in Important (c:chars) : tokWords rst
     where (isCharLit, goCharLit) =
             case cs of
               (c':'\'':cs') -> (True, Important ['\'',c','\''] : tokWords cs')
               _ -> (False, [])

-- | If you encounter a string literal, call this helper with a
-- double-barreled continuation and the rest of your input. The
-- continuation will be called with the remainder of the string
-- literal as the first argument, and the remaining input as the
-- second argument.
skipLiteral :: ((String -> String) -> String -> r) -> String -> r
skipLiteral k = go ('"':)
  where go acc ('\\':'\\':cs) = go (acc . ("\\\\"++)) cs
        go acc ('\\':'"':cs) = go (acc . ("\\\""++)) cs
        -- go acc (c:'"':cs) = k (acc . ([c,'"']++)) cs
        go acc ('"':cs) = k (acc . ('"':)) cs
        go acc (c:cs) = go (acc . (c :)) cs
        go acc [] = k acc []

-- | @splits isDelimiter str@ tokenizes @str@ using @isDelimiter@ as a
-- delimiter predicate. Leading whitespace is also stripped from
-- tokens.
splits :: (Char -> Bool) -> String -> [String]
splits isDelim = filter (not . null) . go . dropWhile isSpace
  where go s = case break isDelim s of
                 (h,[]) -> [dropWhile isSpace h]
                 (h,d:t) -> dropWhile isSpace h : [d] : go t

-- | Predicate on space characters based on something approximating
-- valid identifier syntax. This is used to break apart non-space
-- characters.
validIdentifierChar :: Char -> Bool
validIdentifierChar c = isAlphaNum c || c == '_' || c == '\''

-- | Something like @12E+FOO@ is a single pre-processor token, so
-- @FOO@ should not be macro expanded.
fixExponents :: [Token] -> [Token]
fixExponents [] = []
fixExponents (Important (t1@(d1:_)):Important [c]:Important (d2:t2):ts)
  | elem c "-+" && isDigit d1 && elem (last t1) "eE" && isAlphaNum d2 =
    Important (t1++c:d2:t2) : fixExponents ts
fixExponents (t:ts) = t : fixExponents ts

-- | Break an input 'String' into a sequence of 'Tokens'. Warning:
-- This may not exactly correspond to your target language's
-- definition of a valid identifier!
tokenize :: String -> [Token]
tokenize = fixExponents . concatMap seps . tokWords
  where seps t@(Other _) = [t]
        seps t@(Important ('"':_)) = [t]
        seps t@(Important ('\'':_)) = [t]
        seps (Important s) = map Important $
                             splits (not . validIdentifierChar) s

-- | Collapse a sequence of 'Tokens' back into a 'String'. @detokenize
-- . tokenize == id@.
detokenize :: [Token] -> String
detokenize = concatMap detok
