-- | An expression language corresponding to the subset of C syntax
-- that may be used in preprocessor conditional directives. See
-- <https://gcc.gnu.org/onlinedocs/cpp/If.html>
module Hpp.Expr (Expr(..), readLitInt, parseExpr, renderExpr, evalExpr) where
import Control.Applicative
import Control.Monad ((>=>))
import Data.Bits (complement, (.&.), (.|.), xor, shiftL, shiftR)
import Data.List (foldl')
import Text.Read (readMaybe)
import Hpp.Tokens
import Data.Char (digitToInt, toLower)

-- * Token Parsing Types

data BinOp = Add | Sub | Mul | Div | Mod
           | BitAnd | BitOr | BitXor | ShiftL | ShiftR
           | LessThan | GreaterThan | EqualTo | NotEqualTo
           | GreaterOrEqualTo | LessOrEqualTo
           | And | Or
             deriving (Eq, Ord, Show)

data UnaryOp = Neg | BitNot | Not | Defined deriving (Eq,Ord,Show)

data Lit = LitInt Int | LitStr String | LitChar Char | LitID String
           deriving (Eq,Ord,Show)

data Parsed = PBinOp BinOp | PUnaryOp UnaryOp | PLit Lit deriving (Eq,Ord,Show)

-- * Associativity and Precedence

data Assoc = RightLeft | LeftRight deriving (Eq, Ord, Show)

associativity :: Either BinOp UnaryOp -> Assoc
associativity = either (const LeftRight) (const RightLeft)

precedence :: Either BinOp UnaryOp -> Int
precedence (Right _) = 10
precedence (Left x) = precedenceBin x

-- | Precedence of binary operators from lowest to highest.
precedenceBin :: BinOp -> Int
precedenceBin Or = 0
precedenceBin And = 1
precedenceBin BitOr = 2
precedenceBin BitXor = 3
precedenceBin BitAnd = 4
precedenceBin EqualTo = 5
precedenceBin NotEqualTo = 5
precedenceBin LessThan = 6
precedenceBin GreaterThan = 6
precedenceBin GreaterOrEqualTo = 6
precedenceBin LessOrEqualTo = 6
precedenceBin ShiftL = 7
precedenceBin ShiftR = 7
precedenceBin Add = 8
precedenceBin Sub = 8
precedenceBin Mul = 9
precedenceBin Div = 9
precedenceBin Mod = 9

-- * Lexing

-- | String literals are split by tokenization. Fix them!
fixStringLits :: [Token] -> Maybe [String]
fixStringLits [] = Just []
fixStringLits (Important h@('"':_):xs) =
  let (hs,ys) = break ((== '"') . last . detok) xs
  in case ys of
       [] -> Nothing
       (y:ys') -> fmap ((h ++ detokenize hs ++ detok y) :)
                       (fixStringLits ys')
fixStringLits (Important x:xs) = fmap (x :) (fixStringLits xs)
fixStringLits (Other _ : xs) = fixStringLits xs

onFirstImportant :: (String -> String)
                 -> ([Token] -> [Token])
                 -> [Token]
                 -> [Token]
onFirstImportant f k = go
  where go [] = k []
        go (Important s : ts') = Important (f s) : k ts'
        go (o@(Other _) : ts') = o : go ts'

-- | Re-combine positive and negative unary operators with the tokens
-- to which they are attached.
fixUnaryOps :: [Token] -> [Token]
fixUnaryOps [] = []
fixUnaryOps (t0:ts0) =
  case t0 of
    Important "+" -> onFirstImportant ('+':) (go False) ts0
    Important "-" -> onFirstImportant ('-':) (go False) ts0
    _ -> go False (t0:ts0)
  where go _ [] = []
        go _ (Important "(":ts) = Important "(" : go True ts
        go True (Important "+":ts) = onFirstImportant ('+':) (go False) ts
        go True (Important "-":ts) = onFirstImportant ('-':) (go False) ts
        go _ (Important t:ts) = let isOp = maybe False (const True) $ parseOp t
                                in Important t : go isOp ts
        go _ (o@(Other _):ts) = o : go False ts

-- | Re-combine multiple-character operators.
fixBinaryOps :: [Token] -> [Token]
fixBinaryOps [] = []
fixBinaryOps (h@(Important t1) : ts@(Important t2 : ts')) =
  case parseBinOp (t1++t2) of
    Just o -> Important (renderBinOp o) : fixBinaryOps ts'
    Nothing -> h : fixBinaryOps ts
fixBinaryOps (t:ts) = t : fixBinaryOps ts

renderBinOp :: BinOp -> String
renderBinOp Add = "+"
renderBinOp Sub = "-"
renderBinOp Mul = "*"
renderBinOp Div = "/"
renderBinOp Mod = "%"
renderBinOp BitAnd = "&"
renderBinOp BitOr = "|"
renderBinOp BitXor = "^"
renderBinOp ShiftL = "<<"
renderBinOp ShiftR = ">>"
renderBinOp LessThan = "<"
renderBinOp GreaterThan = ">"
renderBinOp EqualTo = "=="
renderBinOp NotEqualTo = "!="
renderBinOp GreaterOrEqualTo = ">="
renderBinOp LessOrEqualTo = "<="
renderBinOp And = "&&"
renderBinOp Or = "||"

renderUnaryOp :: UnaryOp -> String
renderUnaryOp Neg = "-"
renderUnaryOp BitNot = "~"
renderUnaryOp Not = "!"
renderUnaryOp Defined = "defined "

lexExpr :: [Token] -> Maybe [String]
lexExpr = fixStringLits . fixUnaryOps . fixBinaryOps

-- * Parsing Tokens

parseUnaryOp :: String -> Maybe UnaryOp
parseUnaryOp "-" = Just Neg
parseUnaryOp "~" = Just BitNot
parseUnaryOp "!" = Just Not
parseUnaryOp "defined" = Just Defined
parseUnaryOp _ = Nothing

parseBinOp :: String -> Maybe BinOp
parseBinOp "+" = Just Add
parseBinOp "-" = Just Sub
parseBinOp "*" = Just Mul
parseBinOp "/" = Just Div
parseBinOp "%" = Just Mod
parseBinOp "&" = Just BitAnd
parseBinOp "|" = Just BitOr
parseBinOp "^" = Just BitXor
parseBinOp "<<" = Just ShiftL
parseBinOp ">>" = Just ShiftR
parseBinOp "<" = Just LessThan
parseBinOp ">" = Just GreaterThan
parseBinOp "==" = Just EqualTo
parseBinOp "!=" = Just NotEqualTo
parseBinOp ">=" = Just GreaterOrEqualTo
parseBinOp "<=" = Just LessOrEqualTo
parseBinOp "&&" = Just And
parseBinOp "||" = Just Or
parseBinOp _ = Nothing

parseOp :: String -> Maybe (Either BinOp UnaryOp)
parseOp s = Left <$> parseBinOp s <|> Right <$> parseUnaryOp s

readWideChar :: String -> Maybe Char
readWideChar ('L':'\'':cs0) = go 0 cs0
  where go n ['\''] = Just $ toEnum n
        go n (c:cs) = go (n*256 + fromEnum c) cs
        go _ [] = Nothing
readWideChar _ = Nothing

readNarrowChar :: String -> Maybe Char
readNarrowChar ['\'',c,'\''] = Just c
readNarrowChar _ = Nothing

parseLit :: String -> Maybe Lit
parseLit s = case readLitInt s of
               Just i -> Just (LitInt i)
               Nothing -> case readNarrowChar s <|> readWideChar s  of
                            Just c -> Just (LitChar c)
                            Nothing -> case readMaybe s of
                                         Just str -> Just (LitStr str)
                                         Nothing -> Nothing

digitsFromBase :: Int -> [Int] -> Int
digitsFromBase base = foldl' aux 0
  where aux acc d = base*acc + d

readLitInt' :: String -> Maybe Int
readLitInt' ('0':x:hexDigits)
  | x == 'x' || x == 'X' = fmap (digitsFromBase 16) (mapM hexDigit hexDigits)
  where hexDigit c
          | c >= '0' && c <= '9' = Just $ digitToInt c
          | c >= 'a' && c <= 'f' = Just $ (fromEnum c - fromEnum 'a') + 10
          | c >= 'A' && c <= 'F' = Just $ (fromEnum c - fromEnum 'A') + 10
          | otherwise = Nothing

readLitInt' ('0':octalDigits) = fmap (digitsFromBase 8)
                                     (mapM octalDigit octalDigits)
  where octalDigit c
          | c >= '0' && c < '8' = Just $ digitToInt c
          | otherwise = Nothing
readLitInt' s = readMaybe s

-- | Read a literal integer. These may be decimal, octal, or
-- hexadecimal, and may have a case-insensitive suffix of @u@, @l@, or
-- @ul@.
readLitInt :: String -> Maybe Int
readLitInt s = case map toLower . take 2 $ reverse s of
                 "lu" -> readLitInt' (init (init s))
                 'l':_ -> readLitInt (init s)
                 'u':_ -> readLitInt (init s)
                 _ -> readLitInt' s

-- * Shunting Yard

-- | For <https://en.wikipedia.org/wiki/Shunting-yard_algorithm reference>

data FunLike = FunBin BinOp | FunUnary UnaryOp | FunParen deriving (Eq,Ord,Show)

-- | Exhaust the function/operator stack returning the parsed
-- expression in RPN.
finishShunting :: [Parsed] -> [FunLike] -> Maybe [Parsed]
finishShunting q [] = Just (reverse q)
finishShunting _ (FunParen:_) = Nothing
finishShunting q (FunBin op:ops) = finishShunting (PBinOp op : q) ops
finishShunting q (FunUnary op:ops) = finishShunting (PUnaryOp op : q) ops

opParsed :: Either BinOp UnaryOp -> Parsed
opParsed (Left x) = PBinOp x
opParsed (Right x) = PUnaryOp x

opFun :: Either BinOp UnaryOp -> FunLike
opFun = either FunBin FunUnary

-- | Shunting yard algorithm part for dealing with operators.
juggleBinOps :: Either BinOp UnaryOp -> [Parsed] -> [FunLike]
             -> ([Parsed] -> [FunLike] -> r)
             -> r
juggleBinOps o q [] k = k q [opFun o]
juggleBinOps o1 q s@(o2:ss) k =
  case o2 of
    FunBin o -> aux $ Left o
    FunUnary o -> aux $ Right o
    FunParen -> done
  where a1 = associativity o1
        p1 = precedence o1
        aux o2'
          | a1 == LeftRight && p1 <= p2 = juggleBinOps o1 (opParsed o2':q) ss k
          | a1 == RightLeft && p1 < p2 = juggleBinOps o1 (opParsed o2':q) ss k
          | otherwise = done
          where p2 = precedence o2'
        done = k q (opFun o1 : s)

-- | Shunting yard to produce a reverse polish notation (RPN) list of
-- tokens.
shuntRPN :: [Parsed] -> [FunLike] -> [String] -> Maybe [Parsed]
shuntRPN q s [] = finishShunting q s
shuntRPN q s ("(":es) = shuntRPN q (FunParen:s) es
shuntRPN q s (")":es) = let go _ [] = Nothing
                            go q' (FunParen:s') = shuntRPN q' s' es
                            go q' (FunBin op:s') = go (PBinOp op : q') s'
                            go q' (FunUnary op:s') = go (PUnaryOp op : q') s'
                        in go q s
shuntRPN q s (('+':e@(_:_)):es) = shuntRPN q s (e:es)
shuntRPN q s (('-':e@(_:_)):es) = shuntRPN q (FunUnary Neg : s) (e:es)
shuntRPN q s ("!":es) = shuntRPN q (FunUnary Not : s) es
shuntRPN q s ("~":es) = shuntRPN q (FunUnary BitNot : s) es
shuntRPN q s ("defined":es) = shuntRPN q (FunUnary Defined : s) es
shuntRPN q s (e:es) =
  case parseLit e of
    Just l -> shuntRPN (PLit l : q) s es
    Nothing -> case parseOp e of
                 Just o -> juggleBinOps o q s $
                           \q' s' -> shuntRPN q' s' es
                 Nothing -> shuntRPN (PLit (LitID e) : q) s es

-- * Expressions

-- | Expressions are literal values, binary operators applied to two
-- sub-expressions, or unary operators applied to a single
-- sub-expression.
data Expr = ELit Lit | EBinOp BinOp Expr Expr | EUnaryOp UnaryOp Expr
            deriving (Eq, Ord, Show)

-- | Convert an RPN list of parsed tokens into an 'Expr'.
rpnToExpr :: [Expr] -> [Parsed] -> Maybe Expr
rpnToExpr [e] [] = Just e
rpnToExpr _ [] = Nothing
rpnToExpr s (PLit e:es) = rpnToExpr (ELit e:s) es
rpnToExpr (s:ss) (PUnaryOp o : es) = rpnToExpr (EUnaryOp o s:ss) es
rpnToExpr (s1:s2:ss) (PBinOp o : es) = rpnToExpr (EBinOp o s2 s1 : ss) es
rpnToExpr _ _ = Nothing

-- | Try to read an 'Expr' from a sequence of 'Token's.
parseExpr :: [Token] -> Maybe Expr
parseExpr = lexExpr >=> shuntRPN [] [] >=> rpnToExpr []

-- | Pretty-print an 'Expr' to something semantically equivalent to the original
-- C syntax (some parentheses may be added).
renderExpr :: Expr -> String
renderExpr (ELit (LitInt e)) = show e
renderExpr (ELit (LitStr e)) = '"':e++"\""
renderExpr (ELit (LitChar c)) = [c]
renderExpr (ELit (LitID e)) = e
renderExpr (EBinOp o e1 e2) = concat [ "(", renderExpr e1," ",renderBinOp o 
                                     , " ", renderExpr e2, ")" ]
renderExpr (EUnaryOp o e1) = renderUnaryOp o ++ renderExpr e1

-- * Evaluation

-- | @evalExpr isDefined e@ evaluates expression @e@ in an environment
-- where the existence of macro definitions is captured by the
-- @isDefined@ predicate. All expressions evaluate to an 'Int'!
evalExpr :: Expr -> Int
evalExpr = go
  where go (ELit (LitInt e)) = e
        go (ELit (LitStr _)) = 1
        go (ELit (LitID _)) = 0
        go (ELit (LitChar c)) = fromEnum c
        go (EBinOp Add e2 e3) = go e2 + go e3
        go (EBinOp Sub e2 e3) = go e2 - go e3
        go (EBinOp Mul e2 e3) = go e2 * go e3
        go (EBinOp Div e2 e3) = go e2 `div` go e3
        go (EBinOp Mod e2 e3) = go e2 `mod` go e3
        go (EBinOp BitAnd e2 e3) = go e2 .&. go e3
        go (EBinOp BitOr e2 e3) = go e2 .|. go e3
        go (EBinOp BitXor e2 e3) = go e2 `xor` go e3
        go (EBinOp ShiftL e2 e3) = go e2 `shiftL` go e3
        go (EBinOp ShiftR e2 e3) = go e2 `shiftR` go e3
        go (EBinOp LessThan e2 e3) = fromEnum $ go e2 < go e3
        go (EBinOp GreaterThan e2 e3) = fromEnum $ go e2 > go e3
        go (EBinOp EqualTo e2 e3) = fromEnum $ go e2 == go e3
        go (EBinOp NotEqualTo e2 e3) = fromEnum $ go e2 /= go e3
        go (EBinOp GreaterOrEqualTo e2 e3) = fromEnum $ go e2 >= go e3
        go (EBinOp LessOrEqualTo e2 e3) = fromEnum $ go e2 <= go e3
        go (EBinOp And e2 e3) = fromEnum $ go e2 /= 0 && go e3 /= 0
        go (EBinOp Or e2 e3) = fromEnum $ go e2 /= 0 || go e3 /= 0
        go (EUnaryOp Neg e2) = -(go e2)
        go (EUnaryOp BitNot e2) = complement $ go e2
        go (EUnaryOp Not e2) = fromEnum $ go e2 == 0
        go (EUnaryOp Defined e2) =
          case e2 of 
            ELit (LitInt 1) -> 1
            _ -> 0
