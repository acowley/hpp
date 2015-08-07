-- | Line expansion is the core input token processing
-- logic. Object-like macros are substituted, and function-like macro
-- applications are expanded.
module Hpp.Expansion (expandLine) where
import Control.Arrow (first)
import Control.Monad ((<=<))
import Data.Bool (bool)
import Data.List (delete)
import Data.Maybe (listToMaybe, mapMaybe)
import Hpp.Config
import Hpp.Env
import Hpp.String
import Hpp.Tokens
import Hpp.Types

-- | Extract the 'Token' payload from a 'Scan'.
unscan :: Scan -> Maybe Token
unscan (Scan t) = Just t
unscan (Rescan t) = Just t
unscan _ = Nothing

-- | Expand a single line of tokenized code.
expandLine :: Config -> Int -> Env -> [Token]
           -> Either Error (Env, [Token])
expandLine cfg lineNum macros = fmap (\(e,ts) -> (e, mapMaybe unscan ts))
                              . expandLine' cfg lineNum macros . map Scan

argError :: Int -> String -> Int -> [String] -> Error
argError lineNum name arity args =
  TooFewArgumentsToMacro lineNum $ name++"<"++show arity++">"++show args

-- | Returns the /used/ environment and the new token stream.
expandFunction :: String -> Int -> ([([Scan],String)] -> [Scan])
               -> ([String] -> Error)
               -> ([Scan] -> Either Error [Scan])
               -> [Scan]
               -> Maybe (Either Error [Scan])
expandFunction name arity f mkErr expand = aux <=< argParse
  where aux :: ([[Scan]],[Scan]) -> Maybe (Either Error [Scan])
        aux (args,rst)
          | length args /= arity = Just . Left . mkErr
                                 $ map (detokenize . mapMaybe unscan) args
          | otherwise = Just $
                        do args' <- mapM expand args
                           let raw = map (detokenize . mapMaybe unscan) args
                           return $ Mask name : f (zip args' raw) ++
                                    Unmask name : rst

type EnvLookup = String -> Maybe (Macro, Env)

expandMacro :: Config -> Int -> EnvLookup -> String -> Scan -> [Scan]
            -> (DList Scan -> Maybe Env -> [Scan] -> Either Error r)
            -> Either Error r
expandMacro cfg lineNum env name tok ts k =
  case name of
    "__LINE__" -> simple $ show lineNum
    "__FILE__" -> simple . stringify $ curFileName cfg
    "__DATE__" -> simple . stringify . getDateString $ prepDate cfg
    "__TIME__" -> simple . stringify . getTimeString $ prepTime cfg
    _ -> case env name of
           Nothing -> k (tok:) Nothing ts
           Just (m,env') ->
             case m of
               Object t' ->
                 let expansion = Mask name
                               : map Rescan (spaced t')
                               ++ [Unmask name]
                 in k id (Just env') (expansion++ts)
               Function arity f ->
                 let ex = fmap snd
                        . expandLine' cfg lineNum (deleteKey name env')
                     err = argError lineNum name arity
                 in case expandFunction name arity f err ex ts of
                      Nothing -> k (tok:) (Just env') ts
                      -- FIXME: Missing call to spaced?
                      Just ts' ->
                        do tsEx <- ts'
                           k id (Just env') tsEx
  where simple s = k (Rescan (Important s):) Nothing ts
        -- Avoid accidentally merging tokens like @'-'@
        spaced xs = pre xs ++ pos
          where importantChar (Important [c]) = elem c oops
                importantChar _ = False
                pre = bool id (Other " ":)$
                      (maybe False importantChar $ listToMaybe xs)
                pos = bool [] [Other " "] $
                      (maybe False importantChar $ listToMaybe (reverse xs))
                oops = "-+*.><"

withMask :: Eq a => [a] -> (a -> Maybe b) -> a -> Maybe b
withMask mask f = \x -> if elem x mask then Nothing else f x

-- | Expand all macros on a /non-directive/ line. If there is a problem
-- expanding a macro (this will typically be a macro function), the
-- name of the name of the problematic macro is returned.
expandLine' :: Config -> Int -> Env -> [Scan]
           -> Either Error (Env, [Scan])
expandLine' cfg lineNum macros = go macros id []
  where go :: Env -> DList Scan -> [String] -> [Scan]
           -> Either Error (Env, [Scan])
        go env acc _ [] = Right (env, acc [])
        go env acc mask (tok@(Unmask name) : ts) =
          go env (acc . (tok:)) (delete name mask) ts
        go env acc mask (tok@(Mask name) : ts) =
          go env (acc . (tok:)) (name:mask) ts
        go env acc mask (tok@(Rescan (Important t)) : ts) =
          let envMasked = withMask mask (flip lookupKey env)
          in expandMacro cfg lineNum envMasked t tok ts $ \fAcc _ ts' ->
             go env (acc . fAcc) mask  ts'
        go env acc mask (tok@(Scan (Important t)) : ts) =
          let envLook = flip lookupKey env
          in expandMacro cfg lineNum envLook t tok ts $ \fAcc env' ts' ->
             go (maybe env id env') (acc . fAcc) mask ts'
        go env acc mask (t:ts) = go env (acc . (t:)) mask ts

-- | @breakBalance end tokens@ uses the first element of @tokens@ as
-- the start of a balanced pair, and @end@ as the end of such a
-- pair. It breaks @tokens@ into a prefix with as many @end@ as start
-- tokens, and the remaining tokens.
breakBalance :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe ([a],[a])
breakBalance _ _ [] = Nothing
breakBalance start end ts0 = go (1::Int) id ts0
  where go 0 acc ts = Just (acc [], ts)
        go _ _ [] = Nothing
        go n acc (t:ts)
          | end t = go (n-1) (acc . (t:)) ts
          | start t = go (n+1) (acc . (t:)) ts
          | otherwise = go n (acc . (t:)) ts

-- | Trim whitespace from both ends of a sequence of 'Scan' tokens.
trimScan :: [Scan] -> [Scan]
trimScan [] = []
trimScan (Scan (Other _):ts) = trimScan ts
trimScan (Rescan (Other _):ts) = trimScan ts
trimScan (t@(Rescan (Important _)) : ts) = t : trimScanAux Nothing ts
trimScan (t@(Scan (Important _)) : ts) = t : trimScanAux Nothing ts
trimScan (t@(Mask _) : ts) = t : trimScan ts
trimScan (t@(Unmask _) : ts) = t : trimScan ts

-- | Collapse internal whitespace to single spaces, and trim trailing
-- space.
trimScanAux :: Maybe Scan -> [Scan] -> [Scan]
trimScanAux _ [] = []
trimScanAux _ (Scan (Other _) : ts) = trimScanAux (Just (Scan (Other " "))) ts
trimScanAux _ (Rescan (Other _) : ts) = trimScanAux (Just (Scan (Other " "))) ts
trimScanAux spc (t@(Mask _) : ts) = t : trimScanAux spc ts
trimScanAux spc (t@(Unmask _) : ts) = t : trimScanAux spc ts
trimScanAux spc (t:ts) = maybe [] (:[]) spc ++  (t : trimScanAux Nothing ts)

-- | Parse a function application. Arguments are separated by commas,
-- and the application runs until a closing parenthesis. The input
-- stream should begin immediately /after/ the opening parenthesis.
argParse :: [Scan] -> Maybe ([[Scan]], [Scan])
argParse = fmap (first (map trimScan))
         . go id id
       <=< isApplication
         . dropWhile (maybe True not . fmap isImportant . unscan)
  where go accArgs accArg (t@(Scan (Important s)):ts) =
          aux accArgs accArg t ts s
        go accArgs accArg (t@(Rescan (Important s)):ts) =
          aux accArgs accArg t ts s
        go accArgs accArg (t : ts) = go accArgs (accArg . (t:)) ts
        go _ _ [] = Nothing
        aux accArgs accArg t ts s =
          case s of 
            ")" -> Just (accArgs [accArg []], ts)
            "," -> go (accArgs . (accArg [] :)) id ts
            "(" -> case breakBalance (isTok "(") (isTok ")") ts of
                     Nothing -> Nothing
                     Just (arg,ts') -> go accArgs (accArg . (t:) . (arg++)) ts'
            _ -> go accArgs (accArg . (t:)) ts
        isApplication (Scan (Important "("):ts) = Just ts
        isApplication (Rescan (Important "("):ts) = Just ts
        isApplication _ = Nothing
        isTok t (Scan (Important s)) = t == s
        isTok t (Rescan (Important s)) = t == s
        isTok _ _ = False
