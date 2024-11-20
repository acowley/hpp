{-# LANGUAGE CPP, OverloadedStrings, ViewPatterns #-}
module Hpp.Macro (parseDefinition) where
import Data.Char (isSpace)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup ((<>))
#endif
import Hpp.StringSig
import Hpp.Tokens (trimUnimportant, importants, Token(..), isImportant)
import Hpp.Types (Macro(..), String, TOKEN, Scan(..), Variadic(..))
import Prelude hiding (String)
import qualified Data.List as List

-- * TOKEN Splices

-- | Deal with the two-character '##' token pasting/splicing
-- operator. We do so eliminating spaces around the @##@
-- operator.
prepTOKENSplices :: [TOKEN] -> [TOKEN]
prepTOKENSplices = map (fmap copy) . dropSpaces [] . mergeTOKENs []
  where -- Merges ## tokens, and reverses the input list
        mergeTOKENs acc [] = acc
        mergeTOKENs acc (Important "#" : Important "#" : ts) =
          mergeTOKENs (Important "##" : acc) (dropWhile (not . isImportant) ts)
        mergeTOKENs acc (t:ts) = mergeTOKENs (t : acc) ts
        -- Drop trailing spaces and re-reverse the list
        dropSpaces acc [] = acc
        dropSpaces acc (t@(Important "##") : ts) =
          dropSpaces (t : acc) (dropWhile (not . isImportant) ts)
        dropSpaces acc (t:ts) = dropSpaces (t : acc) ts

-- | Parse the definition of an object-like or function macro.
parseDefinition :: [TOKEN] -> Maybe (String, Macro)
parseDefinition toks =
  case dropWhile (not . isImportant) toks of
    (Important name:Important "(":rst) ->
      let params0 = takeWhile (/= ")") $ filter (/= ",") (importants rst)
          arity0  = length params0
          (params, arity, variadic) = case splitAt (arity0 - 3) params0 of
            (as, [".",".","."]) -> (as, arity0 - 3, Variadic)
            _                   -> (params0, arity0, NotVariadic)
          body = trimUnimportant . drop 1 $ dropWhile (/= Important ")") toks
          macro = Function variadic arity (functionMacro variadic arity params body)
      in Just (name, macro)
    (Important name:_) ->
      let rhs = case dropWhile (/= Important name) toks of
                  [] -> [Important ""]
                  str@(_:t)
                    | all (not . isImportant) str -> [Important ""]
                    | otherwise -> trimUnimportant t
      in Just (copy name, Object (map (fmap copy) rhs))
    _ -> Nothing

-- * Function-like macros as Haskell functions

-- | Drop spaces following @'#'@ characters.
prepStringify :: [TOKEN] -> [TOKEN]
prepStringify [] = []
prepStringify (Important "#" : ts) =
  case dropWhile (not . isImportant) ts of
    (Important t : ts') -> Important (cons '#' t) : prepStringify ts'
    _ -> Important "#" : ts
prepStringify (t:ts) = t : prepStringify ts

-- | Concatenate tokens separated by @'##'@.
paste :: [Scan] -> [Scan]
paste [] = []
paste (Rescan (Important s) : Rescan (Important "##") : Rescan (Important t) : ts) =
  paste (Rescan (Important (trimSpaces s <> sdropWhile isSpace t)) : ts)
paste (t:ts) = t : paste ts

-- | @functionMacro parameters body arguments@ substitutes @arguments@
-- for @parameters@ in @body@ and performs stringification for uses of
-- the @#@ operator and token concatenation for the @##@ operator.
functionMacro :: Variadic -> Int -> [String] -> [TOKEN] -> [([Scan],String)] -> [Scan]
functionMacro variadic arity params body args
  = paste . subst body' {- . M.fromList -} . zip params' $ args'
  where (args',var_args) = case variadic of
          NotVariadic -> (args,[])
          Variadic    -> List.splitAt arity args
        params' = map copy params
        subst toks gamma = go toks
          where go [] = []
                -- handle __VA_ARGS__ first
                go ((Important ","):(Important "##"):(Important "__VA_ARGS__"):ts)
                  | Variadic <- variadic
                  , [] <- var_args
                  = go ts -- GCC extension: we drop the leading comma if __VA_ARGS__ is empty
                go ((Important "__VA_ARGS__"):ts)
                  | Variadic <- variadic
                  = let vas = map (Rescan . Important . snd) var_args
                    in List.intersperse (Rescan (Important ",")) vas ++ go ts

                -- TODO: handle __VA_OPT__ here

                go (p@(Important "##"):t@(Important s):ts) =
                  case lookup s gamma of
                    Nothing -> Rescan p : Rescan t : go ts
                    Just (_,arg) ->
                      Rescan p : Rescan (Important arg) : go ts
                go (t@(Important s):p@(Important "##"):ts) =
                  case lookup s gamma of
                    Nothing -> Rescan t : go (p:ts)
                    Just (_,arg) -> Rescan (Important arg) : go (p:ts)
                go (t@(Important "##"):ts) = Rescan t : go ts
                go (t@(Important (uncons -> Just ('#',s))) : ts) =
                  case lookup s gamma of
                    Nothing -> Rescan t : go ts
                    Just (_,arg) ->
                      Rescan (Important (stringify arg)) : go ts
                go (t@(Important s) : ts) =
                  case lookup s gamma of
                    Nothing -> Rescan t : go ts
                    Just (arg,_) -> arg ++ go ts
                go (t:ts) = Rescan t : go ts
        body' = prepStringify . prepTOKENSplices $
                dropWhile (not . isImportant) body
