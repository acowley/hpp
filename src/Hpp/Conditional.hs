{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}
-- | Parsing functionality for pre-processor conditionals.
module Hpp.Conditional (dropBranch, takeBranch) where
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup ((<>))
#endif
import Data.String (fromString)
import Hpp.Parser (replace, awaitJust, Parser)
import Hpp.Tokens (notImportant, Token(..))
import Hpp.Types (lineNum, use, HasHppState, HasError, LineNum, TOKEN, String)
import Prelude hiding (String)

yieldLineNum :: LineNum -> [TOKEN]
yieldLineNum !ln = [Important ("#line " <> fromString (show ln)), Other "\n"]

getCmd :: [TOKEN] -> Maybe String
getCmd = aux . dropWhile notImportant
  where aux (Important "#" : ts) = case dropWhile notImportant ts of
                                     (Important cmd:_) -> Just cmd
                                     _ -> Nothing
        aux _ = Nothing

dropBranchFun :: [[TOKEN]] -> (Int, [[TOKEN]])
dropBranchFun = go (1::Int) 0
  where go _ !n [] = (n,[])
        go !nesting !n (ln:lns) =
          case getCmd ln of
            Just cmd
              | cmd == "endif" -> if nesting == 1
                                  then (n, ln:lns)
                                  else go (nesting-1) (n+1) lns
              | cmd `elem` ["if","ifdef","ifndef"] ->
                go (nesting+1) (n+1) lns
              | cmd `elem` ["else","elif"] -> if nesting == 1
                                              then (n, ln : lns)
                                              else go nesting (n+1) lns
            _ -> go nesting (n+1) lns

-- | Take everything up to the end of this branch, drop all remaining
-- branches (if any).
takeBranch :: LineNum -> [[TOKEN]] -> [[TOKEN]]
takeBranch = go (1::Int)
  where go _ _ [] = [] -- error: unterminated conditional
        go 0 !n lns = yieldLineNum n : lns
        go !nesting !n (ln:lns) =
          case getCmd ln of
            Just cmd
              | cmd `elem` ["if","ifdef","ifndef"] ->
                ln : go (nesting+1) (n+1) lns
              | cmd == "endif" -> ln : go (nesting - 1) (n + 1) lns
              | nesting == 1 && cmd `elem` ["else","elif"] ->
                let (numSkipped, lns') = dropBranchFun lns
                in go 1 (n+1+numSkipped) lns'
            _ -> ln : go nesting (n+1) lns

-- | Drop the rest of a conditional expression incrementing the given
-- 'LineNum' by the number of lines skipped.
dropBranch :: (HasError m, HasHppState m, Monad m) => Parser m [TOKEN] ()
dropBranch = do ln <- use lineNum
                (el, numSkipped) <- dropBranchAux
                let ln' = ln + numSkipped
                replace (yieldLineNum ln')
                mapM_ replace el

-- | Skip to the end of a conditional branch. Returns the 'Just' the
-- token that ends this branch if it is an @else@ or @elif@, or
-- 'Nothing' otherwise, and the number of lines skipped.
dropBranchAux :: (HasError m, Monad m) => Parser m [TOKEN] (Maybe [TOKEN], Int)
dropBranchAux = go (1::Int) 0
  where go !nesting !n =
          do ln <- awaitJust "dropBranch"
             case getCmd ln of
               Just cmd
                 | cmd == "endif" -> if nesting == 1
                                     then return (Nothing, n+1)
                                     else go (nesting-1) (n+1)
                 | cmd `elem` ["if","ifdef","ifndef"] ->
                   go (nesting+1) (n+1)
                 | cmd `elem` ["else", "elif"] -> if nesting == 1
                                                  then return (Just ln, n+1)
                                                  else go nesting (n+1)
               _ -> go nesting (n+1)
