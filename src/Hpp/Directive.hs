{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables,
             ViewPatterns #-}
-- | Implement the logic of CPP directives (commands prefixed with an
-- octothorpe).
module Hpp.Directive (directive, macroExpansion) where
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict (StateT)
import Hpp.Conditional (dropBranch, takeBranch)
import Hpp.Config (curFileName, curFileNameF, ignoreUnknownDirectives)
import Hpp.Env (lookupKey, deleteKey, insertPair)
import Hpp.Expansion (expandLineState)
import Hpp.Expr (evalExpr, parseExpr)
import Hpp.Macro (parseDefinition)
import Hpp.Preprocessing (prepareInput)
import Hpp.StringSig (unquote, toChars)
import Hpp.Tokens (newLine, notImportant, trimUnimportant, detokenize, isImportant, Token(..))
import Hpp.Types
import Hpp.Parser (replace, await, insertInputSegment, takingWhile, droppingWhile, onInputSegment, evalParse, onElements, awaitJust, ParserT, Parser)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)
import Prelude hiding (String)

-- | Returns everything up to the next newline. The newline character
-- itself is consumed.
takeLine :: (Monad m, HasError m, HasHppState m) => Parser m [TOKEN] [TOKEN]
takeLine = (onElements $ do
              ln <- takingWhile (not . newLine)
              eat <- awaitJust "takeLine" -- Eat the newline character
              case eat of
                Other "\n" -> return ()
                wat -> error $ "Expected newline: "++show wat++" after "++show ln
              return ln)
           <* (lineNum %= (+1))

dropLine :: (Monad m, HasError m, HasHppState m) => Parser m [TOKEN] ()
dropLine = do onElements $ do
                droppingWhile (not . newLine)
                eat <- awaitJust "dropLine" -- Eat the newline character
                case eat of
                  Other "\n" -> return ()
                  wat -> error $ "Expected dropped newline: "++show wat
              lineNum %= (+1)

droppingSpaces ::(Monad m) => ParserT m src TOKEN ()
droppingSpaces = droppingWhile notImportant

-- | Run a Stream with a configuration for a new file.
--
-- Both 'curFileName' and 'dir' are switched to the new file: the
-- directory tracks the file currently being preprocessed so quoted
-- @#include "rel/path.h"@ directives nested inside @fp@ resolve
-- relative to @fp@'s own directory (C99 §6.10.2 "in a manner that
-- depends on the implementation"; gcc/clang look in the directory
-- of the file containing the @#include@). Without this, an included
-- file living in a subdirectory of the search path could not refer to
-- siblings via a quoted include — those would be searched only
-- relative to the original input's directory and the configured
-- include paths, never under the includer's own subdirectory.
--
-- @fp@ must be the /resolved/ path to the file being entered (the
-- one the include search actually located on disk), not the textual
-- @#include@ argument. Callers in 'includeAux' obtain it from
-- 'hppReadFile' / 'hppReadNext' which thread it back from
-- 'searchForInclude'. See Note [Resolved-path tracking for nested
-- includes] in pkg:Hpp.RunHpp.
streamNewFile :: (Monad m, HasHppState m)
              => FilePath -> [[TOKEN]] -> Parser m [TOKEN] ()
streamNewFile fp s =
  do (oldCfg,oldDir,oldLine) <- do st <- getState
                                   let cfg    = hppConfig st
                                       cfg'   = cfg { curFileNameF = pure fp }
                                       oldDir = hppCurDir st
                                       ln     = hppLineNum st
                                       newDir = takeDirectory fp
                                   setState (st { hppConfig  = cfg'
                                                , hppCurDir  = newDir
                                                , hppLineNum = 1
                                                })
                                   return (cfg, oldDir, ln)
     insertInputSegment
       s (getState >>= setState . setL lineNum oldLine
                                . setL config oldCfg
                                . setL dir oldDir)

-- | Handle preprocessor directives (commands prefixed with an octothorpe).
directive :: forall m. (Monad m, HasError m, HasHppState m, HasEnv m)
          => HppT [String] (Parser m [TOKEN]) Bool
directive = lift (onElements (awaitJust "directive")) >>= aux
  where aux :: TOKEN -> HppT [String] (Parser m [TOKEN]) Bool
        aux (Important cmd) = case cmd of
          "pragma" -> True <$ lift dropLine -- Ignored
          "define" -> True <$
                      (lift $ fmap parseDefinition takeLine >>= \case
                        Nothing -> use lineNum >>=
                                   throwError . BadMacroDefinition
                        Just def -> env %= insertPair def)
          "undef" -> do name <- lift . onElements $ do
                          droppingWhile (not . isImportant)
                          name <- awaitJust "undef" >>= \case
                                    Important n -> return n
                                    _ -> error "undef directive got Other token"
                          return name
                        lift dropLine
                        env %= deleteKey name
                        return True
          "include" -> True <$ includeAux hppReadFile
          "include_next" -> True <$ includeAux hppReadNext
          "line" -> do lift (onElements droppingSpaces)
                       toks <- lift (init <$> expandLineState)
                       case toks of
                         Important (toChars -> n):optFile ->
                           case readMaybe n of
                             Nothing -> use lineNum >>=
                                        throwError . flip BadLineArgument n
                             Just ln' -> do
                               unless (null optFile) $ do
                                 let fn = toChars . unquote . detokenize
                                        . dropWhile (not . isImportant)
                                        $ optFile
                                 config %= (\cfg -> cfg { curFileNameF = pure fn })
                               lineNum .= ln'
                               return True
                         _ -> use lineNum >>=
                              throwError
                              . flip BadLineArgument (toChars (detokenize toks))
          "ifdef" ->
            do toks <- lift (onElements droppingSpaces >> takeLine)
               ln <- use lineNum
               case takeWhile isImportant toks of
                 [Important t] ->
                   lookupMacro t >>= \case
                     Nothing ->
                       lift dropBranch
                     Just _ ->
                       lift (onInputSegment (takeBranch ln)) -- (takeBranch ln >>= precede)
                 _ -> throwError . UnknownCommand ln $
                      "ifdef "++ toChars (detokenize toks)
               return True
          "ifndef" ->
            do toks <- lift (onElements droppingSpaces >> takeLine)
               ln <- use lineNum
               case takeWhile isImportant toks of
                 [Important t] ->
                   lookupMacro t >>= \case
                      Nothing -> lift (onInputSegment (takeBranch ln)) -- takeBranch ln >>= precede)
                      Just _ -> lift dropBranch
                 _ -> throwError . UnknownCommand ln $
                      "ifndef "++ toChars (detokenize toks)
               return True
          "else" -> True <$ lift dropLine
          "if" -> True <$ ifAux
          "elif" -> True <$ ifAux
          "endif" -> True <$ lift dropLine
          "error" -> do toks <- lift (onElements droppingSpaces >> takeLine)
                        ln <- subtract 1 <$> use lineNum
                        curFile <- curFileName <$> use config
                        let tokStr = toChars (detokenize toks)
                        throwError $ UserError ln (tokStr++" ("++curFile++")")
          "warning" -> True <$ lift dropLine -- warnings not yet supported

          t -> do cfg <- use config
                  if ignoreUnknownDirectives cfg
                    then
                      -- Pass the line through as plain text. We have
                      -- only consumed the command token 't' so far;
                      -- returning False tells 'macroExpansion' to
                      -- emit the original line and call 'takeLine'
                      -- to advance the input — see the 'Important
                      -- "#":rst' branch in 'macroExpansion'.
                      pure False
                    else do
                      toks <- lift takeLine
                      ln <- subtract 1 <$> use lineNum
                      throwError $ UnknownCommand ln
                        (toChars (detokenize (Important t:toks)))
        aux _ = error "Impossible unimportant directive"
        includeAux :: (LineNum -> FilePath -> HppT src (Parser m [TOKEN]) (FilePath, [String]))
                   -> HppT src (Parser m [TOKEN]) ()
        includeAux readFun =
          do fileName <- lift (toChars . detokenize . trimUnimportant . init
                               <$> expandLineState)
             ln <- use lineNum
             prep <- prepareInput
             (resolvedPath, content) <- readFun ln fileName
             let src = prep content
             lineNum .= ln+1
             lift (streamNewFile resolvedPath src)
        {- SPECIALIZE includeAux ::
            (LineNum -> FilePath -> HppT [String] (Parser (StateT HppState (ExceptT Error IO)) [TOKEN]) (FilePath, [String]))
            -> HppT [String] (Parser (StateT HppState (ExceptT Error IO)) [TOKEN]) () #-}
        ifAux =
          do toks <- lift (onElements droppingSpaces >> takeLine)
             e <- use env
             ln <- use lineNum
             lineNum .= ln - 1 -- takeLine incremented the line count
             ex <- lift (lift (evalParse expandLineState [squashDefines e toks]))
             let res = evalExpr <$> parseExpr (map (fmap toChars) ex)
             lineNum .= ln
             if maybe False (/= 0) res
               then lift (onInputSegment (takeBranch ln)) -- (takeBranch ln >>= precede)
               else lift dropBranch
{-# SPECIALIZE directive ::
    HppT [String] (Parser (StateT HppState (ExceptT Error IO)) [TOKEN]) Bool #-}

-- | We want to expand macros in expressions that must be evaluated
-- for conditionals, but we want to take special care when dealing
-- with the meta @defined@ operator of the expression language that is
-- a predicate on the evaluation environment.
squashDefines :: Env -> [TOKEN] -> [TOKEN]
squashDefines _ [] = []
squashDefines env' (Important "defined" : ts) = go ts
  where go (t@(Other _) : ts') = t : go ts'
        go (t@(Important "(") : ts') = t : go ts'
        go (Important t : ts') =
          case lookupKey t env' of
            Nothing -> Important "0" : squashDefines env' ts'
            -- Just (_,env'') -> Important "1" : squashDefines env'' ts'
            Just _ -> Important "1" : squashDefines env' ts'
        go [] = []
squashDefines env' (t : ts) = t : squashDefines env' ts

-- | Expands an input line producing a stream of output lines.
macroExpansion :: (Monad m, HasHppState m, HasError m, HasEnv m)
               => HppT [String] (Parser m [TOKEN]) (Maybe [TOKEN])
macroExpansion = do
  lift await >>= \case
    Nothing -> return Nothing
    Just ln ->
      -- when (not (all isSpace (detokenize ln)))
      --      (trace ("macro expand: "++detokenize ln) (return ())) >>
      case dropWhile notImportant ln of
        [] -> Just ln <$ (lineNum %= (+1))
        Important "#":rst -> do lift (replace (dropWhile notImportant rst))
                                processed <- directive
                                if processed
                                then macroExpansion
                                else Just ln <$ lift takeLine
        _ -> lift (replace ln >> (Just <$> expandLineState)) <* (lineNum %= (+1))
