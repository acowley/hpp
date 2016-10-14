{-# LANGUAGE LambdaCase, OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
-- | Line expansion is the core input token processing
-- logic. Object-like macros are substituted, and function-like macro
-- applications are expanded.
module Hpp.Expansion (expandLine) where
import Control.Monad.Trans.Class (lift)
import Data.Bool (bool)
import Data.Foldable (foldl', traverse_)
import Data.List (delete)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Hpp.Config (Config, curFileName,
                   getDateString, getTimeString, prepDate, prepTime)
import Hpp.Env (lookupKey, deleteKey)
import Hpp.Parser (Parser, ParserT, precede, replace, await, onIsomorphism,
                   onElements, droppingWhile, awaitJust, evalParse)
import Hpp.StringSig (stringify, uncons, isEmpty, toChars)
import Hpp.Tokens (Token(..), notImportant, isImportant, detokenize)
import Hpp.Types (HasError(..), HasEnv(..), Scan(..), Error(..), Macro(..),
                  TOKEN, String)
import Prelude hiding (String)

-- | Extract the 'TOKEN' payload from a 'Scan'.
unscan :: Scan -> Maybe TOKEN
unscan (Scan t) = Just t
unscan (Rescan t) = Just t
unscan _ = Nothing

isSpaceScan :: Scan -> Bool
isSpaceScan = maybe False notImportant . unscan

isImportantScan :: Scan -> Bool
isImportantScan = maybe False isImportant . unscan

-- | Expand all macros to the end of the current line or until all
-- in-progress macro invocations are complete, whichever comes last.
expandLine :: (HasError m, Monad m, HasEnv m)
           => Config -> Int -> Parser m [TOKEN] [TOKEN]
expandLine cfg lineNum =
  mapMaybe unscan <$>
  onElements (onIsomorphism Scan unscan (expandLine' True cfg lineNum))

expandLine' :: forall m src. (HasError m, Monad m, HasEnv m)
            => Bool -> Config -> Int -> ParserT m src Scan [Scan]
expandLine' oneLine cfg lineNum = go id []
  where go :: ([Scan] -> [Scan]) -> [String] -> ParserT m src Scan [Scan]
        go acc mask = await >>= maybe (return $ acc []) aux
          where aux :: Scan -> ParserT m src Scan [Scan]
                aux tok = case tok of
                  Unmask name -> go (acc . (tok:)) (delete name mask)
                  Mask name -> go (acc . (tok:)) (name : mask)
                  Scan (Important t) -> do ts <- expandMacro cfg lineNum t tok
                                           if ts == [tok]
                                           then go (acc . (tok:)) mask
                                           else precede ts >> go acc mask
                  Rescan (Important t) ->
                    do oldEnv <- lift $
                                 do env <- getEnv
                                    setEnv $ foldl' (flip deleteKey) env mask
                                    return env
                       ts <- expandMacro cfg lineNum t tok
                       lift $ setEnv oldEnv
                       if ts == [tok]
                       then go (acc . (tok:)) mask
                       else precede ts >> go acc mask
                  Scan (Other "\n")
                    | oneLine -> return (acc [tok])
                    | otherwise -> go (acc . (tok:)) mask
                  Rescan (Other "\n")
                    | oneLine -> return (acc [tok])
                    | otherwise -> go (acc . (tok:)) mask
                  _ -> go (acc . (tok:)) mask

-- | Parse a function application. Arguments are separated by commas,
-- and the application runs until the balanced closing parenthesis. If
-- this is not an application, 'Nothing' is returned.
appParse :: (Monad m, HasError m)
         => ParserT m src Scan (Maybe [[Scan]])
appParse = droppingWhile isSpaceScan >> checkApp
  where imp = maybe True notImportant . unscan
        checkApp = do tok <- droppingWhile imp >> await
                      case tok >>= unscan of
                        Just (Important "(") -> goApp
                        _ -> traverse_ replace tok >> return Nothing
        getArg acc = do arg <- fmap trimScan argParse
                        tok <- awaitJust "appParse getArg"
                        case unscan tok of
                          Just (Important ")") -> return (acc [arg])
                          _ -> replace tok >> getArg (acc . (arg:))
        goApp = fmap Just (getArg id)

-- | Emit the tokens of a single argument. Returns 'True' if this is
-- the final argument in an application (indicated by an unbalanced
-- closing parenthesis.
argParse :: (Monad m, HasError m) => ParserT m src Scan [Scan]
argParse = go id
  where go acc = do tok <- awaitJust "argParse"
                    case unscan tok of
                      Just (Important s)
                        | s == ")" -> replace tok >> return (acc [])
                        | s == "," -> return (acc [])
                        | s == "(" -> do ts <- fmap (tok:) parenthetical
                                         go (acc . (ts++))
                        | otherwise -> go (acc . (tok:))
                      _ -> go (acc . (tok:))

-- | Kick this off after an opening parenthesis and it will yield
-- every token up to the closing parenthesis.
parenthetical :: (Monad m, HasError m) => ParserT m src Scan [Scan]
parenthetical = go id (1::Int)
  where go acc 0 = return (acc [])
        go acc n = do tok <- awaitJust "parenthetical"
                      case unscan tok of
                        Just (Important "(") -> go (acc . (tok:)) (n+1)
                        Just (Important ")") -> go (acc . (tok:)) (n-1)
                        _ -> go (acc . (tok:)) n

argError :: Int -> String -> Int -> [String] -> Error
argError lineNum name arity args =
  TooFewArgumentsToMacro lineNum $
  toChars name <> "<" <> show arity <> ">" <> show args

-- | Returns 'Nothing' if this isn't an application; @Left args@ if we
-- parsed arguments @args@, but there is an arity mismatch; or @Right
-- tokens@ if the function application expanded successfully.
expandFunction :: (Monad m, HasError m)
               => String -> Int -> ([([Scan],String)] -> [Scan])
               -> (forall r'. [String] -> ParserT m src Scan r')
               -> ([Scan] -> ParserT m src Scan [Scan])
               -> ParserT m src Scan (Maybe [Scan])
expandFunction name arity f mkErr expand =
  do margs <- appParse
     case margs of
       Nothing -> return Nothing
       Just args
         | length args /= arity -> mkErr $
                                   map (detokenize . mapMaybe unscan) args
         | otherwise ->
           do args' <- mapM expand args
              let raw = map (detokenize . mapMaybe unscan) args
              return . Just $ Mask name : f (zip args' raw) ++ [Unmask name]

lookupEnv :: (Monad m, HasEnv m)
          => String -> ParserT m src Scan (Maybe Macro)
lookupEnv s = lift $ getEnv >>= traverse aux . lookupKey s
  where aux (m, env') = setEnv env' >> return m

expandMacro :: (Monad m, HasError m, HasEnv m)
            => Config -> Int -> String -> Scan -> ParserT m src Scan [Scan]
expandMacro cfg lineNum name tok =
  case name of
    "__LINE__" -> simple . fromString $ show lineNum
    "__FILE__" -> simple . stringify . fromString $ curFileName cfg
    "__DATE__" -> simple . stringify . fromString . getDateString $ prepDate cfg
    "__TIME__" -> simple . stringify . fromString . getTimeString $ prepTime cfg
    _ -> do mm <- lookupEnv name
            case mm of
              Nothing -> return [tok]
              Just m ->
                case m of
                  Object t' ->
                    return $ Mask name : map Rescan (spaced t') ++ [Unmask name]
                  Function arity f ->
                    let ex = expandLine' False cfg lineNum
                        err = lift . throwError
                            . argError lineNum name arity
                    in do mts <- expandFunction name arity f err
                                                (lift . evalParse ex)
                          case mts of
                            Nothing -> return [tok]
                            Just ts -> return ts
  where simple s = return [Rescan (Important s)]
        -- Avoid accidentally merging tokens like @'-'@
        spaced xs = pre <> pos
          where importantChar (Important t) =
                  case uncons t of
                    Nothing -> False
                    Just (c,t') -> elem c oops && isEmpty t'
                importantChar _ = False
                pre = bool xs (Other " ":xs)$
                      (maybe False importantChar $ listToMaybe xs)
                pos = bool [] [Other " "] $
                      (maybe False importantChar $ listToMaybe (reverse xs))
                oops = "-+*.><" :: [Char]

-- | Trim whitespace from both ends of a sequence of 'Scan' tokens.
trimScan :: [Scan] -> [Scan]
trimScan [] = []
trimScan (t:ts) | isSpaceScan t = trimScan ts
                | isImportantScan t = t : trimScanAux Nothing ts
                | otherwise = t : trimScan ts

-- | Collapse internal whitespace to single spaces, and trim trailing
-- space.
trimScanAux :: Maybe Scan -> [Scan] -> [Scan]
trimScanAux _ [] = []
trimScanAux spc (t : ts)
  | isSpaceScan t = trimScanAux (Just (Scan (Other " "))) ts
  | isImportantScan t = maybe [] (:[]) spc ++ (t : trimScanAux Nothing ts)
  | otherwise = t : trimScanAux spc ts
