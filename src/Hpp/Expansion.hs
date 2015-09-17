{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
-- | Line expansion is the core input token processing
-- logic. Object-like macros are substituted, and function-like macro
-- applications are expanded.
module Hpp.Expansion (expandLine) where
import Control.Monad (forM)
import Data.Bool (bool)
import Data.Foldable (foldl', traverse_)
import Data.List (delete)
import Data.Maybe (listToMaybe, mapMaybe)
import Hpp.Config (Config, curFileName,
                   getDateString, getTimeString, prepDate, prepTime)
import Hpp.Env (lookupKey, deleteKey)
import Hpp.Streamer (source, mapTil)
import Hpp.Parser (Parser, awaitP, liftP, precede, zoomParse,
                   droppingWhile, awaitJust, replace, parse)
import Hpp.String (stringify)
import Hpp.Tokens (Token(..), notImportant, isImportant, detokenize)
import Hpp.Types (HasError(..), HasEnv(..), Scan(..), Error(..), Macro(..))

-- | Extract the 'Token' payload from a 'Scan'.
unscan :: Scan -> Maybe Token
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
           => Config -> Int -> Parser m Token [Token]
expandLine cfg lineNum = fmap (mapMaybe unscan) $
                         zoomParse (mapTil Scan)
                                   (expandLine' True cfg lineNum)

expandLine' :: forall m. (HasError m, Monad m, HasEnv m)
            => Bool -> Config -> Int -> Parser m Scan [Scan]
expandLine' oneLine cfg lineNum = go id []
  where go :: ([Scan] -> [Scan]) -> [String] -> Parser m Scan [Scan]
        go acc mask = awaitP >>= maybe (return $ acc []) aux
          where aux :: Scan -> Parser m Scan [Scan]
                aux tok = case tok of
                  Unmask name -> go (acc . (tok:)) (delete name mask)
                  Mask name -> go (acc . (tok:)) (name : mask)
                  Scan (Important t) -> do ts <- expandMacro cfg lineNum t tok
                                           if ts == [tok]
                                           then go (acc . (tok:)) mask
                                           else do precede $ source ts
                                                   go acc mask
                  Rescan (Important t) ->
                    do oldEnv <- liftP $
                                 do env <- getEnv
                                    setEnv $ foldl' (flip deleteKey) env mask
                                    return env
                       ts <- expandMacro cfg lineNum t tok
                       liftP $ setEnv oldEnv
                       if ts == [tok]
                       then go (acc . (tok:)) mask
                       else precede (source ts) >> go acc mask
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
appParse :: (Monad m, HasError m) => Parser m Scan (Maybe [[Scan]])
appParse = droppingWhile isSpaceScan >> checkApp
  where imp = maybe True notImportant . unscan
        checkApp = do tok <- droppingWhile imp >> awaitP
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
argParse :: (Monad m, HasError m) => Parser m Scan [Scan]
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
parenthetical :: (Monad m, HasError m) => Parser m Scan [Scan]
parenthetical = go id (1::Int)
  where go acc 0 = return (acc [])
        go acc n = do tok <- awaitJust "parenthetical"
                      case unscan tok of
                        Just (Important "(") -> go (acc . (tok:)) (n+1)
                        Just (Important ")") -> go (acc . (tok:)) (n-1)
                        _ -> go (acc . (tok:)) n

argError :: Int -> String -> Int -> [String] -> Error
argError lineNum name arity args =
  TooFewArgumentsToMacro lineNum $ name++"<"++show arity++">"++show args

-- | Returns 'Nothing' if this isn't an application; @Left args@ if we
-- parsed arguments @args@, but there is an arity mismatch; or @Right
-- tokens@ if the function application expanded successfully.
expandFunction :: (Monad m, HasError m)
               => String -> Int -> ([([Scan],String)] -> [Scan])
               -> (forall r'. [String] -> Parser m Scan r')
               -> Parser m Scan [Scan]
               -> Parser m Scan (Maybe [Scan])
expandFunction name arity f mkErr expand =
  do margs <- appParse
     case margs of
       Nothing -> return Nothing
       Just args
         | length args /= arity -> mkErr $
                                   map (detokenize . mapMaybe unscan) args
         | otherwise ->
           do args' <- forM args $ \arg -> liftP (parse expand (source arg))
              let raw = map (detokenize . mapMaybe unscan) args
              return . Just $ Mask name : f (zip args' raw) ++ [Unmask name]

lookupEnv :: (Monad m, HasEnv m)
          => String -> Parser m i (Maybe Macro)
lookupEnv s = liftP $ getEnv >>= traverse aux . lookupKey s
  where aux (m, env') = setEnv env' >> return m

expandMacro :: (Monad m, HasError m, HasEnv m)
            => Config -> Int -> String -> Scan -> Parser m Scan [Scan]
expandMacro cfg lineNum name tok =
  case name of
    "__LINE__" -> simple $ show lineNum
    "__FILE__" -> simple . stringify $ curFileName cfg
    "__DATE__" -> simple . stringify . getDateString $ prepDate cfg
    "__TIME__" -> simple . stringify . getTimeString $ prepTime cfg
    _ -> do mm <- lookupEnv name
            case mm of
              Nothing -> return [tok]
              Just m ->
                case m of
                  Object t' ->
                    return $ Mask name : map Rescan (spaced t') ++ [Unmask name]
                  Function arity f ->
                    let ex = expandLine' False cfg lineNum
                        err = liftP . throwError
                            . argError lineNum name arity
                    in do mts <- expandFunction name arity f err ex
                          case mts of
                            Nothing -> return [tok]
                            Just ts -> return ts
  where simple s = return [Rescan (Important s)]
        -- Avoid accidentally merging tokens like @'-'@
        spaced xs = pre ++ pos
          where importantChar (Important [c]) = elem c oops
                importantChar _ = False
                pre = bool xs (Other " ":xs)$
                      (maybe False importantChar $ listToMaybe xs)
                pos = bool [] [Other " "] $
                      (maybe False importantChar $ listToMaybe (reverse xs))
                oops = "-+*.><"

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
