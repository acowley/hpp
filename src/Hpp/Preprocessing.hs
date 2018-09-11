{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}
-- | The simplest pre-processing steps are represented as distinct
-- passes over input lines.
module Hpp.Preprocessing
  (
    trigraphReplacement
  , lineSplicing
  , cCommentRemoval
  , cCommentAndTrigraph
  , prepareInput
  ) where
import Control.Arrow (first)
import Data.Char (isSpace)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup ((<>))
#endif
import Data.String (fromString)
import Hpp.Config
import Hpp.StringSig
import Hpp.Tokens (tokenize, Token(..), skipLiteral)
import Hpp.Types (TOKEN, String, HasHppState, getState, config, getL)
import Prelude hiding (String)

-- * Trigraphs

-- | The first component of each pair represents the end of a known
-- trigraph sequence (each trigraph begins with two consecutive
-- question marks (@\"??\"@). The second component is the
-- single-character equivalent that we substitute in for the trigraph.
trigraphs :: [(Char, Char)]
trigraphs = [ ('=', '#')
            , ('/', '\\')
            , ('\'', '^')
            , ('(', '[')
            , (')', ']')
            , ('!', '|')
            , ('<', '{')
            , ('>', '}')
            , ('-', '~') ]

trigraphReplacement :: Stringy s => s -> s
trigraphReplacement s = aux (breakOn [("??", ())] s)
  where aux Nothing = s
        aux (Just (_, pre, pos)) =
          case uncons pos of
            Nothing -> pre <> "??"
            Just (c,t) ->
              case lookup c trigraphs of
                Just c' -> snoc pre c' <> trigraphReplacement t
                Nothing -> snoc pre '?' <> trigraphReplacement (cons '?' pos)

-- * Line Splicing

-- | If a line ends with a backslash, it is prepended to the following
-- the line.
lineSplicing :: Stringy s => [s] -> [s]
lineSplicing = go id
  where go acc [] = [acc mempty]
        go acc (ln:lns) = case unsnoc ln of
                            Nothing -> acc mempty : go id lns
                            Just (ini, '\\') -> go (acc . (ini<>)) lns
                            Just _ -> acc ln : go id lns
{-# INLINE lineSplicing #-}

-- * C Comments

breakBlockCommentStart :: Stringy s => s -> Maybe (s, s)
breakBlockCommentStart s =
  case breakCharOrSub '"' "/*" s of
    NoMatch -> Nothing
    CharMatch pre pos -> let (lit, rest) = skipLiteral pos
                         in first ((pre <> lit) <>) <$>
                            breakBlockCommentStart rest
    SubMatch pre pos -> Just (pre, pos)

breakBlockCommentEnd :: Stringy s => s -> Maybe s
breakBlockCommentEnd s =
  case breakCharOrSub '"' "*/" s of
    NoMatch -> Nothing
    CharMatch _ pos -> let (_, rest) = skipLiteral pos
                       in breakBlockCommentEnd rest
    SubMatch _ pos -> Just pos

dropOneLineBlockComments :: Stringy s => s -> s
dropOneLineBlockComments s =
  case breakCharOrSub '"' "/*"s of
    NoMatch -> s
    CharMatch pre pos ->
      let (lit,rest) = skipLiteral pos
      in snoc pre '"' <> lit <> dropOneLineBlockComments rest
    SubMatch pre pos ->
      case breakOn [("*/", ())] pos of
        Nothing -> pre <> "/*"
        Just (_,_,pos') -> snoc pre ' ' <> dropOneLineBlockComments pos'

removeMultilineComments :: Stringy s => Int -> [s] -> [s]
removeMultilineComments !lineStart = goStart lineStart
  where goStart _ [] = []
        goStart !curLine (ln:lns) =
          case breakBlockCommentStart ln of
            Nothing -> ln : goStart (curLine+1) lns
            Just (pre,_) -> goEnd (curLine+1) pre lns
        goEnd _ _ [] = error "Unmatched /*"
        goEnd !curLine pre (ln:lns) =
          case breakBlockCommentEnd ln of
            Nothing -> goEnd (curLine+1) pre lns
            Just pos
              | sall isSpace (pre<>pos) ->
                ("#line "<> fromString (show (curLine+1))) : goStart (curLine + 1) lns
              | otherwise -> (pre<>pos)
                             : ("#line "<> fromString (show (curLine+1)))
                             : goStart (curLine+1) lns

-- | Remove C-style comments bracketed by @/*@ and @*/@.
cCommentRemoval :: Stringy s => [s] -> [s]
cCommentRemoval = removeMultilineComments 1 . map dropOneLineBlockComments

-- | Remove C-style comments bracked by @/*@ and @*/@ and perform
-- trigraph replacement.
cCommentAndTrigraph :: Stringy s => [s] -> [s]
cCommentAndTrigraph = removeMultilineComments 1
                    . map (dropOneLineBlockComments . trigraphReplacement)

prepareInput :: (Monad m, HasHppState m) => m ([String] -> [[TOKEN]])
prepareInput =
  do cfg <- getL config <$> getState
     case () of
       _ | eraseCComments cfg && spliceLongLines cfg
           && not (inhibitLinemarkers cfg) -> pure normalCPP
       _ | (eraseCComments cfg && spliceLongLines cfg
            && (not (replaceTrigraphs cfg))) ->
           pure haskellCPP
       _ | not (any ($ cfg) [ eraseCComments
                            , spliceLongLines
                            , replaceTrigraphs ]) -> pure onlyMacrosCPP
       _ | otherwise -> pure (genericConfig cfg)

-- * HPP configurations

-- | Standard CPP settings for processing C files.
normalCPP :: [String] -> [[TOKEN]]
normalCPP = map ((++ [Other "\n"]) . tokenize)
          . lineSplicing
          . cCommentAndTrigraph
{-# INLINABLE normalCPP #-}

-- | For Haskell we do not want trigraph replacement.
haskellCPP :: [String] -> [[TOKEN]]
haskellCPP = map ((++[Other "\n"]) . tokenize)
           . lineSplicing
           . cCommentRemoval
{-# INLINABLE haskellCPP #-}

-- | No C-style comment removal; no line splicing; no trigraph
-- replacement. This variant only supports macros and conditionals.
onlyMacrosCPP :: [String] -> [[TOKEN]]
onlyMacrosCPP = map ((++[Other "\n"]) . tokenize)
{-# INLINABLE onlyMacrosCPP #-}

-- | If we don't have a predefined processor, we build one based on a
-- 'Config' value.
genericConfig :: Config -> [String] -> [[TOKEN]]
genericConfig cfg = map ((++ [Other "\n"]) . tokenize)
                  . (if spliceLongLines cfg then lineSplicing else id)
                  . (if eraseCComments cfg then cCommentRemoval else id)
                  . (if replaceTrigraphs cfg then map trigraphReplacement else id)
