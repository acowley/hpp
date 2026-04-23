{-# LANGUAGE BangPatterns, CPP, OverloadedStrings, MultiWayIf #-}
-- | The simplest pre-processing steps are represented as distinct
-- passes over input lines.
module Hpp.Preprocessing
  (
    trigraphReplacement
  , lineSplicing
  , cCommentRemoval
  , cCommentAndTrigraph
  , gateHaskellComments
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
import Hpp.Tokens (tokenize, Token(..), detokenize, skipLiteral)
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
                         in first ((snoc pre '"' <> lit) <>) <$>
                            breakBlockCommentStart rest
    SubMatch pre pos -> Just (pre, pos)

breakBlockCommentEnd :: Stringy s => s -> Maybe s
breakBlockCommentEnd s =
  case breakOn [("*/",())] s of
    Nothing -> Nothing
    Just (_,_,pos) -> Just pos

dropOneLineComments :: Stringy s => s -> s
dropOneLineComments s =
  case breakCharOrSub '"' "//" s of
    NoMatch -> s
    CharMatch pre pos ->
      let (lit,rest) = skipLiteral pos
      in snoc pre '"' <> lit <> dropOneLineComments rest
    SubMatch pre _pos -> pre

dropOneLineBlockComments :: Stringy s => s -> s
dropOneLineBlockComments s =
  case breakCharOrSub '"' "/*" s of
    NoMatch -> s
    CharMatch pre pos ->
      let (lit,rest) = skipLiteral pos
      in snoc pre '"' <> lit <> dropOneLineBlockComments rest
    SubMatch pre pos ->
      case breakOn [("*/", ())] pos of
        Nothing -> pre <> "/*" <> pos
        Just (_,_,pos')
          | isEmpty pos' -> pre
          -- only add a space if we're not at the end of the line
          | otherwise    -> snoc pre ' ' <> dropOneLineBlockComments pos'

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

-- | Remove C-style comments bracketed by @/*@ and @*/@ and one-line '//'
-- comments.
cCommentRemoval :: Stringy s => [s] -> [s]
cCommentRemoval = cCommentRemoval' False

-- | Remove C-style comments bracked by @/*@ and @*/@ and perform
-- trigraph replacement.
cCommentAndTrigraph :: Stringy s => [s] -> [s]
cCommentAndTrigraph = cCommentRemoval' True

-- | Remove C-style comments bracketed by @/*@ and @*/@ and one-line '//'
-- comments, and optionally perform trigraph replacement.
cCommentRemoval' :: Stringy s => Bool -> [s] -> [s]
cCommentRemoval' do_trigraphs =
  -- important: drop '//' comments last, otherwise we would try to remove '//'
  -- comments into block comments. For example:
  --    <https://www.foo.org>. */
  --  would become
  --    <https:
  map dropOneLineComments
  . removeMultilineComments 1
  . map dropOneLineBlockComments
  . (if do_trigraphs then map trigraphReplacement else id)

-- * Haskell comments

-- Note [Gating directive dispatch inside Haskell comments]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- HPP is a C/C++ preprocessor that spots directives by the rule "the
-- first non-whitespace token on a line is an Important '#'". In
-- Haskell sources that rule misfires on the closing brace of a
-- multi-line LANGUAGE pragma:
--
--     {-# LANGUAGE CPP
--               , OverloadedStrings
--       #-}
--
-- The third line starts with @#-}@, which HPP would otherwise dispatch
-- as the unknown directive @-}@.
--
-- When 'ignoreHaskellComments' is enabled we run a small stateful
-- pass ('gateHaskellComments') over the /tokenized/ line stream. It
-- tracks Haskell lexical state across lines: nestable @{- ... -}@
-- block comments, @-- ...@ line comments, @\"...\"@ string literals,
-- and GHC's 'MultilineStrings' extension (a literal opened with
-- @\"\"\"@ and closed by the next unescaped @\"\"\"@, which may span
-- many lines).
--
-- For any line that /starts/ inside an open block comment or inside
-- an open multi-line string we demote a leading @Important \"#\"@
-- token to @Other \"#\"@. That keeps the @#@ character in the output
-- byte-for-byte (detokenize concatenates all tokens indiscriminately)
-- while making the directive detector in "Hpp.Directive" skip it.
-- Multi-line strings need this for the same reason block comments do
-- — a body line that begins with @#@ would otherwise dispatch as a
-- directive even though, in Haskell, it is just string content.
--
-- We deliberately do /not/ rewrite the text of comments or strings,
-- and we do /not/ suppress macro expansion inside them — the only
-- change is that a stray leading @#@ sitting inside an open block
-- comment or multi-line string no longer trips directive dispatch.

-- | Lexical states tracked by 'gateHaskellComments'.
data HsLex = HsCode
           | HsBlockCmt {-# UNPACK #-} !Int
           | HsLineCmt
           | HsString
           | HsMultiString

-- | See Note [Gating directive dispatch inside Haskell comments].
gateHaskellComments :: [[TOKEN]] -> [[TOKEN]]
gateHaskellComments = go HsCode
  where
    go _ []         = []
    go st (ln:lns)  =
      let demoteHere = case st of
                         HsBlockCmt _  -> True
                         HsMultiString -> True
                         _             -> False
          ln'  | demoteHere = demoteLeadingHash ln
               | otherwise  = ln
          stEnd = walk st (toChars (detokenize ln))
          -- A Haskell line comment terminates at the physical line
          -- break, so any HsLineCmt state at end-of-line resets to
          -- HsCode for the following line.
          stNext = case stEnd of HsLineCmt -> HsCode; s -> s
      in ln' : go stNext lns

    -- Demote the first Important "#" encountered to an Other token so
    -- the directive detector skips it. Leading whitespace / Other
    -- tokens pass through unchanged. If the line's first Important
    -- token is not "#", we leave it alone.
    demoteLeadingHash :: [TOKEN] -> [TOKEN]
    demoteLeadingHash (Other s : rest) = Other s : demoteLeadingHash rest
    demoteLeadingHash (Important "#" : rest) = Other "#" : rest
    demoteLeadingHash ln = ln

    walk :: HsLex -> [Char] -> HsLex
    walk st []                          = st
    walk HsCode ('{':'-':rest)          = walk (HsBlockCmt 1) rest
    walk HsCode ('-':'-':rest)          = walk HsLineCmt rest
    -- The triple-quote pattern must precede the single-quote one:
    -- Haskell pattern matching is order-sensitive and a leading
    -- @\"@ would otherwise win and start a regular string.
    walk HsCode ('"':'"':'"':rest)      = walk HsMultiString rest
    walk HsCode ('"':rest)              = walk HsString rest
    walk HsCode (_:rest)                = walk HsCode rest
    walk (HsBlockCmt n) ('{':'-':rest)  = walk (HsBlockCmt (n+1)) rest
    walk (HsBlockCmt n) ('-':'}':rest)  =
      walk (if n <= 1 then HsCode else HsBlockCmt (n-1)) rest
    walk (HsBlockCmt n) (_:rest)        = walk (HsBlockCmt n) rest
    walk HsLineCmt (_:rest)             = walk HsLineCmt rest
    -- Inside a string literal, backslash escapes consume the next
    -- character so an escaped @\\\"@ doesn't prematurely close it.
    walk HsString ('\\':_:rest)         = walk HsString rest
    walk HsString ('"':rest)            = walk HsCode rest
    walk HsString (_:rest)              = walk HsString rest
    -- Multi-line strings (GHC's MultilineStrings extension): same
    -- escape handling as a regular string, but the close delimiter
    -- is @\"\"\"@. The escape pattern must come first so an escaped
    -- quote inside the body — e.g. @\\\"@ — does not contribute to
    -- a stray triple-quote terminator.
    walk HsMultiString ('\\':_:rest)        = walk HsMultiString rest
    walk HsMultiString ('"':'"':'"':rest)   = walk HsCode rest
    walk HsMultiString (_:rest)             = walk HsMultiString rest

prepareInput :: (Monad m, HasHppState m) => m ([String] -> [[TOKEN]])
prepareInput =
  do cfg <- getL config <$> getState
     let gate | ignoreHaskellComments cfg = gateHaskellComments
              | otherwise                 = id
     let cpp = if
          | eraseCComments cfg
          , spliceLongLines cfg
          , not (inhibitLinemarkers cfg)
          -> normalCPP

          | eraseCComments cfg
          , spliceLongLines cfg
          , not (replaceTrigraphs cfg)
          -> haskellCPP

          | not (eraseCComments cfg)
          , not (spliceLongLines cfg)
          , not (replaceTrigraphs cfg)
          -> onlyMacrosCPP

          | otherwise
          -> genericConfig cfg
     pure (gate . cpp)

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
