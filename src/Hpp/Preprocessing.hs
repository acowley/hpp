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
import qualified Data.ByteString.Char8 as B
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup ((<>))
#endif
import Data.String (fromString)
import Hpp.Config
import Hpp.StringSig
import Hpp.Tokens (tokenize, Token(..), detok, detokenize, skipLiteral)
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

-- Note [Treating Haskell comments and strings as opaque to CPP]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- HPP is a C/C++ preprocessor; when it preprocesses Haskell sources
-- it must NOT interpret tokens that live inside Haskell comments or
-- string literals. Two distinct misfires used to trip:
--
--   1. The closing brace of a multi-line LANGUAGE pragma:
--        {-# LANGUAGE CPP
--                  , OverloadedStrings
--          #-}
--      starts the third line with @#-}@, which HPP would dispatch as
--      the unknown directive @-}@.
--
--   2. A bare identifier inside a @-- …@ line comment (or any other
--      Haskell comment / string body) whose spelling matches a
--      function-like macro defined earlier in the include chain.
--      Concretely, the GHC RTS chain defines @REG(x) __asm__(\"%\" #x)@
--      in @stg/MachRegs/x86.h@; @compiler/CodeGen.Platform.h@ later
--      contains the prose
--        -- Normally, the register names are just stringified as
--        -- part of the REG() macro
--      which HPP would otherwise tokenize as a real @REG()@ call,
--      reporting @TooFewArgumentsToMacro@.
--
-- 'gateHaskellComments' (enabled by 'ignoreHaskellComments' in the
-- config) handles both: we track Haskell lexical state across lines
-- (nestable @{- … -}@, @-- …@, @\"…\"@, and the @\"\"\"…\"\"\"@
-- multi-line string from GHC's 'MultilineStrings' extension) and
-- demote any 'Important' token whose starting character lies inside
-- one of those opaque regions to 'Other'. After detokenization the
-- byte content of the line is unchanged (Other and Important
-- detokenize identically), so the comment/string still reaches the
-- output verbatim; only the directive detector and the macro
-- expander, both of which only consume 'Important' tokens, are
-- redirected past the region.

-- | Lexical states tracked by 'gateHaskellComments'.
data HsLex = HsCode
           | HsBlockCmt {-# UNPACK #-} !Int
           | HsLineCmt
           | HsString
           | HsMultiString
  deriving (Eq, Show)

-- | See Note [Treating Haskell comments and strings as opaque to CPP].
gateHaskellComments :: [[TOKEN]] -> [[TOKEN]]
gateHaskellComments = go HsCode
  where
    go _ []         = []
    go st (ln:lns)  =
      let chars  = toChars (detokenize ln)
          trace  = walkTraced st chars
          stEnd  = case trace of
                     [] -> st
                     _  -> snd (last trace)
          -- A Haskell line comment terminates at the physical line
          -- break, so any HsLineCmt state at end-of-line resets to
          -- HsCode for the following line.
          stNext = case stEnd of HsLineCmt -> HsCode; s -> s
          ln'    = demoteTokens trace ln
      in ln' : go stNext lns

    -- Walk the line's character stream, recording every state
    -- transition as @(positionAfter, newState)@. The list always
    -- starts with @(0, st0)@. Multi-character transitions (e.g.
    -- @--@, @{-@, @\"\"\"@) update the position by the full pattern
    -- length so the resulting trace is monotonic.
    walkTraced :: HsLex -> [Char] -> [(Int, HsLex)]
    walkTraced st0 cs = (0, st0) : aux st0 0 cs
      where
        aux _ _ []                                = []
        aux HsCode pos ('{':'-':rest)             =
          (pos+2, HsBlockCmt 1) : aux (HsBlockCmt 1) (pos+2) rest
        aux HsCode pos ('-':'-':rest)             =
          (pos+2, HsLineCmt) : aux HsLineCmt (pos+2) rest
        -- Haskell character literals. Match conservatively — the
        -- two-char-body and one-char-body forms — and let longer
        -- escapes ('\NUL', '\xFF', '\123', etc.) fall through to be
        -- consumed character-by-character. That fallback is harmless
        -- in HsCode because the escape's interior characters never
        -- introduce a comment / string state on their own. The point
        -- of these patterns is to keep the closing @"@ in a literal
        -- like @'"'@ from accidentally entering HsString — see line
        -- @_quotedbl = '"'@ in filepath's System.FilePath.Internal.
        --
        -- Order matters: the four-char form must precede the
        -- three-char one so that @'\''@ is matched as a complete
        -- escape rather than as @''@ followed by @'@.
        aux HsCode pos ('\'':'\\':_:'\'':rest)    =
          aux HsCode (pos+4) rest
        aux HsCode pos ('\'':_:'\'':rest)         =
          aux HsCode (pos+3) rest
        -- The triple-quote pattern must precede the single-quote
        -- one: Haskell pattern matching is order-sensitive and a
        -- leading @\"@ would otherwise win and start a regular
        -- string.
        aux HsCode pos ('"':'"':'"':rest)         =
          (pos+3, HsMultiString) : aux HsMultiString (pos+3) rest
        aux HsCode pos ('"':rest)                 =
          (pos+1, HsString) : aux HsString (pos+1) rest
        aux HsCode pos (_:rest)                   = aux HsCode (pos+1) rest
        aux (HsBlockCmt n) pos ('{':'-':rest)     =
          (pos+2, HsBlockCmt (n+1)) : aux (HsBlockCmt (n+1)) (pos+2) rest
        aux (HsBlockCmt n) pos ('-':'}':rest)     =
          let new = if n <= 1 then HsCode else HsBlockCmt (n-1)
          in (pos+2, new) : aux new (pos+2) rest
        aux (HsBlockCmt n) pos (_:rest)           =
          aux (HsBlockCmt n) (pos+1) rest
        aux HsLineCmt pos (_:rest)                = aux HsLineCmt (pos+1) rest
        -- Inside a string literal, a backslash escapes the next
        -- character so an escaped @\\\"@ doesn't prematurely close it.
        aux HsString pos ('\\':_:rest)            = aux HsString (pos+2) rest
        aux HsString pos ('"':rest)               =
          (pos+1, HsCode) : aux HsCode (pos+1) rest
        aux HsString pos (_:rest)                 = aux HsString (pos+1) rest
        -- Multi-line strings (GHC's MultilineStrings extension): same
        -- escape handling as a regular string, but the close delimiter
        -- is @\"\"\"@.
        aux HsMultiString pos ('\\':_:rest)       =
          aux HsMultiString (pos+2) rest
        aux HsMultiString pos ('"':'"':'"':rest)  =
          (pos+3, HsCode) : aux HsCode (pos+3) rest
        aux HsMultiString pos (_:rest)            = aux HsMultiString (pos+1) rest

    -- Walk the line's tokens with a running character offset,
    -- consulting 'trace' for the lexical state at each token's
    -- starting position. Tokens whose starting position is inside a
    -- comment or string have their 'Important' constructor rewritten
    -- to 'Other'; everything else is left untouched.
    --
    -- We treat ByteString length as character count, which is exact
    -- for ASCII and acceptable for the UTF-8 source the rest of HPP
    -- assumes — the position only matters insofar as it indexes into
    -- the same byte-list 'walkTraced' just consumed.
    demoteTokens :: [(Int, HsLex)] -> [TOKEN] -> [TOKEN]
    demoteTokens trace0 = goTok 0 trace0
      where
        goTok _   _      []     = []
        goTok pos trace1 (t:ts) =
          let len             = B.length (detok t)
              (st, trace2)    = stateAt pos trace1
              t' = case t of
                     Important s | isOpaque st -> Other s
                     _                          -> t
          in t' : goTok (pos + len) trace2 ts

    -- Advance through the trace until we find the latest entry whose
    -- position is <= p. Returns that entry's state and the suffix of
    -- the trace starting at that entry, so the caller can keep
    -- scanning forward without restarting from the beginning.
    stateAt :: Int -> [(Int, HsLex)] -> (HsLex, [(Int, HsLex)])
    stateAt _ []                                   = (HsCode, [])
    stateAt _ tr@[(_, st)]                         = (st, tr)
    stateAt p tr@((_, st) : rest@((nextPos, _):_))
      | nextPos <= p = stateAt p rest
      | otherwise    = (st, tr)

    isOpaque HsLineCmt      = True
    isOpaque (HsBlockCmt _) = True
    isOpaque HsString       = True
    isOpaque HsMultiString  = True
    isOpaque HsCode         = False

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
