{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Control.Monad.Trans.Except
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Hpp
import qualified Hpp.Config as C
import qualified Hpp.Types as T

#if __GLASGOW_HASKELL__ <= 802
import Data.Monoid ((<>))
#endif

import System.Directory (withCurrentDirectory)
import System.Exit

sourceIfdef :: [ByteString]
sourceIfdef = [ "#ifdef FOO"
              , "x = 42"
              , "#else"
              , "x = 99"
              , "#endif" ]

sourceArith1 :: ByteString -> [ByteString]
sourceArith1 s = [ "#define x 3"
                 , "#if 5 + x > " <> s
                 , "yay"
                 , "#else"
                 , "boo"
                 , "#endif" ]

hppHelper :: HppState -> [ByteString] -> [ByteString] -> IO Bool
hppHelper st src expected =
  case runExcept (expand st (preprocess src)) of
    Left e -> putStrLn ("Error running hpp: " ++ show e) >> return False
    Right (res, _) -> if hppOutput res == expected
                      then return True
                      else do putStrLn ("Expected: "++show expected)
                              putStrLn ("Got:      "++show (hppOutput res))
                              return False

-- | File-based variant of 'hppHelper': runs 'runHpp' (which performs IO
-- to read included files) and checks that the predicate holds on the
-- emitted output. Used to exercise paths that 'expand' skips, e.g.
-- @#include "..."@ resolution.
hppFileHelper :: HppState -> [ByteString] -> ([ByteString] -> Bool) -> IO Bool
hppFileHelper st src ok = do
  r <- runExceptT (runHpp st (preprocess src))
  case r of
    Left e -> putStrLn ("Error running hpp: " ++ show e) >> return False
    Right (res, _) ->
      if ok (hppOutput res)
        then return True
        else do putStrLn ("Got: " ++ show (hppOutput res))
                return False

sourceCommentsAndSplice :: [ByteString]
sourceCommentsAndSplice =
  [ "#ifdef FOO"
  , "Some /* neat */ text"
  , "#else"
  , "I am /* an else branch"
  , "whose importance must not"
  , "be */ underestimated "
  , "#endif"
  , "Do you\\"
  , "understand?"]

-- | A configuration to not splice lines, leave C-style comments,
-- ignore trigraphs, but emit line markers.
defaultCfg :: HppState
defaultCfg = T.over T.config opts emptyHppState
  where opts = T.setL C.spliceLongLinesL False
             . T.setL C.eraseCCommentsL False
             . T.setL C.replaceTrigraphsL False
             . T.setL C.inhibitLinemarkersL False

remove_comments :: HppState -> HppState
remove_comments = T.over T.config (T.setL C.eraseCCommentsL True)

remove_line :: HppState -> HppState
remove_line = T.over T.config (T.setL C.inhibitLinemarkersL True)

-- | Override the include search path on an 'HppState'.
with_include_paths :: [FilePath] -> HppState -> HppState
with_include_paths ps =
  T.over T.config (\c -> c { C.includePathsF = pure ps })

-- | Pass unknown @#@-directives through as plain text instead of
-- raising an error.
ignoreUnknown :: HppState -> HppState
ignoreUnknown = T.over T.config (T.setL C.ignoreUnknownDirectivesL True)

-- | Treat Haskell comments as opaque when looking for directives.
haskellComments :: HppState -> HppState
haskellComments = T.over T.config (T.setL C.ignoreHaskellCommentsL True)

add_definition :: ByteString -> ByteString -> HppState -> HppState
add_definition k v s = fromMaybe (error "Preprocessor definition did not parse")
                                 (addDefinition k v s)

tests :: [IO Bool]
tests =
  [ hppHelper (remove_line emptyHppState)
      sourceIfdef ["x = 99\n","\n"]

  , hppHelper (remove_line $ add_definition "FOO" "1" emptyHppState)
      sourceIfdef ["x = 42\n","\n"]

  , (&&) <$> hppHelper (remove_line emptyHppState) (sourceArith1 "7") ["yay\n","\n"]
         <*> hppHelper (remove_line emptyHppState) (sourceArith1 "8") ["boo\n","\n"]

  , hppHelper (add_definition "FOO" "1" defaultCfg)
            sourceCommentsAndSplice
            [ "#line 2\n"
            , "Some /* neat */ text\n"
            , "#line 8\n"
            , "Do you\\\n"
            , "understand?\n" ]

  , hppHelper defaultCfg
            sourceCommentsAndSplice
            [ "#line 4\n"
            , "I am /* an else branch\n"
            , "whose importance must not\n"
            , "be */ underestimated \n"
            , "Do you\\\n"
            , "understand?\n" ]

  , hppHelper defaultCfg
            [ "#define FOO() foo"
            , "bar"
            , "FOO()"
            , "baz"
            ]
            [ "bar\n"
            , "foo\n"
            , "baz\n"
            ]

  , hppHelper (remove_comments defaultCfg)
            [ "#define FOO(a) a+a"
            , "// Blah FOO() blah"
            , "/* Blah FOO() blah */"
            ]
            [ "\n"
            , "\n"
            ]

  , hppHelper (remove_comments defaultCfg)
            [ "foo"
            , "/* blah"
            , "   https://foo.bar */"
            , "bar"
            ]
            [ "foo\n"
            , "bar\n"
            ]

  , hppHelper (remove_comments defaultCfg)
            [ "foo"
            , "\"something\"/* blah"
            , "   */"
            , "bar"
            ]
            [ "foo\n"
            , "\"something\"\n"
            , "bar\n"
            ]

  , hppHelper (remove_comments defaultCfg)
            [ "foo"
            , "/* blah"
            , "  \" */"
            , "bar"
            ]
            [ "foo\n"
            , "bar\n"
            ]

  , hppHelper defaultCfg
            [ "#define MULTI(a,b,...) (A=a,B=b,rest=__VA_ARGS__)"
            , "MULTI(1,2,3,4,5,6)"
            ]
            [ "(A=1,B=2,rest=3,4,5,6)\n"
            ]

  , hppHelper defaultCfg
            [ "#define MULTI(a,b,...) (a,b,##__VA_ARGS__)"
            , "MULTI(1,2)"
            ]
            [ "(1,2)\n"
            ]

  , hppHelper defaultCfg
            [ "cond000:FOO"
            , "#ifdef FOO"
            , "3: __LINE__: aaaa"
            , "#endif"
            , "5: __LINE__:"
            , "#ifdef FOO"
            , "7: __LINE__: aaaa"
            , "#endif"
            , "9: __LINE__:"
            , "#ifdef FOO"
            , "11: __LINE__: aaaa"
            , "#endif"
            ]
            [ "cond000:FOO\n"
            , "#line 5\n"
            , "5: 5:\n"
            , "#line 9\n"
            , "9: 9:\n"
            , "#line 13\n"
            ]

  , hppHelper (add_definition "FOO" "" defaultCfg)
            [ "cond000:FOO"
            , "#ifdef FOO"
            , "3: __LINE__: aaaa"
            , "#endif"
            , "5: __LINE__:"
            , "#ifdef FOO"
            , "7: __LINE__: aaaa"
            , "#endif"
            , "9: __LINE__:"
            , "#ifdef FOO"
            , "11: __LINE__: aaaa"
            , "#endif"
            ]
            [ "cond000:\n"
            , "#line 3\n"
            , "3: 3: aaaa\n"
            , "#line 5\n"
            , "5: 5:\n"
            , "#line 7\n"
            , "7: 7: aaaa\n"
            , "#line 9\n"
            , "9: 9:\n"
            , "#line 11\n"
            , "11: 11: aaaa\n"
            , "#line 13\n"
            ]

  , hppHelper defaultCfg
            [ "cond001:FOO"
            , "#ifdef FOO"
            , "3: __LINE__: aaaa"
            , "#else"
            , "5: __LINE__: bbbb"
            , "#endif"
            , "#ifdef FOO"
            , "8: __LINE__: aaaa"
            , "#else"
            , "10: __LINE__: bbbb"
            , "#endif"
            , "#ifdef FOO"
            , "13: __LINE__: aaaa"
            , "#else"
            , "15: __LINE__: bbbb"
            , "#endif"
            ]
            [ "cond001:FOO\n"
            , "#line 5\n"
            , "5: 5: bbbb\n"
            , "#line 10\n"
            , "10: 10: bbbb\n"
            , "#line 15\n"
            , "15: 15: bbbb\n"
            ]

  , hppHelper (add_definition "FOO" "" defaultCfg)
            [ "cond001:FOO"
            , "#ifdef FOO"
            , "3: __LINE__: aaaa"
            , "#else"
            , "5: __LINE__: bbbb"
            , "#endif"
            , "#ifdef FOO"
            , "8: __LINE__: aaaa"
            , "#else"
            , "10: __LINE__: bbbb"
            , "#endif"
            , "#ifdef FOO"
            , "13: __LINE__: aaaa"
            , "#else"
            , "15: __LINE__: bbbb"
            , "#endif"
            ]
            [ "cond001:\n"
            , "#line 3\n"
            , "3: 3: aaaa\n"
            , "#line 7\n"
            , "#line 8\n"
            , "8: 8: aaaa\n"
            , "#line 12\n"
            , "#line 13\n"
            , "13: 13: aaaa\n"
            , "#line 17\n"
            ]

  -- Empty trailing argument: glibc's mathcalls.h relies on this when
  -- it expands @__MATHDECL_ALIAS@ into @__MATH_PRECNAME(name,)@. The
  -- second argument is intentionally empty, and the macro must still
  -- see /two/ arguments rather than collapsing to one.
  , hppHelper (remove_line emptyHppState)
            [ "#define PAIR(a,b) [a|b]"
            , "PAIR(x,)"
            ]
            [ "[x|]\n"
            , "\n"
            ]

  -- Empty leading argument and empty middle argument also matter:
  -- @M(,b)@ → 2 args (first empty), @M(a,,c)@ → 3 args (middle empty).
  , hppHelper (remove_line emptyHppState)
            [ "#define TRIO(a,b,c) [a|b|c]"
            , "TRIO(,b,)"
            , "TRIO(a,,c)"
            ]
            [ "[|b|]\n"
            , "[a||c]\n"
            , "\n"
            ]

  -- Quoted nested includes resolve relative to the includer's
  -- directory. The fixture in tests/include-data has:
  --
  --   sub/outer.h    -- contains  #include "inner.h"
  --   sub/inner.h    -- contains  inner_marker
  --
  -- The test cd's into tests/include-data and feeds an initial
  -- @#include "sub/outer.h"@. Once outer.h is being preprocessed,
  -- its sibling @#include "inner.h"@ has to resolve through the
  -- current-file directory @sub@: without the per-file directory
  -- tracking it would only be searched relative to the original
  -- input's directory and never find inner.h.
  , withCurrentDirectory "tests/include-data" $
      hppFileHelper
        (remove_line emptyHppState)
        ["#include \"sub/outer.h\""]
        (any (BS.isInfixOf "inner_marker"))

  -- Quoted nested includes work even when the /includer/ was
  -- resolved off the include search path (not the input's cwd).
  -- This is the GHC RTS scenario: a top-level @#include
  -- <stg/MachRegsForHost.h>@ finds the file under an include
  -- path; that file then does @#include "MachRegs.h"@ to refer
  -- to a sibling, and @MachRegs.h@ in turn does @#include
  -- "MachRegs/x86.h"@ to reach a subdirectory.
  --
  -- The fixture in tests/include-data is:
  --
  --   inc/stg/MachRegsForHost.h  -- #include "MachRegs.h"
  --   inc/stg/MachRegs.h         -- #include "MachRegs/x86.h"
  --   inc/stg/MachRegs/x86.h     -- deep_marker
  --
  -- The includer's directory must be the /resolved/ on-disk path
  -- (@inc/stg/@), not the textual @#include@ argument
  -- (@stg/MachRegsForHost.h@) — otherwise the chained relative
  -- includes fall off the end of every search path and fail with
  -- IncludeDoesNotExist on @MachRegs/x86.h@.
  , withCurrentDirectory "tests/include-data" $
      hppFileHelper
        (with_include_paths ["inc"] $ remove_line emptyHppState)
        ["#include <stg/MachRegsForHost.h>"]
        (any (BS.isInfixOf "deep_marker"))

  -- C99 §6.10.2: macros must NOT be expanded inside `<...>` or
  -- `"..."` include arguments. Real-world trigger: glibc's
  -- <errno.h> installs `#define errno (*__errno_location ())`,
  -- so any downstream `#include <errno.h>` would otherwise be
  -- rewritten to `#include <(*__errno_location ()).h>` and fail
  -- with IncludeDoesNotExist. The `#define` form (with a space
  -- before the body) produces an object-like macro — the form
  -- that triggers the bug. add_definition cannot reproduce it
  -- because the helper concatenates tokens with no separator,
  -- which parseDefinition reads as function-like.
  , withCurrentDirectory "tests/include-data" $
      hppFileHelper
        (with_include_paths ["macro-in-include"] (remove_line emptyHppState))
        [ "#define errno (*__errno_location ())"
        , "#include <errno.h>"
        ]
        (any (BS.isInfixOf "errno_marker"))

  -- A @#line N@ directive must make the immediately following
  -- input line expand __LINE__ to N (not N+1). The line-counter
  -- semantics here are load-bearing for any caller that resets
  -- lineNum mid-stream — see Note [Resetting __LINE__ after the
  -- CLI prelude] in pkg:Hpp.CmdLine.
  , hppHelper (remove_line emptyHppState)
            [ "ignored line"
            , "#line 1 \"main.c\""
            , "__LINE__"        -- → must report 1
            , "__LINE__"        -- → must report 2
            ]
            [ "ignored line\n"
            , "1\n"
            , "2\n"
            , "\n"
            ]

  -- Stringifying an empty (or empty-bodied) argument used to crash
  -- with @Data.ByteString.index: negative index: -1@: 'trimSpaces'
  -- read past the start of an empty bytestring while looking for
  -- trailing whitespace. The exposure path that surfaced this in
  -- glibc was @__ASMNAME(__USER_LABEL_PREFIX__, name)@, where the
  -- predefined @__USER_LABEL_PREFIX__@ is empty and the helper
  -- macro stringifies it: @#x@ on an empty arg should yield @""@.
  , hppHelper (remove_line emptyHppState)
            [ "#define EMPTY "
            , "#define STR(x) #x"
            , "#define M(a, b) STR(a) b"
            , "M(EMPTY, foo)"
            ]
            [ "\"\" foo\n"
            , "\n"
            ]

  -- ignoreUnknownDirectives: unknown '#'-commands pass through unchanged
  -- (mirrors -traditional-cpp's permissive handling of Haskell pragmas
  -- whose closing @#-}@ lands on its own line).
  , hppHelper (ignoreUnknown $ remove_line emptyHppState)
            [ "before"
            , "  #-}"
            , "after"
            ]
            [ "before\n"
            , "  #-}\n"
            , "after\n"
            , "\n"
            ]

  -- Known directives are still processed when the option is on.
  , hppHelper (ignoreUnknown $ remove_line $ add_definition "FOO" "1" emptyHppState)
            sourceIfdef ["x = 42\n","\n"]

  -- ignoreHaskellComments: the closing brace of a multi-line Haskell
  -- pragma ('  #-}') begins inside an open '{-' block comment. The
  -- gating pass demotes the leading '#' token so directive dispatch
  -- skips it, and the bytes are emitted unchanged.
  , hppHelper (haskellComments $ remove_line emptyHppState)
            [ "{-# LANGUAGE CPP"
            , "           , OverloadedStrings"
            , "  #-}"
            , "module M where"
            , "x = 1"
            ]
            [ "{-# LANGUAGE CPP\n"
            , "           , OverloadedStrings\n"
            , "  #-}\n"
            , "module M where\n"
            , "x = 1\n"
            , "\n"
            ]

  -- Nested block comments are tracked: an inner '{-' inside an outer
  -- '{-' bumps the depth so a single '-}' stays inside the outermost
  -- comment and the '#-}' line remains gated.
  , hppHelper (haskellComments $ remove_line emptyHppState)
            [ "{- outer {- inner"
            , "  #-}"
            , "  still in outer -}"
            , "-}"
            , "x = 1"
            ]
            [ "{- outer {- inner\n"
            , "  #-}\n"
            , "  still in outer -}\n"
            , "-}\n"
            , "x = 1\n"
            , "\n"
            ]

  -- A '{-' appearing inside a string literal should not open a
  -- Haskell block comment, so the directive that follows is still
  -- dispatched normally.
  , hppHelper (haskellComments $ add_definition "FOO" "1"
               $ remove_line emptyHppState)
            [ "s = \"{- not a comment -}\""
            , "#ifdef FOO"
            , "y = 42"
            , "#endif"
            ]
            [ "s = \"{- not a comment -}\"\n"
            , "y = 42\n"
            , "\n"
            ]

  -- A multi-line Haskell string literal continued via the
  -- backslash-gap syntax ('\' at end of one line, '\' at the start
  -- of the next) keeps HsString state alive across lines. A '{-'
  -- written inside such a string must not open a Haskell block
  -- comment — otherwise the open comment would persist past the
  -- closing '"' and demote the leading '#' of the following
  -- directive, leaving '#ifdef FOO' to pass through as plain text.
  -- This test uses 'defaultCfg' so that 'spliceLongLines' is off
  -- and the '\' line endings reach 'gateHaskellComments' intact.
  , hppHelper (haskellComments . remove_line . add_definition "FOO" "1"
              $ defaultCfg)
            [ "s = \"open {- gap \\"
            , "    \\ end\""
            , "#ifdef FOO"
            , "y = 42"
            , "#endif"
            ]
            [ "s = \"open {- gap \\\n"
            , "    \\ end\"\n"
            , "y = 42\n"
            ]

  -- Same scenario, but the '{-' appears on the /second/ line of the
  -- multi-line string (after the gap closes). The string was opened
  -- on the first line, so HsString state must be carried across the
  -- gap into the second line — otherwise a fresh HsCode walk would
  -- see '{-' open an unclosed block comment and the trailing
  -- '#ifdef FOO' would be demoted to plain text.
  , hppHelper (haskellComments . remove_line . add_definition "FOO" "1"
              $ defaultCfg)
            [ "s = \"first \\"
            , "    \\ {- still in string \\"
            , "    \\ end\""
            , "#ifdef FOO"
            , "y = 42"
            , "#endif"
            ]
            [ "s = \"first \\\n"
            , "    \\ {- still in string \\\n"
            , "    \\ end\"\n"
            , "y = 42\n"
            ]

  -- GHC's MultilineStrings extension: a literal opens with '"""' and
  -- closes at the next '"""', and may span many lines. The body of a
  -- multi-line string can contain text that looks like a directive
  -- ('#define INSIDE 1' below) — that line must pass through as
  -- string content rather than dispatch as a directive. As a
  -- corollary, INSIDE must NOT be defined: the bare 'INSIDE' that
  -- appears after the closing '"""' is expected to remain
  -- unexpanded. A real '#define FOO 42' after the string then
  -- dispatches normally and 'FOO' expands to 42 — proving the
  -- closing '"""' returned the state machine to HsCode.
  , hppHelper (haskellComments . remove_line $ defaultCfg)
            [ "before"
            , "s = \"\"\""
            , "#define INSIDE 1"
            , "still in string"
            , "\"\"\""
            , "INSIDE"
            , "#define FOO 42"
            , "FOO"
            ]
            [ "before\n"
            , "s = \"\"\"\n"
            , "#define INSIDE 1\n"
            , "still in string\n"
            , "\"\"\"\n"
            , "INSIDE\n"
            , "42\n"
            ]

  -- A '{-' inside a multi-line string must not open a Haskell block
  -- comment, and a '#ifdef'/'#endif' pair inside one must not run
  -- conditional skipping. Without proper '"""' tracking the opening
  -- 'foo = """' would walk as three single-quote toggles and end in
  -- HsString rather than HsMultiString; the body's '#ifdef
  -- NEVER_DEFINED' would then dispatch (HsString does not gate
  -- '#'), the false branch would swallow 'would skip', and the
  -- output would be missing the body lines.
  , hppHelper (haskellComments . remove_line $ defaultCfg)
            [ "foo = \"\"\""
            , "{- not a comment"
            , "#ifdef NEVER_DEFINED"
            , "would skip"
            , "#endif"
            , "-}"
            , "\"\"\""
            , "after"
            ]
            [ "foo = \"\"\"\n"
            , "{- not a comment\n"
            , "#ifdef NEVER_DEFINED\n"
            , "would skip\n"
            , "#endif\n"
            , "-}\n"
            , "\"\"\"\n"
            , "after\n"
            ]

  -- Outside a Haskell block comment, a regular '#'-prefixed line is
  -- still dispatched as a directive even with ignoreHaskellComments
  -- on. (Sanity check: the gate only fires on lines that begin inside
  -- an open block comment.)
  , hppHelper (haskellComments $ remove_line
               $ add_definition "FOO" "1" emptyHppState)
            sourceIfdef ["x = 42\n","\n"]

  ]

main :: IO ()
main = do results <- sequenceA tests
          if and results
            then do putStrLn (show (length results) ++ " tests passed")
                    exitWith ExitSuccess
            else do putStrLn (show (length (filter id results)) ++
                              " of " ++ show (length results) ++
                              " tests passed")
                    exitWith (ExitFailure 1)
