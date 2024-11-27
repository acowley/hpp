{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Control.Monad.Trans.Except
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Hpp
import qualified Hpp.Config as C
import qualified Hpp.Types as T

#if __GLASGOW_HASKELL__ <= 802
import Data.Monoid ((<>))
#endif

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
