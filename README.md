# The `hpp` Executable
`hpp` is a Haskell pre-processor that is also a C90-compatible
pre-processor (with the addition of a `--cpp` flag). It is
packaged as both a library and an executable.

To use as a Haskell preprocessor for resolving `#ifdef` conditionals
and macro expansion, an invocation might look like,

```
hpp -DDEBUG Foo.hs
```

To use as a C preprocessor, an invocation might look like,

```
hpp -DDEBUG --cpp foo.c
```

To have GHC use `hpp` as the C pre-processor, add this line to the top
of a Haskell source file that makes use of the `CPP` `LANGUAGE`
pragma,

```
{-# OPTIONS_GHC -cpp -pgmPhpp #-}
```

Or add this line to your `.cabal` file:

```
ghc-options: -pgmPhpp
```

Note that you will need to ensure that the `hpp` executable is available in your build environment (e.g. you can add `hpp` as a `build-depends` in your `.cabal` file).

# The `hpp` Library

The `hpp` executable is a command-line interface to the `hpp` library. While the `hpp` package has been designed to have minimal dependencies beyond what the `GHC` compiler itself uses, it does include a few small, framework-free unit tests that demonstrate basic usage as a library. In the `testIf` example, we preprocess the `sourceIfDef` input with a starting definition equivalent to `#define FOO 1`. In `testArith1`, we exercise basic integer arithmetic and comparison. The `hppHelper` function shows how to run your source input through the preprocessor: `runHpp initialState (preproces mySource)`.

```haskell
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
import Control.Monad.Trans.Except
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Hpp
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
hppHelper st src expected = runExceptT (runHpp st (preprocess src)) >>= \case
  Left e -> putStrLn ("Error running hpp: " ++ show e) >> return False
  Right (res, _) -> if hppOutput res == expected
                       then return True
                       else do putStr ("Expected "++show expected++", got")
                               print (hppOutput res)
                               return False

testElse :: IO Bool
testElse = hppHelper emptyHppState sourceIfdef ["x = 99\n","\n"]

testIf :: IO Bool
testIf = hppHelper (fromMaybe (error "Preprocessor definition did not parse")
                              (addDefinition "FOO" "1" emptyHppState))
                   sourceIfdef
                   ["x = 42\n","\n"]

testArith1 :: IO Bool
testArith1 = (&&) <$> hppHelper emptyHppState (sourceArith1 "7") ["yay\n","\n"]
                  <*> hppHelper emptyHppState (sourceArith1 "8") ["boo\n","\n"]

main :: IO ()
main = do results <- sequenceA [testElse, testIf, testArith1]
          if and results then exitWith ExitSuccess else exitWith (ExitFailure 1)
```
