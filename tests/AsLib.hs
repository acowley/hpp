{-# LANGUAGE LambdaCase, OverloadedStrings #-}
import Control.Monad.Trans.Except
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Hpp
import Hpp.Env (insertPair)
import Hpp.Types (over, env)
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
testIf = hppHelper (over env (insertPair defn) emptyHppState)
                   sourceIfdef
                   ["x = 42\n","\n"]
  where defn = fromMaybe (error "Preprocessor definition did not parse")
                         (parseDefinition "FOO 1")

testArith1 :: IO Bool
testArith1 = (&&) <$> hppHelper emptyHppState (sourceArith1 "7") ["yay\n","\n"]
                  <*> hppHelper emptyHppState (sourceArith1 "8") ["boo\n","\n"]

main :: IO ()
main = do results <- sequenceA [testElse, testIf, testArith1]
          if and results then exitWith ExitSuccess else exitWith (ExitFailure 1)
