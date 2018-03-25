{-# LANGUAGE LambdaCase #-}
import Hpp.CmdLine
import System.Environment

usage :: IO ()
usage = mapM_ putStrLn
  [ "Usage: hpp [options] inputFile [outputFile]"
  , ""
  , "Options:"
  , "-D name"
  , "  Define name as an object macro defined as 1."
  , "-D name=definition"
  , "  Define name as an object macro defined as definition."
  , "-U name"
  , "  Remove any previous definition of name."
  , "-I dir"
  , "  Add directory dir to the search path for includes."
  , "-o file"
  , "  Write output to file."
  , "-include file"
  , "  Acts as if #include \"file\" were the first line "
  , "  in the primary source file. -include options are "
  , "  processed after -D and -U options."
  , "-imacros file"
  , "  Like -include, except that output is discarded. Only"
  , "  macro definitions are kept."
  , "--cpp"
  , "  C98 compatibility."
  , "  Implies: --fline-splice --ferase-comments --freplace-trigraphs"
  , "  -D __STDC__  -D __STDC_VERSION__=199409L -D _POSIX_C_SOURCE=200112L"
  , "-P"
  , "  Inhibit #line markers (when this option is given after --cpp)"
  , "--fline-splice"
  , "  Enable continued line splicing."
  , "--ferase-comments"
  , "  Remove all C-style comments before processing."
  , "--freplace-trigraphs"
  , "  Replace trigraph sequences before processing." ]

main :: IO ()
main = do getArgs >>= \case
            [] -> usage
            args -> runWithArgs args >> return ()


{-

For testing against C:

hpp -I/usr/local/include -I/usr/include -fline-splice -ferase-comments -D __x86_64__ -D __GNUC__ -D _POSIX_C_SOURCE n_1.c

For the mcpp validation suite

../tool/cpp_test HPP "../../dist/build/hpp/hpp -I/usr/local/include -I/usr/include -fline-splice -ferase-comments -D __x86_64__ -D __GNUC__=4 -D __STDC__ -D __STDC_VERSION__=199409L -D _POSIX_C_SOURCE -D __DARWIN_ONLY_UNIX_CONFORMANCE %s.c | gcc -o %s -x c -" "rm %s" < n_i_.lst

-}
