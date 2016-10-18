#!/usr/bin/env bash

HOST='http://tcpdiag.dl.sourceforge.net/project/mcpp/mcpp/V.2.7.2/'
FILE='mcpp-2.7.2.tar.gz'
GCC=gcc

# Note: head -n -1 would be faster than "sed '$ d'", but doesn't work
# with BSD or OS X head.

# Get the include paths GCC will use by default.
GCCDIR=($(echo | ${GCC} -E -v - 2>&1 | sed -n '/#include <...> search starts/,/End of search list/ p' | tail -n +2 | sed '$ d' | grep -v '(framework directory)' | sed 's/^[[:space:]]*//'))

if (! [ $? -eq 0 ]) || [ -z "${GCCDIR}" ]; then
  GCCDIR=""
else
  GCCDIR="${GCCDIR[@]/#/-I}"
fi

if ! [ -d "mcpp-2.7.2" ]; then
  echo 'Downloading MCPP source'
  curl "${HOST}${FILE}" > "${FILE}"
  tar xf ${FILE}
fi

if ! [ -f tool/cpp_test ]; then
  echo 'Building the test runner'
  (cd mcpp-2.7.2/tool && gcc cpp_test.c -o cpp_test)
  if ! [ $? -eq 0 ]; then
    echo 'Building cpp_test failed'
    exit 1
  fi
fi

echo 'Building hpp'
if [ -z "$TRAVIS" ]; then
  (cd .. && stack build hpp:hpp)
else
  (cd .. && cabal build)
fi
if ! [ $? -eq 0 ]; then
  echo 'Building hpp failed'
  exit 1
fi

echo 'Running the validation suite'

# I explicitly add /usr/include to the start of the include file
# search path to avoid trouble on OS X. Specifically, the inclusion of
# <limits.h> causes trouble if the Clang version is picked up before
# the POSIX version.

# Note that we define __i386__ as the architecture as the MCPP
# validation test n_12.c assumes a long is 32 bits.

if [ -z "$TRAVIS" ]; then
  (cd mcpp-2.7.2/test-c &&  ../tool/cpp_test HPP "$(cd ../../.. && stack exec which -- hpp) -I/usr/include ${GCCDIR} --cpp -D__i386__ -D__DARWIN_ONLY_UNIX_CONFORMANCE %s.c | gcc -o %s -w -x c -" "rm %s" < n_i_.lst)
else
  (cd mcpp-2.7.2/test-c &&  ../tool/cpp_test HPP "$(find ../../../dist -type f -executable -name hpp) -I/usr/include ${GCCDIR} --cpp -D__i386__ -D__DARWIN_ONLY_UNIX_CONFORMANCE %s.c | gcc -o %s -w -x c -" "rm %s" < n_i_.lst)
fi

if ! [ $? -eq 0 ]; then
  echo 'The test runner exited with an error.'
  exit 1
fi


ALLTESTS=($(cat mcpp-2.7.2/test-c/n_i_.lst))

if [ -f "mcpp-2.7.2/test-c/HPP.sum" ]; then
  GLOBIGNORE="*"
  RESULTS=($(tail -n +10 mcpp-2.7.2/test-c/HPP.sum | head -n 35))
  ALLPASSED=1
  for i in "${!ALLTESTS[@]}"; do
      case ${RESULTS[$i]} in
      "o")
          echo "Test ${ALLTESTS[$i]} compiled, but execution failed"
          ALLPASSED=0
          ;;
      "-")
          echo "Test ${ALLTESTS[$i]} could not be compiled"
          ALLPASSED=0
          ;;
      esac
  done
  if [ ${ALLPASSED} -eq 1 ]; then
      echo "All ${#ALLTESTS[@]} test files passed!"
      exit 0
  else
      cat "mcpp-2.7.2/test-c/HPP.err"
      exit 1
  fi
else
  echo 'Test suite did not produce any output'
  exit 1
fi
