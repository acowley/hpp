#!/usr/bin/env bash

HOST='http://prdownloads.sourceforge.net/mcpp/mcpp-2.7.2.tar.gz?download'
FILE='mcpp-2.7.2.tar.gz'
GCC=gcc
CABAL=${CABALBUILD:-cabal v2-build}

# Note: head -n -1 would be faster than "sed '$ d'", but doesn't work
# with BSD or OS X head.

# Get the include paths GCC will use by default.
GCCDIR=($(${GCC} -xc -E -v /dev/null 2>&1 | sed -n '/#include <...> search starts/,/End of search list/p' | tail -n +2 | sed '$ d' | grep -v '(framework directory)' | sed 's/^[[:space:]]*//' | tac))

if (! [ $? -eq 0 ]) || [ -z "${GCCDIR}" ]; then
  GCCDIR=""
else
  GCCDIR="${GCCDIR[@]/#/-I}"
fi

if ! [ -d "mcpp-2.7.2" ]; then
  echo 'Downloading MCPP source'
  curl -L "${HOST}" > "${FILE}"
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
(cd .. && ${CABAL})
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

# Forward GCC's predefined macros to hpp via -include. Glibc's
# headers inspect a number of these and behave differently when they
# are missing:
#   * __GNUC__/__GNUC_MINOR__/__GNUC_PATCHLEVEL__ — without these,
#     bits/floatn-common.h thinks GCC < 7 and emits `typedef float
#     _Float32;` which fails to compile on GCC >= 7 where _Float32
#     is a reserved built-in keyword.
#   * __LONG_MAX__ / __INT_MAX__ / __LONG_LONG_MAX__ — limits.h
#     expands LONG_MAX et al. to these. Without them, n_12.c's
#     `#if LONG_MAX <= 1073741823` evaluates the LHS as 0 and picks
#     the wrong branch.
#
# We pass -std=c99 to dump the standards-conforming subset rather
# than the GNU dialect: the GNU dialect predefines bare 'linux',
# 'unix' and 'i386' as 1, which collides with paths like
# @#include <linux/limits.h>@ that some glibc bits/ headers use.
GCC_PREDEFS="$(pwd)/gcc-predefs.h"
${GCC} -std=c99 -dM -E -x c /dev/null > "${GCC_PREDEFS}"

# GCC 14 promoted -Wimplicit-int and -Wimplicit-function-declaration from
# warnings to errors by default. The MCPP test programs use K&R-style
# `main()` and call exit() without including <stdlib.h>, so downgrade
# both back to warnings (which -w then silences).
GCCFLAGS="-w -Wno-error=implicit-int -Wno-error=implicit-function-declaration"

if [ -z "$TRAVIS" ]; then
    (cd mcpp-2.7.2/test-c &&  ../tool/cpp_test HPP "$(find $(pwd)/../../../dist-newstyle -type f -executable -name hpp) ${GCCDIR} --cpp -D__x86_64__ -include ${GCC_PREDEFS} %s.c | gcc -o %s ${GCCFLAGS} -x c -" "rm %s" < n_i_.lst)
else
    (cd mcpp-2.7.2/test-c &&  ../tool/cpp_test HPP "$(find $(pwd)/../../../../dist-newstyle -type f -executable -name hpp) ${GCCDIR} --cpp -D__x86_64__ -include ${GCC_PREDEFS} %s.c | gcc -o %s ${GCCFLAGS} -x c -" "rm %s" < n_i_.lst)
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
