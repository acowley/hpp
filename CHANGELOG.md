# 0.6.0

- Various bug fixes by @rahulmutt. These may change behavior not captured by the MCPP test suite.
- Switch to `unordered-containers` from `bytestring-trie` for stackage compatibility
- Internal refactoring

# 0.5.1
Added the `expand` API for pure macro processing (i.e. `#include`s are ignored).

# 0.5.0
- Redesigned library API
The `Hpp` module exports the main pieces. `Hpp.Env`, `Hpp.Types`, and `Hpp.Config` may be used for configuring the preprocessor.

# 0.4.0

- Simplify the parsing machinery
- Don't remove C++-style single-line comments
- Don't error on unknown cpp directives
  Previously, a line beginning with "#-}" would cause an error
- Don't do trigraph replacement by default.
  Haskell allows "??" in operator names and you can be sure `lens` uses it!

# 0.3.1

Address a change wherein GHC 8 will pass `-include` arguments without a space between "-include" and the file to be included.

# 0.3

Switch to a stream processing model.

This library is designed to have minimal dependencies, so we now have
a bespoke implementation of a cross between the pipes and machines
libraries included.

This change was done to make some parsing
operations easier, believe it or not. For example, most pre-processing
is done on a line-by-line basis, but we must also support macro
function applications that cross line boundaries. Thus the expansion
logic can not merely be given one line at a time from an input
file. Previously, a heuristic tried to combine consecutive lines
before the parsing stage. Now, the parser itself is able to pull
tokens in across lines when necessary.

TL;DR: The upshot is that processing `/usr/include/stdio.h` on OS X (a
surprisingly complicated file!)  now uses 78% of the time and 0.38%
the memory of previous versions of `hpp`.

# 0.1

First release!
