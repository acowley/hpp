with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "hpp";
  buildInputs = [ ];
  #ghc = ghc;
  ghc = haskell.compiler.ghc822;
}
