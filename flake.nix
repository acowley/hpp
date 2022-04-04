{
  description = "A Haskell pre-processor.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let

      compiler = "8107";
      # compiler = "921";
      pkgs = import nixpkgs {
        inherit system;
      };
      dontCheck = pkgs.haskell.lib.dontCheck;
      dontHaddock = pkgs.haskell.lib.dontHaddock;
      hspkgs = (pkgs.haskell.packages."ghc${compiler}").override {
        overrides = self: super: 
          if compiler == "921"
          then { } 
          else { };
      };
      drv = hspkgs.callPackage ./default.nix {};
      ghc = hspkgs.ghc.withPackages (ps: 
        drv.passthru.getBuildInputs.haskellBuildInputs
      );
  in {
    devShell = pkgs.mkShell {
      buildInputs = [
        ghc
        hspkgs.cabal-install
      ];
    };
  });
}
