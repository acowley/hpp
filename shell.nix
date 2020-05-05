{ compiler ? "ghc883"
, withHoogle ? true
, sources ? import ./nix/sources.nix
}:
let
  pkgs = import sources.nixpkgs {};
  hspkgs = pkgs.haskell.packages.${compiler};
  drv = hspkgs.callPackage ./default.nix {};
  ghc = hspkgs.ghc.withHoogle (ps: drv.passthru.getBuildInputs.haskellBuildInputs ++ [ps.cabal-install]);
in pkgs.mkShell {
  buildInputs = [ ghc ];
  shellHook = ''
    source <(grep '^export NIX_' ${ghc}/bin/ghc)
    source <(echo 'export HIE_HOOGLE_DATABASE='$(grep -F -- '--database' ${ghc}/bin/hoogle | sed 's/.* --database \(.*\.hoo\).*/\1/'))
  '';
}
