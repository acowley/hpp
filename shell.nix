{ compiler ? "ghc865"
, withHoogle ? true
, sources ? import ./nix/sources.nix
}:
let
  pkgs = import sources.nixpkgs {};
  ghcide = (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/archive/caab5c37a80c4b706ebdc5b50ad610cc96d6b9e2.tar.gz") {}).ghcide-ghc865;
  hspkgs = pkgs.haskell.packages.${compiler};
  drv = hspkgs.callPackage ./default.nix {};
  ghc = hspkgs.ghc.withHoogle (ps: drv.passthru.getBuildInputs.haskellBuildInputs ++ [ps.cabal-install]);
in pkgs.mkShell {
  buildInputs = [ ghc ghcide pkgs.cabal2nix ];
  shellHook = ''
    source <(grep '^export NIX_' ${ghc}/bin/ghc)
    source <(echo 'export HIE_HOOGLE_DATABASE='$(grep -F -- '--database' ${ghc}/bin/hoogle | sed 's/.* --database \(.*\.hoo\).*/\1/'))
  '';
}
