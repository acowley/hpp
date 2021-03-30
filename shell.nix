{ compiler ? null
, withHoogle ? true
, sources ? import ./nix/sources.nix
}:
let
  pkgs = import sources.nixpkgs-chan {};
  hspkgs = if isNull compiler then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};
  drv = hspkgs.callPackage ./default.nix {};
  ghc = hspkgs.ghc.withHoogle (ps: drv.passthru.getBuildInputs.haskellBuildInputs);
in pkgs.mkShell {
  buildInputs = [ ghc hspkgs.cabal-install hspkgs.haskell-language-server];
  # shellHook = ''
  #   source <(grep '^export NIX_' ${ghc}/bin/ghc)
  #   source <(echo 'export HIE_HOOGLE_DATABASE='$(grep -F -- '--database' ${ghc}/bin/hoogle | sed 's/.* --database \(.*\.hoo\).*/\1/'))
  # '';
}
