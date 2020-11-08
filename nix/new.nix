{ # Fetch the latest haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}
}:
let
  pkgs = haskellNix.pkgs;

  sourceByRegex = import ./source-by-regex.nix pkgs;

  src = sourceByRegex ../. [
    "cabal.project"
    "src/"
    "src/.*/"
    "src/.*.hs"
    ".*.cabal"
    "LICENSE"
    ];

  patchedSrc = pkgs.runCommandNoCC "tttool-src" {} ''
    cp -r ${src} $out
    chmod -R u+w $out
    sed -i -e 's/with-compiler/-- with-compiler/' $out/cabal.project
  '';

  tttool = pkgs:
    (pkgs.haskell-nix.cabalProject {
      src = patchedSrc;
      compiler-nix-name = "ghc8102";
    }).tttool.components.exes.tttool;
in
{ linux = tttool pkgs;
  static = tttool pkgs.pkgsCross.musl64;
  windows = tttool pkgs.pkgsCross.mingwW64;
}
