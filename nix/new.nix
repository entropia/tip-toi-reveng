{ # Fetch the latest haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs
}:
let
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
