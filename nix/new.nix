let
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/c00cc0cfedf03ee3f21de420882189adbb0801d8.tar.gz") {};

  # windows crossbuilding with ghc-8.10 needs at least 20.09.
  # A peek at https://github.com/input-output-hk/haskell.nix/blob/master/ci.nix can help
  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  pkgs = import nixpkgsSrc nixpkgsArgs;

  sourceByRegex = import ./source-by-regex.nix pkgs;

  src = sourceByRegex ../. [
    "cabal.project"
    "src/"
    "src/.*/"
    "src/.*.hs"
    ".*.cabal"
    "LICENSE"
    ];

  # Remove the with-comiler flag from cabal.project
  # We do that to help users that want to build with plain cabal
  # but it confuses `cabal new-configure` when run by nix
  patchedSrc = pkgs.applyPatches {
    name = "tttool-src";
    inherit src;
    postPatch = ''
      sed -i -e 's/with-compiler/-- with-compiler/' cabal.project
    '';
  };

  tttool = pkgs:
    (pkgs.haskell-nix.cabalProject {
      src = patchedSrc;
      compiler-nix-name = "ghc8102";
      index-state = "2020-11-08T00:00:00Z";
      modules =
      if pkgs.hostPlatform.isMusl
      then
        [{
          # terminfo is disabled on musl by haskell.nix, but still the flag
          # is set in the package plan, so override this
          packages.haskeline.flags.terminfo = false;
          packages.tttool.configureFlags = [
             #"--disable-executable-dynamic"
             #"--disable-shared"
             #"--ghc-option=-v"
             "--ghc-option=-static"
             #"--ghc-option=-optl=-static"
             #"--ghc-option=-optl=-pthread"
             #"--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
             #"--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
           ];
        }]
      else [];
    }).tttool.components.exes.tttool;
in
{ linux = tttool pkgs;
  static = tttool pkgs.pkgsCross.musl64;
  windows = tttool pkgs.pkgsCross.mingwW64;
}
