{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:

  let sourceByRegex = import ./source-by-regex.nix pkgs; in

  let src = sourceByRegex ../. [
      "src/"
      "src/.*/"
      "src/.*.hs"
      ".*.cabal"
      "LICENSE"
      ]; in

  {
    flags = { old-locale = false; bytestring_has_builder = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "tttool"; version = "1.8"; };
      license = "MIT";
      copyright = "2013-2017 Joachim Breitner";
      maintainer = "mail@joachim-breitner.de";
      author = "Joachim Breitner";
      homepage = "https://github.com/entropia/tip-toi-reveng";
      url = "";
      synopsis = "Working with files for the Tiptoi® pen";
      description = "The Ravensburger Tiptoi® pen is programmed via special\nfiles. Their file format has been reverse engineered; this\nis a tool to analyse and create such files.";
      buildType = "Simple";
      };
    components = {
      exes = {
        "tttool" = {
          configureFlags = pkgs.lib.optionals (pkgs.hostPlatform.isMusl) [
             "--ghc-option=-static"
             "--ghc-option=-optl=-static"
             "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
             "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          ];
          depends = (([
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.executable-path)
            (hsPkgs.filepath)
            (hsPkgs.template-haskell)
            (hsPkgs.JuicyPixels)
            (hsPkgs.aeson)
            (hsPkgs.hashable)
            (hsPkgs.haskeline)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.random)
            (hsPkgs.vector)
            (hsPkgs.yaml)
            (hsPkgs.HPDF)
            (hsPkgs.split)
            (hsPkgs.optparse-applicative)
            (hsPkgs.spool)
            (hsPkgs.zlib)
            (hsPkgs.natural-sort)
            (hsPkgs.blaze-svg)
            (hsPkgs.base64-bytestring)
            (hsPkgs.text)
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.5") (hsPkgs.ghc-prim)) ++ (if flags.old-locale
            then [ (hsPkgs.time) (hsPkgs.old-locale) ]
            else [ (hsPkgs.time) ])) ++ (if flags.bytestring_has_builder
            then [ (hsPkgs.bytestring) ]
            else [ (hsPkgs.bytestring) (hsPkgs.bytestring-builder) ]);
          };
        };
      };
    } // rec { inherit src; }
