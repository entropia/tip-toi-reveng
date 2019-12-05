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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "tttool"; version = "1.9"; };
      license = "MIT";
      copyright = "2013-2019 Joachim Breitner";
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
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.base64-bytestring)
            (hsPkgs.binary)
            (hsPkgs.blaze-svg)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.executable-path)
            (hsPkgs.filepath)
            (hsPkgs.hashable)
            (hsPkgs.haskeline)
            (hsPkgs.HPDF)
            (hsPkgs.JuicyPixels)
            (hsPkgs.mtl)
            (hsPkgs.natural-sort)
            (hsPkgs.optparse-applicative)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.random)
            (hsPkgs.split)
            (hsPkgs.spool)
            (hsPkgs.template-haskell)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.vector)
            (hsPkgs.yaml)
            (hsPkgs.zlib)
            ];
          };
        };
      };
    } // rec { inherit src; }
