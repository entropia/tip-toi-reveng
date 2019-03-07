{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:

 let
   src = pkgs.fetchgit {
     url = "https://github.com/nomeata/HPDF";
     rev = "1062cf0378c6bff384380101821e6744d41d0484";
     sha256 = "0jjam8s0sl9abgd1a9l6d0ilmzp0l34kvw24548m10r3fg7jc6kd";
   };
 in

  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "HPDF"; version = "1.4.10"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2007-2016, alpheccar.org";
      maintainer = "misc@NOSPAMalpheccar.org";
      author = "";
      homepage = "http://www.alpheccar.org";
      url = "";
      synopsis = "Generation of PDF documents";
      description = "A PDF library with support for several pages, page transitions, outlines, annotations, compression, colors, shapes, patterns, jpegs, fonts, typesetting ... Have a look at the \"Graphics.PDF.Documentation\" module to see how to use it. Or, download the package and look at the test.hs file in the Test folder. That file is giving an example of each feature.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.containers)
          (hsPkgs.random)
          (hsPkgs.bytestring)
          (hsPkgs.array)
          (hsPkgs.zlib)
          (hsPkgs.binary)
          (hsPkgs.mtl)
          (hsPkgs.vector)
          (hsPkgs.errors)
          (hsPkgs.base64-bytestring)
          ];
        };
      tests = {
        "HPDF-Tests" = {
          depends = [ (hsPkgs.base) (hsPkgs.HTF) (hsPkgs.HPDF) ];
          };
        };
      };
    } // rec { inherit src; }
