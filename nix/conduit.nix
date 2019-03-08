{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
 let
   src = pkgs.fetchgit {
     url = "https://github.com/nomeata/conduit";
     rev = "60b3c9a08c732d38e92835f146f3fefb325317a0";
     sha256 = "089v82iighq1h9gqpprmsib88m2lkh04zabah2rxwr1p4b5vgsp1";
   };
 in
  {
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "conduit"; version = "1.3.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "http://github.com/snoyberg/conduit";
      url = "";
      synopsis = "Streaming data processing library.";
      description = "`conduit` is a solution to the streaming data problem, allowing for production,\ntransformation, and consumption of streams of data in constant memory. It is an\nalternative to lazy I\\/O which guarantees deterministic resource handling.\n\nFor more information about conduit in general, and how this package in\nparticular fits into the ecosystem, see [the conduit\nhomepage](https://github.com/snoyberg/conduit#readme).\n\nHackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/conduit>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.resourcet)
          (hsPkgs.transformers)
          (hsPkgs.mtl)
          (hsPkgs.primitive)
          (hsPkgs.unliftio-core)
          (hsPkgs.exceptions)
          (hsPkgs.mono-traversable)
          (hsPkgs.vector)
          (hsPkgs.bytestring)
          (hsPkgs.text)
          (hsPkgs.filepath)
          (hsPkgs.directory)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
        };
      tests = {
        "conduit-test" = {
          depends = [
            (hsPkgs.conduit)
            (hsPkgs.base)
            (hsPkgs.hspec)
            (hsPkgs.QuickCheck)
            (hsPkgs.transformers)
            (hsPkgs.mtl)
            (hsPkgs.resourcet)
            (hsPkgs.containers)
            (hsPkgs.exceptions)
            (hsPkgs.safe)
            (hsPkgs.split)
            (hsPkgs.mono-traversable)
            (hsPkgs.text)
            (hsPkgs.vector)
            (hsPkgs.directory)
            (hsPkgs.bytestring)
            (hsPkgs.silently)
            (hsPkgs.filepath)
            (hsPkgs.unliftio)
            ];
          };
        };
      benchmarks = {
        "optimize-201408" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.conduit)
            (hsPkgs.vector)
            (hsPkgs.deepseq)
            (hsPkgs.containers)
            (hsPkgs.transformers)
            (hsPkgs.hspec)
            (hsPkgs.mwc-random)
            (hsPkgs.gauge)
            (hsPkgs.kan-extensions)
            ];
          };
        "unfused" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.conduit)
            (hsPkgs.gauge)
            (hsPkgs.transformers)
            ];
          };
        };
      };
    } // { src = "${src}/conduit"; }
