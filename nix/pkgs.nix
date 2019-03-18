# let plan = import ./plan.nix; in
# { pkg-def = plan;
#   overlay =
#     { tttool = ./tttool.nix;
#       HPDF = ./HPDF2.nix;
#     };
# }

{ pkgs ? import <nixpkgs> {}
, haskell
, iohk-module
, iohk-overlay
, ...
}:
let

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  plan = import ./plan.nix;
  compiler = (plan haskell).compiler.nix-name;
  pkgSet = haskell.mkPkgSet {
    pkg-def = plan;
    pkg-def-extras = [
      iohk-overlay.${compiler}
      { tttool = ./tttool.nix;
        HPDF = ./HPDF.nix;
        conduit = ./conduit.nix;
      }
      (hackage: { Win32 = hackage.Win32."2.6.2.0".revisions.default; })
      (hackage: { mintty = hackage.mintty."0.1.2".revisions.default; })
      ];
    modules = [
      # haskell.ghcHackagePatches.${compiler}
      (iohk-module { nixpkgs = pkgs; th-packages = [ "tttool" ]; })
      {
        # The plan produced by plan-to-nix does not include all necessary flag assignments
        packages.haskeline.flags.terminfo = false;
        packages.transformers-compat.flags.two = false;
        packages.transformers-compat.flags.three = false;
        packages.transformers-compat.flags.four = false;
        packages.transformers-compat.flags.five = false;
        packages.transformers-compat.flags.five-three = true;
        packages.time-locale-compat.flags.old-locale = false;
        # Configure static building of tttool
        packages.tttool.configureFlags = pkgs.lib.optionals (pkgs.hostPlatform.isMusl) [
           "--ghc-option=-static"
           "--ghc-option=-optl=-static"
           "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
           "--extra-lib-dirs=${pkgs.zlib.static}/lib"
         ];
      }
    ];
  };
in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
