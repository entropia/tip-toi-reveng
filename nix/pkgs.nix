# let plan = import ./plan.nix; in
# { pkg-def = plan;
#   overlay =
#     { tttool = ./tttool.nix;
#       HPDF = ./HPDF2.nix;
#     };
# }

{ pkgs ? import <nixpkgs> {}
, haskell
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
  pkgSet = haskell.mkPkgSet {
    pkg-def = import ./plan.nix;
    pkg-def-overlays = [
      { tttool = ./tttool.nix;
        HPDF = ./HPDF.nix;
        conduit = ./conduit.nix;
      }
      (hackage: { Win32 = hackage.Win32."2.6.2.0".revisions.default; })
      (hackage: { mintty = hackage.mintty."0.1.2".revisions.default; })
      ];
    modules = [ ];
  };
in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
