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
    inherit pkgs;
    pkg-def = import ./plan.nix;
    pkg-def-overlays = [
      { tttool = ./tttool.nix;
        HPDF = ./HPDF.nix;
      }];
    modules = [ ];
  };
in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
