let
  localLib = import ./lib.nix;
in
{ ... }@args:
localLib.nix-tools.default-nix ./pkgs.nix args


# { pkgs ? import ./nixpkgs {}
# }:
# let
#   overrideWith = override: default:
#    let
#      try = builtins.tryEval (builtins.findFile builtins.nixPath override);
#    in if try.success then
#      builtins.trace "using search host <${override}>" try.value
#    else
#      default;
# in
# 
# let
#   haskellLib = pkgs.fetchFromGitHub {
#     owner  = "input-output-hk";
#     repo   = "haskell.nix";
#     rev    = "3c78fdaede42abe88936a55ecffbadf533cb3d28";
#     sha256 = "06rmib2hza6s5gvx52dqw905pc4rmcwxj7xyafk21nbbh76l5rmw";
#     name   = "haskell-lib-source";
#   };
#   haskell = import (overrideWith "haskell" haskellLib) { inherit pkgs; };
# in
# 
# let
#   my-pkgs = import ./pkgs.nix;
# 
#   pkgSet = haskell.mkPkgSet {
#     pkg-def = my-pkgs.pkg-def;
#     pkg-def-overlays = [
#       # this overlay will provide additional packages
#       # ontop of the package set.  E.g. extra-deps
#       # for stack packages. or local packages for
#       # cabal.projects
#       my-pkgs.overlay
#     ];
#     modules = [
#       # specific package overrides would go here
#       # example:
#       #  packages.cbors.patches = [ ./one.patch ];
#       #  packages.cbors.flags.optimize-gmp = false;
#     ];
#   };
# 
# in pkgSet.config.hsPkgs // { _config = pkgSet.config; }
