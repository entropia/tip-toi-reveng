let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          HPDF = haskellPackagesNew.callPackage ./HPDF.nix { };
          tttool = haskellPackagesNew.callPackage ./project.nix { };
        };
      };
    };
  };

  pkgs = import ./nixpkgs { inherit config; };

in
  pkgs.haskellPackages.tttool
