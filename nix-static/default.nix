{
  static ? false,
  windows ? false
}:

let mypkgs = new: old: rec {
  HPDF = new.callPackage ./HPDF.nix { };
  tttool = new.callPackage ./project.nix {inherit static; };
}; in

let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override { overrides = mypkgs; };
      haskell = pkgs.haskell // {
	packages = pkgs.haskell.packages // {
	  ghc843 = pkgs.haskell.packages.ghc843.override { overrides = mypkgs; };
	  ghc863 = pkgs.haskell.packages.ghc863.override { overrides = mypkgs; };
        };
      };
    };
  };

  pkgs = import ./nixpkgs { inherit config; };

in
  if windows
  then pkgs.pkgsCross.mingwW64.haskellPackages.tttool
  else if static
  then pkgs.pkgsCross.musl64.haskell.packages.ghc864.tttool
  else pkgs.haskellPackages.tttool
