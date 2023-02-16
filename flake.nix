{
  description = "The Tiptoi toolkit";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";

  outputs = { self, haskellNix }:
  let
    drvs = import ./default.nix {
      haskellNix = haskellNix;
    };
  in
  {
    packages.x86_64-linux = {
      inherit (drvs)
        shell
        linux-exe
        windows-exe
        static-exe
        osx-exe
        osx-exe-bundle
        book
        os-switch
        release
        release-zip
        gme-downloads
        cabal-freeze
        check-cabal-freeze;
      default = drvs.release-zip;
    };
  };

  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
