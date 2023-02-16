{
  description = "The Tiptoi toolkit";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";

  outputs = { self, haskellNix }:
  let
    # Lets import the “old style” nix setup
    drvs = import ./default.nix
      { haskellNix = haskellNix; checkMaterialization = false; };
    drvs-check-materialization = import ./default.nix
      { haskellNix = haskellNix; checkMaterialization = true; };
  in
  {
    packages.x86_64-linux = {
      inherit (drvs)
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
        cabal-freeze;
      default = drvs.release-zip;
    };

    checks.x86_64-linux = {
        checkMaterialization1 = drvs-check-materialization.release;
        checkMaterialization2 = drvs-check-materialization.linux-exe;
        inherit (drvs) check-cabal-freeze;
    };

    devShells.x86_64-linux.default = drvs.shell;
    devShells.x86_64-linux.update-cabal-freeze = drvs.check-cabal-freeze;
  };

  nixConfig = {
    extra-substituters = ["https://cache.iog.io" "https://tttool.cachix.org"];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "tttool.cachix.org-1:e/5HpIa6ZqwatH07kmO7di1p9K+AMrgkNHl/OGUUMzU="
    ];
    allow-import-from-derivation = "true";
  };
}
