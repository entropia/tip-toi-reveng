let
  localLib = import ./lib.nix;

  tttool-exe = args:
    let
      pkgs = localLib.iohkNix.getPkgs args;
      haskellLib = pkgs.fetchFromGitHub {
        owner  = "nomeata";
        repo   = "haskell.nix";
        rev    = "fd36e3f8256694884cebbe72d63177358ab15b0e";
        sha256 = "0faxzn7labgf4jc75rrmnzq20bap2xpfv3paxg6fcfwwydcnnacs";
        fetchSubmodules = false;
        name   = "haskell-lib-source";
      };
      haskell = import haskellLib { inherit pkgs; };
      nix-tools = import ./pkgs.nix { inherit haskell pkgs; };
    in
    nix-tools.tttool.components.exes.tttool;
in
 { linux-exe = tttool-exe {};
   windows-exe = tttool-exe { crossSystem = localLib.systems.examples.mingwW64;};
   static-exe = tttool-exe { crossSystem = localLib.systems.examples.musl64;};
 }
