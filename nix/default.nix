let
  localLib = import ./lib.nix;

  tttool-exe = args:
    let
      pkgs = localLib.iohkNix.getPkgs args;
      haskell = localLib.nix-tools.haskell { inherit pkgs; };
      nix-tools = import ./pkgs.nix { inherit haskell pkgs; };
    in
    nix-tools.tttool.components.exes.tttool;
in
 { linux-exe = tttool-exe {};
   windows-exe = tttool-exe { crossSystem = localLib.systems.examples.mingwW64;};
 }
