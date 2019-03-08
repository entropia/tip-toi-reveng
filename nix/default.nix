let
  localLib = import ./lib.nix;

in
 { linux-exe = (localLib.nix-tools.default-nix ./pkgs.nix {}).nix-tools.exes.tttool;
   windows-exe = (localLib.nix-tools.default-nix ./pkgs.nix { crossSystem = localLib.systems.examples.mingwW64; }).nix-tools.exes.tttool;
 }
