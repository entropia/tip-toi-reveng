let
  localLib = import ./lib.nix;
in
localLib.nix-tools.release-nix {
  package-set-path = ./.;
  packages = [ "tttool" ];
  required-targets = jobs: [
    jobs.nix-tools.exes.tttool.x86_64-linux
    # windows cross compilation targets
    jobs.nix-tools.exes.x86_64-pc-mingw32-tttool.x86_64-linux
];
}
