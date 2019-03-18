let localLib = import ./lib.nix; in

let
  tttool-exe = pkgs:
    let
      # haskellLib = pkgs.fetchFromGitHub {
      #   owner  = "input-output-hk";
      #   repo   = "haskell.nix";
      #   rev    = "8ee6fcfba7bb220e8d6e19106ad2ae2c25ecdf43";
      #   sha256 = "1ndpxyairppcr3mnb4zi7gsvdqmncp09fgxdk8cbrh7ccb1r30kz";
      #   fetchSubmodules = false;
      #   name   = "haskell-lib-source";
      # };
      # haskell = import haskellLib { inherit pkgs; };
      haskell = localLib.nix-tools.haskell { inherit pkgs; };
      iohk-module = localLib.nix-tools.iohk-module;
      iohk-extras = localLib.nix-tools.iohk-extras;
      nix-tools = import ./pkgs.nix { inherit pkgs haskell iohk-module iohk-extras; };
    in
    nix-tools.tttool.components.exes.tttool;

  playmus-exe = pkgs: pkgs.stdenv.mkDerivation {
    name = "playmus";
    src = ../playmus;
    buildInputs = [ pkgs.SDL pkgs.SDL_mixer ];
    builder = pkgs.writeScript "compile-playmus" ''
      source ${pkgs.stdenv}/setup
      mkdir -p $out/bin
      find
      pwd
      gcc $src/playmus.c -o $out/bin/playmus `sdl-config --cflags --libs` -lSDL_mixer -static -optl=-static
    '';
  };

in

let
  pkgs = localLib.iohkNix.getPkgs {};
  pkgs-static = localLib.iohkNix.getPkgs { crossSystem = localLib.systems.examples.musl64; };
  pkgs-windows = localLib.iohkNix.getPkgs { crossSystem = localLib.systems.examples.mingwW64; };

  sourceByRegex = import ./source-by-regex.nix pkgs;

in rec {
  linux-exe = tttool-exe pkgs;
  windows-exe = tttool-exe pkgs-windows;
  static-exe = tttool-exe pkgs-static;

  # playmus-static = playmus-exe pkgs-static;
  # playmus-windows = playmus-exe pkgs-windows;

  static-files = sourceByRegex ../. [
    "README.md"
    "Changelog.md"
    "oid-decoder.html"
    "example/.*"
    "Debug.yaml"
    "templates/"
    "templates/.*md"
    "templates/.*yaml"
    "Audio/"
    "Audio/digits/.*"
  ];

  contrib = ../contrib;

  release = pkgs.stdenv.mkDerivation {
    name = "tttool-release";

    buildInputs = [ static-exe ];

    builder = pkgs.writeScript "create-tttool-release.sh" ''
      source ${pkgs.stdenv}/setup

      mkdir -p $out/
      cp -vsr ${static-files}/* $out
      cp -v ${static-exe}/bin/tttool $out/
      cp -v ${windows-exe}/bin/tttool.exe $out/
      mkdir -p $out/contrib
      cp -vr ${contrib}/* $out/contrib/
    '';
  };

  release-zip = pkgs.stdenv.mkDerivation {
    name = "tttool-release.zip";

    buildInputs = [ release pkgs.perl pkgs.zip ];

    builder = pkgs.writeScript "zip-tttool-release.sh" ''
      source ${pkgs.stdenv}/setup

      version=$(${release}/tttool --help|perl -ne 'print $1 if /tttool-(.*) -- The swiss army knife/')
      echo $version
      mkdir -p $out/
      cd ${release}
      zip -r $out/tttool-$version.zip .
    '';
  };

}
