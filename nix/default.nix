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
  pkgs-osx = localLib.iohkNix.getPkgs { system = "x86_64-darwin"; };

  sourceByRegex = import ./source-by-regex.nix pkgs;

in rec {
  linux-exe = tttool-exe pkgs;
  windows-exe = tttool-exe pkgs-windows;
  static-exe = tttool-exe pkgs-static;
  osx-exe = tttool-exe pkgs-osx;

  # playmus-static = playmus-exe pkgs-static;
  # playmus-windows = playmus-exe pkgs-windows;

  static-files = sourceByRegex ../. [
    "README.md"
    "Changelog.md"
    "oid-decoder.html"
    "example/.*"
    "Debug.yaml"
    "templates/"
    "templates/.*\.md"
    "templates/.*\.yaml"
    "Audio/"
    "Audio/digits/"
    "Audio/digits/.*\.ogg"
  ];

  contrib = ../contrib;

  book =
    let
      sphinx-env = pkgs.python.withPackages(ps: [
        ps.sphinx
        ps.recommonmark
      ]);
      tex = pkgs.texlive.combine {
        inherit (pkgs.texlive)
          scheme-basic latexmk cmap collection-fontsrecommended
          fncychap titlesec tabulary varwidth framed fancyvrb float parskip
          wrapfig upquote capt-of needspace;
      };
    in
    pkgs.stdenv.mkDerivation {
      name = "tttool-book";

      buildInputs = [ sphinx-env tex ];

      src = builtins.path {
        path = ../book;
        name = "book";
        filter = path: type:
          baseNameOf path != "_build" &&
          baseNameOf path != ".gitignore";
      };

      buildPhase = ''
        source ${pkgs.stdenv}/setup
        make html
        make latexpdf
        rm -f _build/html/.buildinfo
        rm -rf _build/html/_sources
      '';

      installPhase = ''
        mkdir -p $out/
        mv _build/html $out/book.html
        mv _build/latex/tttool.pdf $out/book.pdf
      '';
    };

  release = pkgs.stdenv.mkDerivation {
    name = "tttool-release";

    buildInputs = [ pkgs.perl ];

    builder = pkgs.writeScript "create-tttool-release.sh" ''
      source ${pkgs.stdenv}/setup

      # check version
      version=$(${static-exe}/bin/tttool --help|perl -ne 'print $1 if /tttool-(.*) -- The swiss army knife/')
      doc_version=$(perl -ne "print \$1 if /VERSION: '(.*)'/" ${book}/book.html/_static/documentation_options.js)

      if [ "$version" != "$doc_version" ]
      then
        echo "Mismatch between tttool version \"$version\" and book version \"$doc_version\""
        exit 1
      fi

      mkdir -p $out/
      cp -vsr ${static-files}/* $out
      cp -vs ${static-exe}/bin/tttool $out/
      cp -vs ${windows-exe}/bin/tttool.exe $out/
      mkdir -p $out/contrib
      cp -vsr ${contrib}/* $out/contrib/
      cp -vsr ${book}/* $out
    '';
  };

  release-zip = pkgs.stdenv.mkDerivation {
    name = "tttool-release.zip";

    buildInputs = [ pkgs.perl pkgs.zip ];

    builder = pkgs.writeScript "zip-tttool-release.sh" ''
      source ${pkgs.stdenv}/setup

      version=$(${release}/tttool --help|perl -ne 'print $1 if /tttool-(.*) -- The swiss army knife/')
      base="tttool-$version"
      echo $version
      mkdir -p $out/$base
      cd $out
      cp -r ${release}/* $base/
      chmod u+w -R $base
      zip -r $base.zip $base
      rm -rf $base
    '';
  };

}
