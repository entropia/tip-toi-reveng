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
      haskell = pkgs.callPackage localLib.nix-tools.haskell {};
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

  osx-bundler = pkgs: tttool:
   pkgs.stdenv.mkDerivation {
      name = "tttool-bundle";

      buildInputs = [ pkgs.macdylibbundler ];

      builder = pkgs.writeScript "zip-tttool-release.sh" ''
        source ${pkgs.stdenv}/setup

        mkdir -p $out/bin/osx
        cp ${tttool}/bin/tttool $out/bin/osx
        chmod u+w $out/bin/osx/tttool
        dylibbundler \
          -b \
          -x $out/bin/osx/tttool \
          -d $out/bin/osx \
          -p '@executable_path' \
          -i /usr/lib/system \
          -i ${pkgs.darwin.Libsystem}/lib
      '';
    };

in

let
  overlay = self: super:
    {
      macdylibbundler = import ./macdylibbundler.nix { inherit (self) stdenv fetchFromGitHub; };
    };
  getPkgs = opts: localLib.iohkNix.getPkgs (opts // { extraOverlays = [ overlay ];});

  pkgs         = getPkgs {};
  pkgs-static  = getPkgs { crossSystem = localLib.systems.examples.musl64; };
  pkgs-windows = getPkgs { crossSystem = localLib.systems.examples.mingwW64; };
  pkgs-osx     = getPkgs { system = "x86_64-darwin"; };

  sourceByRegex = import ./source-by-regex.nix pkgs;

in rec {
  linux-exe = tttool-exe pkgs;
  windows-exe = tttool-exe pkgs-windows;
  static-exe = tttool-exe pkgs-static;
  osx-exe = tttool-exe pkgs-osx;
  osx-exe-bundle = osx-bundler pkgs-osx osx-exe;

  macdylibbundler = pkgs.macdylibbundler;

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

  os-switch = pkgs.writeScript "tttool-os-switch.sh" ''
    #!/usr/bin/env bash
    case "$OSTYPE" in
      linux*)   exec "$(dirname "$BASH_SOURCE")/linux/tttool" "$@" ;;
      darwin*)  exec "$(dirname "$BASH_SOURCE")/osx/tttool" "$@" ;;
      msys*)    exec "$(dirname "$BASH_SOURCE")/tttool.exe" "$@" ;;
      cygwin*)  exec "$(dirname "$BASH_SOURCE")/tttool.exe" "$@" ;;
      *)        echo "unsupported operating system $OSTYPE" ;;
    esac
  '';

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
      mkdir $out/linux
      cp -vs ${static-exe}/bin/tttool $out/linux
      cp -vs ${windows-exe}/bin/tttool.exe $out/
      mkdir $out/osx
      cp -vsr ${osx-exe-bundle}/bin/osx/* $out/osx
      cp -vs ${os-switch} $out/tttool
      mkdir $out/contrib
      cp -vsr ${contrib}/* $out/contrib/
      cp -vsr ${book}/* $out
    '';
  };

  release-zip = pkgs.stdenv.mkDerivation {
    name = "tttool-release.zip";

    buildInputs = with pkgs; [ perl zip bash ];

    builder = pkgs.writeScript "zip-tttool-release.sh" ''
      source ${pkgs.stdenv}/setup

      version=$(bash ${release}/tttool --help|perl -ne 'print $1 if /tttool-(.*) -- The swiss army knife/')
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
