let
  sources = import nix/sources.nix;

  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import sources.haskellNix {};

  # windows crossbuilding with ghc-8.10 needs at least 20.09.
  # A peek at https://github.com/input-output-hk/haskell.nix/blob/master/ci.nix can help
  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
  nixpkgsArgs = haskellNix.nixpkgsArgs;

  pkgs = import nixpkgsSrc nixpkgsArgs;
  pkgs-osx = import nixpkgsSrc (nixpkgsArgs // { system = "x86_64-darwin"; });

  # a nicer filterSource
  sourceByRegex =
    src: regexes: builtins.filterSource (path: type:
      let relPath = pkgs.lib.removePrefix (toString src + "/") (toString path); in
      let match = builtins.match (pkgs.lib.strings.concatStringsSep "|" regexes); in
      ( type == "directory"  && match (relPath + "/") != null
      || match relPath != null)) src;

  patchedSrc = pkgs.applyPatches {
    name = "tttool-src";
    src = sourceByRegex ./. [
      "cabal.project"
      "src/"
      "src/.*/"
      "src/.*.hs"
      ".*.cabal"
      "LICENSE"
      ];
    # Remove the with-compiler flag from cabal.project
    # We include that to help users that want to build with plain cabal but it
    # confuses haskell.nix, so remove it here
    postPatch = ''
      sed -i -e 's/with-compiler/-- with-compiler/' cabal.project
    '';
  };

  tttool-exe = pkgs:
    (pkgs.haskell-nix.cabalProject {
      src = patchedSrc;
      compiler-nix-name = "ghc8102";
      index-state = "2020-11-08T00:00:00Z";
      modules =
      if pkgs.hostPlatform.isMusl
      then
        [{
          packages.tttool.configureFlags = [ "--ghc-option=-static" ];
          # terminfo is disabled on musl by haskell.nix, but still the flag
          # is set in the package plan, so override this
          packages.haskeline.flags.terminfo = false;
        }]
      else [];
    }).tttool.components.exes.tttool;

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

in rec {
  linux-exe      = tttool-exe pkgs;
  windows-exe    = tttool-exe pkgs.pkgsCross.mingwW64;
  static-exe     = tttool-exe pkgs.pkgsCross.musl64;
  osx-exe        = tttool-exe pkgs-osx;
  osx-exe-bundle = osx-bundler pkgs-osx osx-exe;

  macdylibbundler = pkgs.macdylibbundler;

  static-files = sourceByRegex ./. [
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

  contrib = ./contrib;

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
        path = ./book;
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

  gme-downloads = pkgs.runCommandNoCC "gme-downloads" {
    buildInputs = with pkgs; [ wget ];
    outputHashMode = "recursive";
    outputHash =  "sha256:01byby8fmqmxfg5cb5ss0pmhvf2av65sil9cqbjswky0a1mn7kp5";
  } ''
    mkdir -p $out
    bash ${./testsuite/download.sh} $out
  '';

  tests = pkgs.stdenv.mkDerivation {
    name = "tttool-tests";
    phases = "unpackPhase checkPhase installPhase";
    src = builtins.path {
      path = ./testsuite;
      filter = path: type: baseNameOf path != "output";
    };
    doCheck = true;
    buildInputs = [ linux-exe pkgs.glibcLocales ];
    checkPhase = ''
      patchShebangs .
      ln -s ${gme-downloads} downloaded
      ./run.sh
    '';
    installPhase = "touch $out";
  };

  # The following two derivations keep the cabal.config.freeze file
  # up to date.
  cabal-freeze = pkgs.stdenv.mkDerivation {
    name = "cabal-freeze";
    src = linux-exe.src;
    buildInputs = [ pkgs.cabal-install linux-exe.env ];
    buildPhase = ''
      mkdir .cabal
      touch .cabal/config
      rm cabal.project # so that cabal new-freeze does try to use HPDF via git
      HOME=$PWD cabal new-freeze --offline --enable-tests || true
    '';
    installPhase = ''
      mkdir -p $out
      echo "-- Run nix-shell -A check-cabal-freeze to update this file" > $out/cabal.project.freeze
      cat cabal.project.freeze >> $out/cabal.project.freeze
    '';
  };

  check-cabal-freeze = pkgs.runCommandNoCC "check-cabal-freeze" {
      nativeBuildInputs = [ pkgs.diffutils ];
      expected = cabal-freeze + /cabal.project.freeze;
      actual = ./cabal.project.freeze;
      cmd = "nix-shell -A check-cabal-freeze";
      shellHook = ''
        dest=${toString ./cabal.project.freeze}
        rm -f $dest
        cp -v $expected $dest
        chmod u-w $dest
        exit 0
      '';
    } ''
      diff -r -U 3 $actual $expected ||
        { echo "To update, please run"; echo "nix-shell -A check-cabal-freeze"; exit 1; }
      touch $out
    '';
}
