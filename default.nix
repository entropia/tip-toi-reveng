{ checkMaterialization ? false }:
let
  sources = import nix/sources.nix;

  # patch haskell.nix to work around
  # https://github.com/input-output-hk/haskell.nix/issues/917
  haskellNixSrc =
    let prepkgs = import sources.nixpkgs {};
    in prepkgs.applyPatches {
      name = "haskell.nix-patched";
      src = sources.haskellNix;
      postPatch = ''
        sed -i -e s/pkgs.evalPackages.fetchgit/pkgs.fetchgit/ lib/cabal-project-parser.nix
      '';
    };

  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import haskellNixSrc {};

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

  tttool-exe = pkgs: sha256:
    (pkgs.haskell-nix.cabalProject {
      src = sourceByRegex ./. [
          "cabal.project"
          "src/"
          "src/.*/"
          "src/.*.hs"
          ".*.cabal"
          "LICENSE"
        ];

      # Pinning the input to the constraint solver
      compiler-nix-name = "ghc8102";
      index-state = "2020-11-08T00:00:00Z";
      plan-sha256 = sha256;
      inherit checkMaterialization;

      # This overrides the `with-compiler` in cabal.project
      # haskell.nix does pass `--with-ghc=…`, but not `-w`, so cabal will
      # still read with-compiler from the project, but there is no way to use
      # an equivalent of `--with-ghc` in `cabal.project`
      # Also see https://github.com/input-output-hk/haskell.nix/issues/916
      cabalProject = builtins.replaceStrings
        [ "with-compiler" ]
        [ "-- with-compiler" ]
        (builtins.readFile ./cabal.project);

      modules = [{
        # smaller files
        packages.tttool.dontStrip = false;
      }] ++
      pkgs.lib.optional pkgs.hostPlatform.isMusl {
        packages.tttool.configureFlags = [ "--ghc-option=-static" ];
        # terminfo is disabled on musl by haskell.nix, but still the flag
        # is set in the package plan, so override this
        packages.haskeline.flags.terminfo = false;
      };
    }).tttool.components.exes.tttool;

  osx-bundler = pkgs: tttool:
   pkgs.stdenv.mkDerivation {
      name = "tttool-bundle";

      buildInputs = [ pkgs.macdylibbundler ];

      builder = pkgs.writeScript "tttool-osx-bundler.sh" ''
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
  linux-exe      = tttool-exe pkgs
     "0rnn4q0gx670nzb5zp7xpj7kmgqjmxcj2zjl9jqqz8czzlbgzmkh";
  windows-exe    = tttool-exe pkgs.pkgsCross.mingwW64
     "01js5rp6y29m7aif6bsb0qplkh2az0l15nkrrb6m3rz7jrrbcckh";
  static-exe     = tttool-exe pkgs.pkgsCross.musl64
     "0gbkyg8max4mhzzsm9yihsp8n73zw86m3pwvlw8170c75p3vbadv";
  osx-exe        = tttool-exe pkgs-osx
     "0rnn4q0gx670nzb5zp7xpj7kmgqjmxcj2zjl9jqqz8czzlbgzmkh";
  osx-exe-bundle = osx-bundler pkgs-osx osx-exe;

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
      linux*)   exec "$(dirname "''${BASH_SOURCE[0]}")/linux/tttool" "$@" ;;
      darwin*)  exec "$(dirname "''${BASH_SOURCE[0]}")/osx/tttool" "$@" ;;
      msys*)    exec "$(dirname "''${BASH_SOURCE[0]}")/tttool.exe" "$@" ;;
      cygwin*)  exec "$(dirname "''${BASH_SOURCE[0]}")/tttool.exe" "$@" ;;
      *)        echo "unsupported operating system $OSTYPE" ;;
    esac
  '';

  release = pkgs.runCommandNoCC "tttool-release" {
    buildInputs = [ pkgs.perl ];
  } ''
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
    cp -vs ${windows-exe}/bin/* $out/
    mkdir $out/osx
    cp -vsr ${osx-exe-bundle}/bin/osx/* $out/osx
    cp -vs ${os-switch} $out/tttool
    mkdir $out/contrib
    cp -vsr ${contrib}/* $out/contrib/
    cp -vsr ${book}/* $out
  '';

  release-zip = pkgs.runCommandNoCC "tttool-release.zip" {
    buildInputs = with pkgs; [ perl zip ];
  } ''
    version=$(bash ${release}/tttool --help|perl -ne 'print $1 if /tttool-(.*) -- The swiss army knife/')
    base="tttool-$version"
    echo "Zipping tttool version $version"
    mkdir -p $out/$base
    cd $out
    cp -r ${release}/* $base/
    chmod u+w -R $base
    zip -r $base.zip $base
    rm -rf $base
  '';

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
      rm cabal.project # so that cabal new-freeze does not try to use HPDF via git
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
