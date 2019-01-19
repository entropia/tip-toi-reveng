.. _installation:

Installation
============

Um eigene Tiptoi-Bücher zu gestalten brauchst du das ``tttool``.

Windows
-------

Unter Windows lädst du von https://github.com/entropia/tip-toi-reveng/releases die neueste Version als Zip-Datei (``tttool-win32-version.zip``) herunter, und extrahierst sie. Du kannst das ``tttool`` direkt verwenden, es ist keine weitere Installation nötig.

Linux (Debian, Ubuntu und ähnliche)
-----------------------------------

Unter Linux kompilierst du das ``tttool`` selbst.

1. Zuerst installierst du ``git``::

    $ sudo apt-get install git

2. Dann kannst lädst du dir den aktuellen Quellcode des ``tttool`` herunter::

    $ git clone https://github.com/entropia/tip-toi-reveng.git tttool
    $ cd tttool

3. Nun installierst du das Werkzeug ``stack``. Das geht am einfachsten mit

  .. code::

    curl -sSL https://get.haskellstack.org/ | sh

  oder du folgst den ausführlicheren Anweisungen auf der `Stack-Homepage <https://docs.haskellstack.org/en/stable/install_and_upgrade/>`_.

4. Zulezt baust du ``tttool``. Das dauert eine Weile::

    stack install --local-bin-path .


Nun solltest du im aktuellen Verzeichnis eine Datei ``tttool`` finden, die du ausführen kannst::

  $ ./tttool --help
  Usage: tttool [-t|--transscript FILE] [--code-dim W[xH]] [--dpi DPI]
                [--pixel-size N] COMMAND
    tttool-1.6.1 -- The swiss army knife for the Tiptoi hacker
  …


macOS
-----

Die Installation unter macOS verläuft ähnlich zu der unter Linux in einem Terminal-Fenster. Zunächst benötigst du, falls noch nicht installiert, die XCode command line tools::

$ xcode-select --install

Weiterhin benötigst du den Paketmanager Homebrew (https://brew.sh/)::

$ /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

Mithilfe von Homebrew installierst du Haskell::

$ brew install ghc cabal-install

Nun besorgst du dir die Quellen::

$ git clone https://github.com/entropia/tip-toi-reveng.git tttool
$ cd tttool

und kompilierst sie::

$ cabal update
$ cabal install --only-dependencies
$ cabal install --bindir=.

Wie zuvor befindet sich das fertige tttool nun im aktuellen Verzeichnis::

$ ./tttool --help
