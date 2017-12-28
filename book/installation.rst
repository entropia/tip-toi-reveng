.. _installation:

Installation
============

Um eigene Tiptoi-Bücher zu gestalten brauchst du das ``tttool``.

Windows
-------

Unter Windows lädst du von https://github.com/entropia/tip-toi-reveng/releases die neueste Version als Zip-Datei (``tttool-win32-version.zip``) herunter, und extrahierst sie. Du kannst das ``tttool`` direkt verwenden, es ist keine weitere Installation nötig.

Linux (Debian, Ubuntu und ähnliche)
-----------------------------------

Unter Linux kompilierst du das ``tttool`` selbst. Dazu brauchst du erst einmal ein paar Pakete::

$ sudo apt-get install git haskell-platform libncurses5-dev libttspico-utils vorbis-tools

Dann kannst lädst du dir den aktuellen Quellcode des ``tttool`` herunter::

$ git clone https://github.com/entropia/tip-toi-reveng.git tttool
$ cd tttool

Folgende Befehlsfolge installiert ein paar Bibliotheken und das ``tttool`` selbst::

$ cabal update
$ cabal install --only-dependencies
$ cabal install --bindir=.

Nun solltest du im aktuellen Verzeichnis eine Datei ``tttool`` finden, die du ausführen kannst::

  $ ./tttool --help
  Usage: tttool [-t|--transscript FILE] [--code-dim W[xH]] [--dpi DPI]
                [--pixel-size N] COMMAND
    tttool-1.6.1 -- The swiss army knife for the Tiptoi hacker
  …


Sonstige (MacOS, andere Linuxe)
-------------------------------

Im Grunde kannst du dich an der Anleitung für Debian und Ubuntu oben orientieren, lediglich den ersten Befehl, der vor allem den Haskell-Compiler installiert, muss du anpassen. Vielleicht helfen dir die `Informationen zur Installation of Haskell <https://www.haskell.org/downloads#minimal>`_ weiter.
