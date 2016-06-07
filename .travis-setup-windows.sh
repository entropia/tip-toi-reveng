#!/bin/bash

set -xe
sudo add-apt-repository -y ppa:pipelight/stable
sudo apt-get update
sudo apt-get install --install-recommends wine-staging wine-staging-compat xvfb
wget https://github.com/fpco/minghc/releases/download/2015-12-04/minghc-7.10.2-i386.exe
wine minghc-7.10.2-i386.exe /S | grep -v Extracting
wine cabal --version
wine cabal update
wget http://sourceforge.net/projects/gnuwin32/files/pcre/7.0/pcre-7.0.exe/download -O pcre-7.0.exe
xvfb-run -a wine ./pcre-7.0.exe /VERYSILENT
test -d ~/".wine/drive_c/Program Files (x86)/GnuWin32/include"
wine cabal install --only-dependencies --enable-tests --enable-benchmarks --extra-include-dirs='C:\Program Files (x86)\GnuWin32\include' --extra-lib-dirs='C:\Program Files (x86)\GnuWin32\lib' --constraint 'pcre-light < 0.4.0.4'
# pcre-light 0.4.0.4 requires pkg-config
