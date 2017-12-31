#!/bin/bash

set -xe
sudo add-apt-repository -y ppa:pipelight/stable
sudo apt-get update
sudo apt-get install --install-recommends wine-staging wine-staging-compat xvfb
wget https://github.com/fpco/minghc/releases/download/2015-12-04/minghc-7.10.2-i386.exe
wine minghc-7.10.2-i386.exe /S | grep -v Extracting
wine cabal --version
wine cabal update
wine cabal install --only-dependencies --enable-tests --enable-benchmarks
