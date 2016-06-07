#!/bin/bash

set -xe
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install cabal-install-$CABALVER ghc-$GHCVER # see note about happy/alex
sudo apt-get install libxss-dev
cabal --version
echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
cabal update
cabal install --only-dependencies --enable-tests --enable-benchmarks
