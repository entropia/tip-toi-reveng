#!/bin/bash

set -e
wine ghc -O -with-rtsopts=-K100M  tttool.hs
(cd Audio/digits/; ./build.sh)

rev=$(git describe --always)
zipfile=tttool-win32-$rev.zip 
rm -f $zipfile

zip $zipfile \
	tttool.exe \
	README.md \
	oid-decoder.html \
	example \
	example.yaml \
	templates/README.md \
	templates/*.yaml \
	transcript/*.csv \
	wip/* \
	Audio/digits/*.ogg
echo Created $zipfile
