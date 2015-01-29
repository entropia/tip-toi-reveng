#!/bin/bash

set -e
wine cabal install --bindir=.
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
	debug.yaml \
	oid-table.png \
	templates/README.md \
	templates/*.yaml \
	transcript/*.csv \
	wip/* \
	Audio/digits/*.ogg
echo Created $zipfile
