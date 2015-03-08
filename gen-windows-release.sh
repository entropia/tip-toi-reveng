#!/bin/bash

set -e
wine cabal install --distdir=dist-win --bindir=.
(cd Audio/digits/; ./build.sh)

rev=$(git describe --tags)
zipfile=tttool-win32-$rev.zip 
rm -f $zipfile

OGGENC=oggenc2.87-1.3.4-generic.zip 

mkdir -p contrib
test -e contrib/$OGGENC ||
    wget http://www.rarewares.org/files/ogg/$OGGENC -O contrib/$OGGENC 
unzip -d contrib contrib/$OGGENC oggenc2.exe
mv contrib/oggenc2.exe contrib/oggenc.exe 


# install espeak first in wine
cp ~/.wine/drive_c/Programme/eSpeak/command_line/espeak.exe contrib/
cp -r ~/.wine/drive_c/Programme/eSpeak/espeak-data/ contrib/

zip --recurse-paths $zipfile \
	tttool.exe \
	README.md \
	Changelog.md \
	oid-decoder.html \
	example \
	example.yaml \
	Debug.yaml \
	oid-table.png \
	templates/README.md \
	templates/*.yaml \
	transcript/*.csv \
	wip/* \
	Audio/digits/*.ogg \
        contrib/oggenc.exe \
        contrib/espeak.exe \
        contrib/espeak-data
echo Created $zipfile
