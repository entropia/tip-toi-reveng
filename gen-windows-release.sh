#!/bin/bash

set -e
wine cabal install --distdir=dist-win --bindir=.
(cd Audio/digits/; ./build.sh)

rev=$(git describe --tags)
zipfile=tttool-win32-$rev.zip
rm -f $zipfile

# Oggenc
OGGENC=oggenc2.87-1.3.5-generic.zip

mkdir -p contrib
test -e contrib/$OGGENC ||
    wget http://www.rarewares.org/files/ogg/$OGGENC -O contrib/$OGGENC
unzip -d contrib contrib/$OGGENC oggenc2.exe
mv contrib/oggenc2.exe contrib/oggenc.exe

#SDL
SDL=SDL-1.2.15-win32.zip
SDLMIXER=SDL_mixer-1.2.12-win32.zip

test -e contrib/$SDL ||
    wget http://libsdl.org/release/$SDL -O contrib/$SDL
unzip -o -d contrib contrib/$SDL
test -e contrib/$SDLMIXER ||
    wget https://www.libsdl.org/projects/SDL_mixer/release/$SDLMIXER -O contrib/$SDLMIXER
unzip -o -d contrib contrib/$SDLMIXER

cp -v playmus/playmus.exe contrib/playmus.exe

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
        contrib/espeak-data \
        contrib/LICENSE* \
        contrib/README-SDL.txt \
	contrib/*.dll \
	contrib/playmus.exe
echo Created $zipfile
