#!/bin/bash

# This script creates ogg audio files speaking the digits 0 to 9 in english.
# Requires flac (package flac) and oggenc (package vorbis-tools)

if test -d digits
then
  cd digits
fi

if ! test -d ../source
then
  echo "Please run me in the digits/ directory"
  exit 1
fi

function create () {
	out="$1";
	shift;
	echo "Creating $out.ogg..."
	flac -s -d --endian little --sign signed --force-raw-format -c "$@" |
	oggenc -Q --downmix --resample 22050 -r - -o $out.ogg
}

create blob ../source/blob.flac
for d in $(seq 0 9)
do
	create english-$d ../source/english-$d.flac
done
