#!/bin/bash

# This script creates ogg audio files speaking the numbers from 0000 to 1999 in
# english.
# Requires flac (package flac) and oggenc (package vorbis-tools)

if test -d numbers
then
  cd numbers
fi

if ! test -d ../source
then
  echo "Please run me in the numbers/ directory"
  exit 1
fi

function create () {
	out="$1";
	shift;
	echo "Creating $out.ogg..."
	flac -s -d --endian little --sign signed --force-raw-format -c "$@" |
	oggenc -Q --downmix --resample 22050 -r - -o $out.ogg
}

for d4 in $(seq 0 9); do
create 000$d4 ../source/blob.flac ../source/english-$d4.flac
done

for d3 in $(seq 1 9); do
for d4 in $(seq 0 9); do
create 00$d3$d4 ../source/blob.flac ../source/english-{$d3,$d4}.flac
done; done

for d2 in $(seq 1 9); do
for d3 in $(seq 0 9); do
for d4 in $(seq 0 9); do
create 0$d2$d3$d4 ../source/blob.flac ../source/english-{$d2,$d3,$d4}.flac
done; done; done

for d2 in $(seq 0 9); do
for d3 in $(seq 0 9); do
for d4 in $(seq 0 9); do
create 1$d2$d3$d4 ../source/blob.flac ../source/english-{1,$d2,$d3,$d4}.flac
done; done; done
