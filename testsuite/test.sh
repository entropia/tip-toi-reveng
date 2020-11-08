#!/usr/bin/env bash

set -e
set -o pipefail

if [ -z "$TTTOOL" ]; then
  if [ -x ../tttool ]; then
    TTTOOL="../tttool"
  else
    TTTOOL="tttool"
  fi
fi
export TTTOOL

if ! [ -d input/ ];
then
  echo "Input directory input/ is misisng or not a directory"
  exit 1
fi

if ! [ -d downloaded/ ];
then
  echo "Input directory downloaded' is missing or not a directory"
  echo "Did you run download.sh"
  exit 1
fi

if [ -d output/ ];
then
  echo "Directory output/ exists, please delete"
  exit 1
fi

mkdir output

# clean input
rm -f input/*.codes.yaml

# Guard against changes to the help output
$TTTOOL --help > "output/help.txt"

for cmd in $($TTTOOL --help |grep '^  [a-z]'|cut -d\  -f3 | grep -v ^tttool); do
  $TTTOOL "$cmd" --help > "output/help-$cmd.txt"
done


# Simple example files
$TTTOOL assemble --no-date "input/example.yaml" "output/example.gme"
rm -f "input/example.yaml.codes"

$TTTOOL info "output/example.gme" > "output/example.info.txt"
$TTTOOL rewrite "output/example.gme" "output/example.rewritten.gme"
$TTTOOL export "output/example.gme" "output/example.rexport.yaml"

mkdir output/downloaded
for gme in downloaded/*.gme; do
  $TTTOOL info "$gme" > "output/$gme.info.txt"
  $TTTOOL lint "$gme" > "output/$gme.lint.txt"
  $TTTOOL export "$gme" "output/$gme.yaml"
  # lets not keep the yaml files in the repo, in case they are copyrightabled
  md5sum "output/$gme.yaml" > "output/$gme.yaml.md5sum"
  rm -f "output/$gme.yaml"
done

# normalize test output
find "output" -name \*.txt -print0 |
  xargs -0 -r sed -i -e 's/tttool-\([0-9]\+\.\)*[0-9]\+/tttool-VERSION/'

# clean input
rm -f input/*.codes.yaml
