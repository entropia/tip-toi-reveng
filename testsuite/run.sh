#!/usr/bin/env bash

set -e

cd "$(dirname "${BASH_SOURCE[0]}")"

if [ -z "$TTTOOL" ]; then
  if [ -x ../tttool ]; then
    TTTOOL="../tttool"
  else
    TTTOOL="tttool"
  fi
fi
export TTTOOL

# utf8 output (why does C.UTF-8 not work?)
export LANG=en_US.UTF-8

if ! $TTTOOL --help >/dev/null
then
  echo "Cannot run $TTTOOL, aborting"
  exit 1
fi

echo "tttool test suite: Using \"$TTTOOL\""

while getopts "adpstir" o; do
    case "${o}" in
        a)
            ACCEPT=yes
            ;;
        *)
          echo "Invalid arguments"
          exit 1
          ;;
    esac
done

shift $((OPTIND-1))

if ! [ -d downloaded/ ];
then
  echo "Missing downloaded/. Did you run download.sh?"
  exit 1
fi

rm -rf output

./test.sh

if ! diff -N -r -q expected output >/dev/null
then
  echo "Unexpected test output:"
  if [ "$ACCEPT" = yes ]; then
    echo "Accepting as new expected output"
    rm -rf expected
    mv output expected
    exit 0
  else
    diff -N -u -r expected output || true
    # lets leave this around to compare binaries
    # rm -rf output
    exit 1
  fi
else
  echo "All good!"
  #rm -rf output
  exit 0
fi

