#!/usr/bin/env bash

set -e
gmes="${1:-gme-files-test.txt}"
dir="${2:-downloaded}"

mkdir -p "$dir"
while read -r filename; do
  wget --no-verbose --timestamping --no-check-certificate -P "$dir" \
    https://ssl-static.ravensburger.de/db/applications/"$filename"
done < "$gmes"
