#!/usr/bin/env bash

set -e
dir="${1:-downloaded}"

mkdir -p "$dir"
cd "$dir"

# We used to have a script that would fetch an .xml file from ravensburger
# that lists all GME files, but that XML file disappeared.
# For now, just list some GME files

function get() {
  wget --no-verbose --timestamping --no-check-certificate \
    https://ssl-static.ravensburger.de/db/applications/"$1"
}

get WissenQuizzen1.gme
get Pocketwissen_Feuerwehr.gme
