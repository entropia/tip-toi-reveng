#!/usr/bin/env bash

set -e

curl http://catalog.tiptoi.de/tiptoi.json |
  jq -r '.products[].gameFiles[].fileName' | sort > gme-files-all.txt
