#!/usr/bin/env bash

set -e

token="$(curl \
  --user tiptoi-manager-v2:CYmWkYyhY3traWuGd5cHcNV \
  --data-urlencode grant_type=client_credentials \
  https://oauth.ravensburger.com/oauth/token |
  jq -r .access_token)"

curl --oauth2-bearer "$token"\
  https://ttapiv2.ravensburger.com/api/v2/catalog/de_DE |
  jq -r '.products[].gameFiles[].fileName' > gme-files-all.txt
