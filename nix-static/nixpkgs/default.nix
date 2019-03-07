let
  fetchFromGitHub = if (builtins ? "fetchTarball")
    then { owner, repo, rev, sha256 }: builtins.fetchTarball {
        inherit sha256;
        url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      }
    else (import <nixpkgs> {}).fetchFromGitHub;
in import (fetchFromGitHub (builtins.fromJSON (builtins.readFile ./github.json)))
