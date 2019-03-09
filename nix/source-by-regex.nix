pkgs: src: regexes: builtins.filterSource (path: type:
  let relPath = pkgs.lib.removePrefix (toString src + "/") (toString path); in
  let match = builtins.match (pkgs.lib.strings.concatStringsSep "|" regexes); in
  ( type == "directory"  && match (relPath + "/") != null
  || match relPath != null)) src
