A small test suite for tttool
-----------------------------

The script `download.sh` downloads a those GME files listed in
`gme-files-test.txt` into the directory `downloaded/`. This is a subset of
Ravensburger GMEs, useful for testing.

Feel free to propose a different set of GME files to include in the test

The script `test.sh` runs a bunch of `tttool` commands on the files in `input/`
and `downloaded/`, putting the result in `output/`

The script `run.sh` invokes `test.sh` and compares that output against
`expected/`.

With `run.sh -a` the changes are accepted.

By default, they use `../tttool`. Also convenient:

    TTTOOL='cabal -v0 run tttool --' ./run.sh

to automatically rebuild.

To run the test suite with nix, run

    nix .. -A tests

If the downloaded files changed, change the `outputHash` in
`../default.nix` a bit, run the above, and then copy the new hash from the
error message into the `.nix` file.


Getting more GMEs
-----------------

To test theories or search for certain things, it might be useful to fetch all
GMEs. For that, run

     ./list-gmes.sh
     ./download.sh gme-files-all.txt all-gmes

Warning, this downloads ~6GB.
