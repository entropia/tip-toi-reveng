A small test suite for tttool

The script `download.sh` downloads a few GME files into the directory
`downloaded/`

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
