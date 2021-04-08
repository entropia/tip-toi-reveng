Building from source
--------------------

 1. If you have not done so yet, fetch the source code and change to the
    directory containing the code:

        git clone https://github.com/entropia/tip-toi-reveng.git tttool
        cd tttool

 2. Install the `nix` tool, if you do not have it yet:

        bash <(curl -L https://nixos.org/nix/install)

 3. Install the Cachix tool, and enable the tttool cache.

        nix-env -iA cachix -f https://cachix.org/api/v1/install
        cachix use tttool

    (This step is optional, but will save a lot of time.)

 4. Build `tttool`:

        nix-build -A linux-exe

 5. Copy the resulting program to the current directory:

        cp result/bin/tttool .

 4. At this point, `tttool` should be ready to go. If you run

        ./tttool

    you should see the list of commands shown above.

If you have any problems, you can [report an issue via GitHub](https://github.com/entropia/tip-toi-reveng/issues).

Making a release
----------------

1. Ensure that the version number is up-to-date in:
   `Changelog.md`, `tttool.cabal`, `book/conf.py`
2. Push to CI so that the OSX binaries are built and uploaded to the Cachix
   cache.
3. Run

       nix-build -A release-zip

4. Upload `result/tttool-n.m.zip`.
