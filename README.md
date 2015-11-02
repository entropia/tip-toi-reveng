Das tttool hat jetzt eine deutsche Webseite für Anwender: http://tttool.entropia.de/

tip-toi-reveng
==============


The goal of this project is to understand the file and paper format for the
Ravensburger TipToi pen. The ultimate goal is that everyone can create their
own books, with their own sounds.

The current status is that we understood most of the file format (see the
[GME file format specification](GME-Format.md)). We provide a tool that allows
you to dissect these files.

The tool can also be used to generate completely new files from scratch; see
below for details.

If you want to learn more please have a look into our wiki (https://github.com/entropia/tip-toi-reveng/wiki).

The tttool tool
---------------

Use the tool `tttool.hs` to investigate the gme files and build new ones. It
supports various subcommands:

    Usage: tttool [options] command

    Options:
        -t <transcriptfile>
           in the screen output, replaces media file indices by a transscript

    Commands:
        info <file.gme>...
           general information
        media [-d dir] <file.gme>...
           dumps all audio samples to the given directory (default: media/)
        scripts <file.gme>...
           prints the decoded scripts for each OID
        script <file.gme> <n>
           prints the decoded scripts for the given OID
        raw-scripts <file.gme>...
           prints the scripts for each OID, in their raw form
        raw-script <file.gme> <n>
           prints the scripts for the given OID, in their raw form
        binaries [-d dir] <file.gme>...
           dumps all binaries to the given directory (default: binaries/)
        games <file.gme>...
           prints the decoded games
        lint <file.gme>
           checks for errors in the file or in this program
        segments <file.gme>...
           lists all known parts of the file, with description.
        segment <file.gme> <pos>
           which segment contains the given position.
        holes <file.gme>...
           lists all unknown parts of the file.
        explain <file.gme>...
           lists all parts of the file, with description and hexdump.
        play <file.gme or file.yaml>
           interactively play: Enter OIDs, and see what happens.
        rewrite <infile.gme> <outfile.gme>
           parses the file and serializes it again (for debugging).
        export <infile.gme> [<outfile.yaml>]
           dumps the file in the human-readable yaml format
        assemble <infile.yaml> <outfile.gme>
           creates a gme file from the given source
        oid-code [-d DPI] <codes>
           creates a PNG file for each given code
           scale this to 10cm×10cm
           By default, it creates a 1200 dpi image. With -d 600, you
           obtain a 600 dpi image. With -d 600d resp. 1200d you can double the
           size of the pixel.
           <codes> can be a range, e.g. 1,3,1000-1085.
           Uses oid-<code>.png as the file name.
        oid-code [-d DPI] <infile.yaml>
           Like above, but creates one file for each code in the yaml file.
           Uses oid-<product-id>-<scriptname or code>.png as the file name.
        raw-oid-code [-d DPI] <raw codes>
           creates a PNG file with the given "raw code". Usually not needed.
           Uses oid-raw-<code>.png as the file name.

A transscript is simply a `;`-separated table of OIDs and some text, see for example [`transcript/WWW_Bauernhof.csv`](transcript/WWW_Bauernhof.csv).


Installation
------------

This program is written in Haskell and can be installed on Windows, MacOS or Linux.

For Windows users, we create zipfile containing `tttool`, you can find them in
the [releases section](https://github.com/entropia/tip-toi-reveng/releases) of
the github project.

Otherwise, installation from source is not difficult either:

 1. If you have not done so yet, fetch the source code and change to the
    directory containing the code:

        git clone https://github.com/entropia/tip-toi-reveng.git tttool
        cd tttool

 2. Install the *Haskell platform*, see http://www.haskell.org/platform/
    for details for your system. Users of Debian or Ubuntu simply run

        apt-get install haskell-platform

 3. Install the development packages for ncurses, i.e.

        apt-get install libncurses5-dev

 4. Install the Haskell dependencies. The Haskell platform comes with a tool
    called `cabal`, and you should run the two commands

        cabal update
        cabal install --only-dependencies

 5. Now you can build the program using

        cabal install --bindir=.

 6. At this point, `tttool` should be ready to go. If you run

        ./tttool

    you should see the list of commands shown above.

If you have any problems, you can [report an issue via GitHub](https://github.com/entropia/tip-toi-reveng/issues).

Building your own gme files
---------------------------

Once you have installed `tttool`, you can create your own `.gme` files. The
process is as follows

 1. Record the audio samples you want to include, as Ogg Vorbis files, mono, 22050Hz. I use

        arecord -r 22050 foo.wav
        oggenc foo.wav
        rm foo.wav

 2. Write a `my-book.yaml` file containing some general information, and especially
    the scripts (i.e. what to do) for each OIDs (i.e. the various fields of a
    book). You can use the [example.yaml](example.yaml) file as a starting
    point; it contains more information in its comments.

 3. Run `./tttool assemble my-book.yaml`, and make sure it reports no error, i.e.
    finishes silently.

 4. Copy the now generated `my-book.gme` to your TipToi pen and enjoy!

If you need to find out what OID code corresponds to what part of the book, you
can generate a debug gme using the `debug.yaml` file, adjusting its
`Product-Id` to your product’s id, building it with `./tttool assemble
debug.yaml` and loading the resulting `debug.gme` on your pen.  It will then
read out the codes, as a sequence of english digits.

We are also collecting template files, where the OIDs are commented; these can
be found in the `./templates` directory. Please improve and contribute!

Again, please let us know if you have problems, but also tell us what fun
things you did if you succeded.

Text to speech
--------------

If you have `libttspico-utils` and `vorbis-tools installed`, you can have tttool
generate audio files from text for you, which makes developing your yaml file
much easier. See [text2speech.yaml](text2speech.yaml) for more information.

Printing your own books
-----------------------

With the code in this repository, you can create GME files. This is even more
fun if you can also create your own books! „Pronwan“ found out how that works,
as you can see in [this video demonstration](http://youtu.be/KC97F4PfNhk). He
also published 30 minute [video tutorial](http://youtu.be/4AjvjFM8GzM) (in
German).

Press Review
------------

 * [tiptoi hacking](https://blogs.fsfe.org/guido/2014/05/tiptoi-hacking-und-systemanforderungen/) by Guido Arnold
 * [TipToi Hacking](http://www.nerd.junetz.de/blogbox/index.php?/archives/1377-TipToi-Hacking.html) and [TipToi Hacking II](http://www.nerd.junetz.de/blogbox/index.php?/archives/1378-TipToi-Hacking-II.html) by Mr. Blog
 * [Various posts](https://www.joachim-breitner.de/blog/tag/Tiptoi) by Joachim “nomeata” Breitner (the main author of `tttool`)
 * [Self-made animal figures](https://www.youtube.com/watch?v=Yic57Y9VORA&app=desktop) demonstration video

TODO
----

 * What are all the header fields? (See [wip/Header.md](wip/Header.md))
 * Finish decoding the games. (See [wip/games.txt](wip/games.txt))
 * What is the purpose of the additional script table?

Other resources in this repository
----------------------------------

 * [`oid-decoder.html`](http://htmlpreview.github.io/?https://github.com/entropia/tip-toi-reveng/blob/master/oid-decoder.html) allows you to manually decode an OID image.
 * `scripts/updates.sh` downloads all gme files from the Ravensburger server.
 * `gameanalyse.c` and `libtiptoi.c` is an alternative tool to investigate gme
   files. It can also [replace audio files in gme files](Audio/README.md);
   compile and run it for diagnostic output.
 * `Audio/` contains some audio files, such as digits read out.
 * `docs/` collects information about TipToi found elsewhere.
 * `matlab/` contains scripts to analyse gme files in Matlab
 * `wip/` (work in progess) contains notes about the parts of the gme files that are not
   fully understood yet.
 * `perl-tools` contains a perl based script, to generate a PDF with all OID codes from a yaml-file as well some functions to generate PNG-files, inject pHYs-chunks with resolution hints into GD generated PNG files as result from some testing

