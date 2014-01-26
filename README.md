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

If you want to learn more, here are some relevant links:
 * Discussion about Tip-Toi: http://www.quadrierer.de/geekythinking/blog/?itemid=368
 * Discussion about Tip-Toi and the related TING pen, including discussion on how to print your own books: http://www.mikrocontroller.net/topic/214479

The decode tool
---------------

Use the tool `decode.hs` to investigate the gme files and build new ones. It
supports various subcommands:

	Usage: decode [options] command

	Options:
	    -t <transcriptfile>
	       replaces media file indices by a transscript

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
	       lists all parts of the file, with description and hexdum and hexdumpp.
	    play <file.gme>
	       interactively play: Enter OIDs, and see what happens.
	    rewrite <infile.gme> <outfile.gme>
	       parses the file and serializes it again (for debugging).
	    create-debug <outfile.gme> <productid>
	       creates a special Debug.gme file for that productid
	    export <infile.gme> [<outfile.yaml>]
	       dumps the file in the human-readable yaml format
	    assemble <infile.yaml> <outfile.gme>
	       creates a gme file from the given source

A transscrpit is simply a `;`-separated table of OIDs and some text, see for example [`transcript/WWW_Bauernhof.csv`](transcript/WWW_Bauernhof.csv).


Installation
------------

This program is written in Haskell an can be installed on Windows, MacOS or Linux.

 1. First install the *Haskell platform*, see http://www.haskell.org/platform/
    for details for your system. Users of Debian or Ubuntu simply run `apt-get
    install haskell-platform`.

 2. Install the dependencies. The Haskell platform comes with a tool called
    `cabal`, and you should run the two commands

        cabal update
        cabal install yaml

 3. Now you can build the program using

        make

 4. At this point, `decode` should be ready to go. If you run

        ./decode

    you should see the list of commands shown above.

If you have any problems, you can [report an issue via GitHub](https://github.com/entropia/tip-toi-reveng/issues).

Building your own gme files
---------------------------

Once you have installed `decode`, you can create your own `.gme` files. The
process is as follows

 1. Record the audio samples you want to include, as Ogg Vorbils files, mono, 22050Hz. I use

       arecord -r 22050 foo.wav
       oggenc foo.wav
       rm foo.wav

 2. Write a `my-book.yaml` file containing some general information, and especially
    the scripts (i.e. what to do) for each OIDs (i.e. the various fields of a
    book). You can use the [example.yaml](example.yaml) file as a starting
    point; it contains more information in its comments.

 3. Run `./decode assemble my-book.yaml`, and make sure it reports no error, i.e.
    finishes silently.

 4. Copy the now generated `my-book.gme` to your TipToi pen and enjoy!

If you need to find out what OID code corresponds to what part of the book, you
can generate a debug gme file using `./decode create-debug` and load that on your pen. It will then read out the codes, as a sequence of english digits.

Again, please let us know if you have problems, but also tell us what fun thigs
you did if you succeded.


TODO
----

 * What are all the header fields? (See [wip/Header.md](wip/Header.md))
 * Finish decoding the games. (See [wip/games.txt](wip/games.txt))
 * What is the additional script table for.

Other resources in this repository
----------------------------------

 * [`oid-decoder.html`](http://htmlpreview.github.io/?https://github.com/entropia/tip-toi-reveng/blob/master/oid-decoder.html) allows you to manually decode an OID image.
 * `scripts/updates.sh` downloads all GME files from the Ravensburger server.
 * `gameanalyse.c` and `libtiptoi.c` is an alternative tool to investigate GME
   files. It can also [replace audio files in GME files](Audio/README.md);
   compile and run it for diagnostic output,
 * `Audio/` contains some audio files, such as digits read out.
 * `docs/` collects information about TipToi found elsewhere.
 * `matlab/` contains scripts to analysie GME files in Matlab
 * `wip/` contains notes about the parts of the GME files that we have not
   fully understood.


