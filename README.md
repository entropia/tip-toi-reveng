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

Use the tool `tttool` to investigate the gme files and build new ones. It
supports various subcommands:

    GME creation commands:
    assemble                 creates a gme file from the given source

    OID code creation commands:

    oid-table                creates a PDF file with all codes in the yaml file
    oid-codes                creates PNG files for every OID in the yaml file.
    oid-code                 creates PNG files for each given code(s)

    GME analysis commands:
    info                     Print general information about a GME file
    export                   dumps the file in the human-readable yaml format
    scripts                  prints the decoded scripts for each OID
    script                   prints the decoded scripts for a specific OID
    games                    prints the decoded games
    lint                     checks for errors in the file or in this program
    segments                 lists all known parts of the file, with description.
    segment                  prints the decoded scripts for a specific OID
    explain                  print a hexdump of a GME file with descriptions
    holes                    lists all unknown parts of the file.
    rewrite                  parses the file and writes it again (for debugging)

    GME extraction commands:
    media                    dumps all audio samples
    binaries                 dumps all binaries

    Simulation commands:
    play                     interactively play a GME file

Run

    ./tttool --help

to learn about global options (e.g. DPI settings), and

    ./tttool command --help

for the options of the individual command.

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

If you want to convert existing audio files of almost any format, and you have
`ffmpeg` installed, you can use 

    ffmpeg -i input-audio-in-some.fmt -ar 22050 -ac 1 foo.ogg

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
 * `scripts/update.sh` downloads all gme files from the Ravensburger server (requires perl and the [XML::Simple](http://search.cpan.org/~grantm/XML-Simple/) module).  

   Instead of downloading all of them, you can conveniently browse them at <http://tiptoi.vakat.de/>, a service provided by Falko Oldenburg <tiptoi@vakat.de>.
 * `gameanalyse.c` and `libtiptoi.c` is an alternative tool to investigate gme
   files. It can also [replace audio files in gme files](Audio/README.md);
   compile and run it for diagnostic output.
 * `Audio/` contains some audio files, such as digits read out.
 * `docs/` collects information about TipToi found elsewhere.
 * `wip/` (work in progess) contains notes about the parts of the gme files that are not
   fully understood yet.
 * `perl-tools` contains a perl based script, to generate a PDF with all OID codes from a yaml-file as well some functions to generate PNG-files, inject pHYs-chunks with resolution hints into GD generated PNG files as result from some testing

