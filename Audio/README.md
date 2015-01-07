Replacing Audio Files
=====================

This small howto explains how you can replace the audio files in a gme file.


Replace audios with numbers
---------------------------

There is a sound package you can use to debug the tiptoi.
If you apply this sound package to a gme file the tiptoi will output the ID (position) of the audio file in the audio file table.

Procedure:
 * Compile libtiptoi.c with you favorite C compiler (only MS Visual Studio express 2013 tested so far)
 * obtain an audio package (for example numbers, see below)
 * run libtiptoi with the path to the audio package, an output filename and input filename as arguments

Audio packages
 * **numbers**
   numbers in english where each digit of the ID will be spoken.
 
   To build this audio package, make sure `flac` and `oggenc` are installed,
   and run `mknumbers.sh` to build OGG audio files in `numbers/`, based on the files in `numbers/source/`.

   See `numbers/source/LICENSE.md` for license information.

For now this is only tested with Bauernhof and will not work if there is data after the audiotable!
	
	example:
	libtiptoi n ./numbers Bauernhof_numbers.gme Bauernhof.gme


Extract Audio Files
-------------------

To extract the audio files of a gme and write a filelist.txt file use the x mode with a path to extract and an input file.
The output path must exist!

	example:
	libtiptoi x ./bauernhof Bauernhof.gme
   
Afterwards you will find all audio files with their ID as name in that path together with a filelist.txt 
As there might be multiple entries in the audio table to the same audio file the file filelist might contain duplicates.


Replace Audio Files
-------------------

You can replace all audio files in a gme by using the mode r. You have to give the path to the filelist.txt, an input and an output file as parameter.

	example:
	libtiptoi r  ./pathto/filelist.txt Bauernhof_new.gme Bauernhof.gme

