Replacing Audio Files
==============

This small howto explains how you can replace the audiofiles in a gme file.


Replace audios with numbers
---------------------------

there are 3 soundpackages, compressed by 7-zip, you can use to debug the tiptoy.
If you apply one of these soundpackages to a gme file the tiptoy will output the ID (position) of the audio file in the audio file table.

packages:
 * numbers - numbers in english where each digit of the ID will be spoken
 * zahlen - outputs the IDs in german
 * hex - outputs the IDs in german hex values

procedure:
   1) Compile libtiptoy.c with you favorite C-Compiler (only MS Visual Studio express 2013 tested so far)
   2) download and extract an audio package (for example numbers.7z)
   3) run libtiptoy with the path to the audiopackage, an output filename and inputfilename as arguments
example:
   libtiptoi n  ./numbers Bauernhof_numbers.gme Bauernhof.gme

For now this is only tested with Bauernhof and will not work if there is data after the audiotable!

Extract Audio Files
-----------------

To extract the oudiofiles of a gme and write a filelist.txt file use the x mode with a path to extract and an inputfile.
The outputpath must exist!
example:
   libtiptoi x  ./bauernhof Bauernhof.gme
   
Afterwards you will find all Auudiofiles with their id as name in that path with a filelist.txt 
As there might be multible entries in the Audio Table to the same Audiofile the file filelist might contain duplicates.


Replace Audio Files
-----------------

You can replace all audiofiles in a gme with the mode r. You have to give the path to the filelist.txt an input and an outputfile as parameter.

exaple:
   libtiptoi r  ./pathto/filelist.txt Bauernhof_new.gme Bauernhof.gme

