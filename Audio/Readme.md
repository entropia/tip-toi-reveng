Replacing Audio Files
==============

This small howto explains how you can replace the audiofiles in a gme file.


Replace audios with numbers
---------------------------

There is a soundpackage you can use to debug the tiptoy.
If you apply this soundpackage to a gme file the tiptoy will output the ID (position) of the audio file in the audio file table.

Audio package
numbers - numbers in english where each digit of the ID will be spoken
 
	The "Blop Sound" at the start of the samples is (CC) by Mark DiAngelo
	http://soundbible.com/2008-0-9-Male-Vocalized.html
	http://creativecommons.org/licenses/by/3.0/legalcode

	The "0-9 Male Vocalized" sounds are (CC) by Mike Koenig
	and have been splitted and merged by me
	http://soundbible.com/2067-Blop.html
	http://creativecommons.org/licenses/by/3.0/legalcode

	Booth sounds have been converted from wav to ogg

procedure:
 * Compile libtiptoy.c with you favorite C-Compiler (only MS Visual Studio express 2013 tested so far)
 * download  an audio package (for example numbers)
 * run libtiptoy with the path to the audiopackage, an output filename and inputfilename as arguments

For now this is only tested with Bauernhof and will not work if there is data after the audiotable!
	
	example:
	libtiptoi n  ./numbers Bauernhof_numbers.gme Bauernhof.gme


Extract Audio Files
-----------------

To extract the Audiofiles of a gme and write a filelist.txt file use the x mode with a path to extract and an inputfile.
The outputpath must exist!

	example:
	libtiptoi x  ./bauernhof Bauernhof.gme
   
Afterwards you will find all Audiofiles with their id as name in that path with a filelist.txt 
As there might be multible entries in the Audio Table to the same Audiofile the file filelist might contain duplicates.


Replace Audio Files
-----------------

You can replace all audiofiles in a gme with the mode r. You have to give the path to the filelist.txt, an input and an outputfile as parameter.

	example:
	libtiptoi r  ./pathto/filelist.txt Bauernhof_new.gme Bauernhof.gme

