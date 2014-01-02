tip-toi-reveng
==============

Trying to understand the file format of Tip Toi

What we know
============

All offsets are from the beginning of the file.
 * At offset 4 is a 32bit offset of an ogg file table
 * The ogg file table consists of pairs of offsets and length, and ends with zeros
 * The ogg file is encrypted using a simple scheme, using a magic XOR value:
   - 0 is left alone
   - the XOR value is left alone
   - everything else is XORed bytewise by the value

What we have
============

OGG files with correct OGG header, Vorbis metadata, but do not play, and has “holes”. Encryption scheme incomplete?

Some OGG files have the XOR value in the header and still have a correct checksum, so the scheme above seems to be on the right track.


XOR-Values
==========

(Find with findxor.hs)

 * WWW_Bauernhof.gme: 0xAD
 * WWW_Feuerwehr.gme: 0x3B
 * Leserabe_een.gme: 0xDD

Firmware
========

Running `arm-linux-gnueabi-objdump --architecture=arm -b binary -D Update3202.upd` disassembles the ARM firmware. At 0x6db0 begins a function that *might* do the XOR decoding. Another candidate is 0x29518.

"update.upd" seems to contain code for firmware writing, the firmware itself as well as some standard apps, audio & video player libs (Ogg Vorbis, FLAC, mp3, AVI) etc.
The German words in the file (Apfel, Auto, Baum, Bein, Berg, Birne, ...) indicate that this file is already customized for Tip Toi.

The filesystem contains references drive to "A:" (eg "A:/SYSTEM", "A:/Product log file.bin", "A:/Firmware log file.bin"), "B:" (eg "B:/", "B:/App_Demo.bin") and "W:" (eg. "W:/codepage.bin", "W:/ImageRes.bin")

The Flash ICs are accessed via an MTD subsystem ("MtdLib").

"Update3202.upd" seems to be a Ravensburger specific update which is in parts identical to "update.upd" but includes for example a calendar.


Books
=====

Code fragments which look like written in C can be found at the end of various GME files.
Here is an example from the book "Weltatlas":

AnswerIndex=%d.
right oid founded:%d. total right oid:%d.
pGame->SuccessPercentage:%d.
pGame->CurQuestionNum=%d.
A:/game8.bin
Play times =%d, Voicenum=%d, addr =0x%X, len = %d, pGame = 0x%X.
Abnormal termination   Arithmetic exception:  Illegal instruction 
Interrupt received     Illegal address        Termination request Stack 
overflow         Redirect: can't open:  Out of heap memory User-defined 
signal 1  User-defined signal 2  Pure virtual fn called C++ library  exception


Summary
=======

All this indicates that the pen is a sophisticated Embedded Linux system with abilities far beyond a simple "read OID & play Ogg file" function!
