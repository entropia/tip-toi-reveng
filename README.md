tip-toi-reveng
==============

Trying to understand the file format of Tip Toi

What we know
------------

The file format consists of these parts
 * A header
 * A block containing the *play scripts*
 * Some unknown data (possibly games)
 * A block containing the audio files, in WAV or OGG format
 * A checksum

All offsets are always relative to the start of the file. If we say something is an 8-bit number followed by a zero byte, it might of course also be a 16-bit number, and we just did not see high values yet.

All values are stored in little-endian format; offsets in this document are hexadecimal numbers, printed in the usual (big-endian) format. So if you see `d5b2 0100` in the hexdump, it is an offset to position `0x1B2D5`.

The header
----------

The header begins with these 8 32-bit numbers, listed with their offset:
 * `0x0000`: The offset to the *play script table*
 * `0x0004`: The offset to the *media file table*
 * `0x0008`: Unknown. Commonly `0x238b`. If you change this value in a working game file it will no longer be accepted. Maybe some kind of header checksum?
 * `0x000C`: The offset to an *aditional script table*
 * `0x0010`: The offset to the *game table*
 * `0x0014`: Product id code (== OID code of the power on symbol on page 1)
 * `0x0018`: Pointer to register init values. 16bit counter followed by n*16bit values. First value is register $0, followed by $1 and so on
 * `0x001C`: Unknown, tests revealed that this value probably has something to do with the XOR value of the media file table  
 * Next (at `0x0020`), is a variable length string, consisting of its length (8-bits), and that many characters. Commonly `CHOMPTECH DATA FORMAT CopyRight 2009 Ver2.4.031`
 * Next is a 8-byte long date (`20111024`). For some books the date contains a language string, e.g `20111024GERMAN` or `20111002DUTCH`.
 * 0x0071: pointer to the power-on sound (played, when the book is recognized. If 0, no sound is played.)  This pointer leads to a 16 bit counter with the value 1 followed by one 32bit pointer to a media list (16bit count, n*16bit media number)

The rest of the header is dubious, and contains a few more 32-bit numbers.


The play scripts
----------------

At the position referenced by `0x0000` (commonly `0x0200`), is the play script table. It constists of
 * a 32 bit number of the largest oid (max_oid) in this book, following by
 * a 32 bit number of the smallest oid (min_oid) in this book
 * (max_oid - min_oid + 1) 32bit offsets. Each of them either is `0xFFFF FFFF` or is an offset to a *play script*. They corresond to the OID code: The first OID code used corresponds to the first *play script*, and so on. `0xFFFF FFFF` means that this OID is disabled.


A play script contains of another table, which points to one or more *script lines*. A script line consists of a list of *conditional*, a list of *actions*, and a list of *media file indices*. Table of a play scitpt is simply a 16-bit number followed by that many offsets.

A script line has the format  `aaaa  conditionals... bbbb  actions... cccc media...` where
 * `a` is the number of conditionals,
   - Each conditional is 8 bytes long.
 * `b` is the number of actions,
   - Each action is 7 bytes long.
 * `c` is the number of media table indices
   - The media table indices are 16-bit numbers.

The conditionals are of the format `t1 aaaa cccc t2 bbbb`
 * `t1` & `t2` (uint8) type of `a` and `b` resp. (0 == register, 1 == value)
 * `a` & `b` (uint16) value or id of register
 * `c` (uint16) is the compare operator

Known Compare Operators are:
 * `FFF9`  equal == (written `$r==m?` in decode's output): Only continue with this line if register `$r` has value `m`.
 * `FFFB`  lesser < (written `$r<m?` in decode's output): Only continue with this line if register `$r` has a value lower than `m`.
 * `FFFD`  greater or equal >= (written `$r>=m?` in decode's output): Only continue with this line if register `$r` has a value greater or equal `m`.
 * `FFFF` not equal !=  (written `$r!=m?` in decode's output): Only continue with this line if register `$r` has not value `m`.

Currently unknown Compare Operators are `FFFA` & `FFFE`.

The actions are of the format `rrrr cccc tt mmmm`
 * `r` (uint16) id of register
 * `c` (uint16) is the command
 * `t` (uint8) type of `m` (0 == register, 1 == value)
 * `m` (uint16) value or id of register

Known Commands are:
 * `FFF9` (written `$r:=m`): Set register `$r` to `m` or value of `$m`
 * `FFF0` (written `$r+=m`): Increment register `$r` by `m` or value of `$m`
 * `FFE8` (written `P(m)`): Play audio referenced by the `m`th entry in the indices list.
 * `FC00` (written `P(b-a)`): Play a random sample from that inclusive range. `a` := lowbyte(`m`), `b` := highbyte(`m`)
 * `FD00` (written `G(n)`): Begin game `n`. `n` := highbyte(`m`.
 * `FAFF` (written `C`): Cancel game mode.

Currently unknown Commands are `F8FF`, `FB00`, `FE00`, `FF00`, `FFE0`, `FFE1`, `FFF1`, `FFF3` & `FFF4`.

The commands `P`, `G` and `C` seem to ignore their registers, `C` also its parameter (which always is `FFFF`)

There are probably 256 registers. A register can hold 16bit values.

The audio file table
--------------------

The audio file table consists of pairs of offsets and length (both 16-bit), and ends at the position of the first entry. (No explicit length found).

The audio files themselves are encrypted using a simple scheme, using a magic XOR value (`x`):
   - The values `0x00`, `0xFF`, `x` and `x XOR 0xFF` are left alone
   - Everything else is XORed bytewise by x.

The magic XOR value can be found by finding the number which makes the first 4 bytes of the first media file read `OggS` or `RIFF`.

In `Leserabe_een.gme*`, the audio table is repeated right after itself. Why?

Additional script table
-----------------------

Used only in 'Puzzle Ponyhof' and 'Reise durch die Jahreszeiten' and their FR,
NL, IT Version. The format is that of a play script (see above). In most files,
it is an empty play script, i.e. simply `0x0000` . In 'Reise durch die
Jahreszeiten' it is a playscript with empty script lines.

TODO: When is this executed?


The checksum
------------

The last 4 bytes of the file are a simple additive check-sum over the file, which is not checked by the pen.


Open issues
-----------

 * What are all the header fields?
 * What is the meaning of the bytes before the OGG file table? Some ideas in Table-Notes.md.

Links
-----

 * Discussion about Tip-Toi: http://www.quadrierer.de/geekythinking/blog/?itemid=368
 * Discussion about Tip-Toi and the related TING pen: http://www.mikrocontroller.net/topic/214479

Tools
-----

Use the tool `decode.hs` to investigate the gme files. It supports various subcommands:

	Usage:
	decode info <file.gme>...
	       general information
	decode media [-d dir] <file.gme>...
	       dumps all audio samples to the given directory (default: media/)
	decode scripts <file.gme>...
	       prints the decoded scripts for each OID
	decode script <file.gme> <n>
	       prints the decoded scripts for the given OID
	decode raw-scripts <file.gme>...
	       prints the scripts for each OID, in their raw form
        decode raw-script <file.gme> <n>
               prints the scripts for the given OID, in their raw form
	decode lint <file.gme>
	       checks for errors in the file or in this program
	decode segments <file.gme>...
	       lists all known parts of the file, with description.
	decode segment <file.gme> <pos>
	       which segment contains the given position.
	decode holes <file.gme>...
	       lists all unknown parts of the file.
        decode play <file.gme>
               interactively play: Enter OIDs, and see what happens.
        decode rewrite <infile.gme> <outfile.gme>
               parses the file and serializes it again (for debugging).

It is a Haskell program without dependencies and can be easily compiled on all platforms, using these steps:

        apt-get install haskell-platform # Or download from http://www.haskell.org/platform/
        cabal update
        ghc --make -O2 decode.hs
        ./decode foo.gme

Hardware
--------
(as found in Tiptoi pen sold in December 2013 in Germany)


OID 1.5 sensor module, 0 - 35° reading angle, 8-bit ADC, 7,000 Lux:
Sonix SNM9S102C2000B (not yet confirmed by IC marking)

OID 1.5 image decoder, LQFP48, crystal/RC, LVD & LDO built in, 2-wire interface V2 data output:
Sonix SN9P601FG-301

Unknown Chomptech (?) IC:
ZC90B

2 Kbit (256 x 8 bit) SERIAL EEPROM:
Shanghai Fudan Microelectronics FM24C02B

Flash, NAND 16 GBit (2 GBit x 8):
Hynix H27UAG8T2BTR

Main processor (?), 64-pin LQFP, probably an ASSP provided by Anyka (based on AK10XXL with ARM926EJ-S, 200 MHz, no JTAG, UART):
Chomptech ZC3202N

Quartz:
12.000 MHz

1.0 Watt Audio power Amplifier:
8891UL


OID 1.5 Spec
------------

Number of codes:	15000
Pattern Size:		1.0 x 1.0 mm²
Visual Interference:	2.8%
Error Rate:		< 0.5%



Firmware
--------

Running `arm-linux-gnueabi-objdump --architecture=arm -b binary -D Update3202.upd` disassembles the ARM firmware. At 0x6db0 begins a function that *might* do the XOR decoding. Another candidate is 0x29518.

Running `strings Update3202.updf` shows a string `pMeGame->VoiceIndexForOID[Offset][PlayIndex]=%d.`, probably a debug string. Is maybe `pMeGame` a pointer to the `.gme` file, which contains a table from OID to sound or state machine? This might be something someone who can read assembly can find out.

**"update.upd"** seems to contain code for firmware writing, the firmware itself as well as some standard apps, audio & video player libs (Ogg Vorbis, FLAC, mp3, AVI) etc.
The German words in the file (Apfel, Auto, Baum, Bein, Berg, Birne, ...) indicate that this file is already customized for Tiptoi.
0x1C2AC to 0x1C2FA contain a MSWIN4.1 BIOS Parameter Block (BPB, http://thestarman.narod.ru/asm/mbr/MSWIN41.htm) with MSB first (= big-endian).
From 0x1fae00 onwards, almost only audio data in RIFF/WAVE PCM format is contained. The exact format (eg sample rate) differs between the files.

**"Update3202.upd"** seems to be a Ravensburger specific update which is in parts identical to "update.upd" but includes for example a calendar and other languages.
From 0x47c600 onwards, almost only audio data in RIFF/WAVE PCM format is contained. The exact format (eg sample rate) differs between the files.

The firmware might be based on Anyka's Spotlight10 BaseLine and Media Development Kit which uses ARM Development Suite (ADS) Version 1.2 for AK10 MCUs.

The filesystem contains references drive to "A:" (eg "A:/SYSTEM", "A:/Product log file.bin", "A:/Firmware log file.bin"), "B:" (eg "B:/", "B:/App_Demo.bin") and "W:" (eg. "W:/codepage.bin", "W:/ImageRes.bin").

The Flash ICs are accessed via an MTD subsystem ("MtdLib").

Though other products by Chomptech are based on Linux, the Tiptoi reading pen is probably not (OS yet to be identified. Maybe Windows CE?)


Books
-----

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
-------

All this indicates that the pen is a sophisticated embedded system with abilities far beyond a simple "read OID & play Ogg file" function!
