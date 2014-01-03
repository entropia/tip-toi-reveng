tip-toi-reveng
==============

Trying to understand the file format of Tip Toi

What we know
------------

All offsets are from the beginning of the file.
 * At offset 4 is a 32bit offset of an ogg file table
 * The ogg file table consists of pairs of offsets and length, and ends at the position
   of the first entry. (No explicit length found).
 * The ogg file is encrypted using a simple scheme, using a magic XOR value (`x`):
   - The values `0x00`, `0xFF`, `x` and `x XOR 0xFF` are left alone
   - Everything else is XORed bytewise by x.
 * This is verified by checking the CRC header of the OGG files

What we have
------------

Some files are successfully decoded. Known so far:
 * `WWW_Bauernhof.gme` (MD5 sum `9e4a8bfcb33346dd4dcdc888ee6e20ba`)
 * `WWW_Feuerwehr.gme` (MD5 sum `19812ec9a96e09326a27173b28d6671d`)
 * `Leserabe_een.gme` (MD5s sum `f16660fb7ee59bb4deb5390bb78f8860`), but playback is too fast

Open issues
-----------

 * What is the meaning of the bytes before the OGG file table? Some ideas in Table-Notes.md.
 * The last four bytes are likely some kind of checksum. Which checksum?

Links
-----

 * Discussion about Tip-Toi: http://www.quadrierer.de/geekythinking/blog/?itemid=368
 * Discussion about Tip-Toi and the related TING pen: http://www.mikrocontroller.net/topic/214479

Tools
-----

 * `decode.hs`: Decodes the OGG files from the `.gme` files. To run, run

        apt-get install haskell-platform
        cabal update
        cabal install hogg
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
