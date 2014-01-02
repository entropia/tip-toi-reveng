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

The last entry of the ogg file table seems to be broken (offset larger than
file size). Why? Or is there a way to find the number of table entries
elsewhere? Where?

XOR-Values
----------

(Find with `findxor.hs`, or `decode.hs`)

 * WWW_Bauernhof.gme: `0xAD`
 * WWW_Feuerwehr.gme: `0x3B`
 * Leserabe_een.gme: `0xDD`

Tools
-----

 * `decode.hs`: Decodes the OGG files from the `.gme` files. To run, run

        apt-get install haskell-platform
        cabal update
        cabal install hogg
        ghc --make -O2 decode.hs
        ./decode foo.gme

Firmware
--------

Running `arm-linux-gnueabi-objdump --architecture=arm -b binary -D Update3202.upd` disassembles the ARM firmware. At 0x6db0 begins a function that *might* do the XOR decoding. Another candidate is 0x29518.

"update.upd" seems to contain code for firmware writing, the firmware itself as well as some standard apps, audio & video player libs (Ogg Vorbis, FLAC, mp3, AVI) etc.
The German words in the file (Apfel, Auto, Baum, Bein, Berg, Birne, ...) indicate that this file is already customized for Tip Toi.

The filesystem contains references drive to "A:" (eg "A:/SYSTEM", "A:/Product log file.bin", "A:/Firmware log file.bin"), "B:" (eg "B:/", "B:/App_Demo.bin") and "W:" (eg. "W:/codepage.bin", "W:/ImageRes.bin")

The Flash ICs are accessed via an MTD subsystem ("MtdLib").

"Update3202.upd" seems to be a Ravensburger specific update which is in parts identical to "update.upd" but includes for example a calendar.


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

All this indicates that the pen is a sophisticated Embedded Linux system with abilities far beyond a simple "read OID & play Ogg file" function!
