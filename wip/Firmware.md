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

