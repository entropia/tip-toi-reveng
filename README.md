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


XOR-Values:
===========

(Find with findxor.hs)

 * WWW_Bauernhof.gme: 0xAD
 * WWW_Feuerwehr.gme: 0x3B
 * Leserabe_een.gme: 0xDD
