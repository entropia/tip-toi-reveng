The first few KBytes
====================

Rough notes from reading the hexdump of the stuff before the ogg file.

Consider `WWW_Feuerwehr.gme`. Everything up to `00026C59` is unknown.

Tables
------

After some header information and lots of zeros, there are obvious offset tables at `0x200`,`0x2e0`, `0x380`, `0x438`, `0x500`, `0x5c8`, `0x690`, `0x6a4`, `0x700`, `0x768`, `0x7e0`, possibly separated by streaks of `0xffff` (or are these just invalid entries?).

The beginnig of these tables (`0x0200`) is also the first byte of the file!

 * The first two entries point somewhere in the table.
 * All other entries are either `0xFFFFFFFF`, or point somewhere that follows the jump table pattern below.
 * Not all jump tables are referenced from the main table.

Jump table pattern
------------------

There is a pattern, consisting of
 * A number,  16 bit. Commonly 16 or 17.
 * That many offsets, the first one commonly pointing directly after the list

In `WWW_Feuerwehr.gme`, this pattern is for example found at `0x00025e4`
 * First two bytes `1100` (= 17)
 * Followed by 17 offsets (note the endianness):

         1. 2a26 0000
         2. 5226 0000
         3. 9526 0000
         4. b726 0000
         5. e226 0000
         6. f226 0000
         7. 0227 0000
         8. 1227 0000
         9. 2227 0000
        10. 3227 0000
        11. 4227 0000
        12. 5227 0000
        13. 6227 0000
        14. 7227 0000
        15. 8227 0000
        16. 9227 0000
        17. a227 0000

 * These are always at least 16 bytes apart, but can have odd lengths. The data pointed to is

         1. 0200 0000 00f9 ff01 0100 0005 00f9 ff01 0000 0200 0500 f0ff 0101 0000 00e8 ff01 0000 0100 2900 0000
         2. 0200 0000 00f9 ff01 0100 0005 00f9 ff01 0100 0500 0500 f9ff 0100 0000 00e8 ff01 0000 0000 e8ff 0101 0000 00e8 ff01 0200 0000 e8ff 0103 0004 002a 002b 002c 002d 0000 00
         3. 0100 0000 00f9 ff01 0200 0200 0000 e8ff 0100 0000 00e8 ff01 0100 0200 2900 2a00 0000
         4. 0100 0000 00f9 ff01 0300 0300 0000 e8ff 0100 0000 00e8 ff01 0100 0000 e8ff 0102 0003 002e 001c 0008 0000 00
         5. 0100 0000 00f9 ff01 0400 0000 0000 0000
         6. 0100 0000 00f9 ff01 0500 0000 0000 0000
         7. 0100 0000 00f9 ff01 0600 0000 0000 0000
        [..]
        16. 0100 0000 00f9 ff01 0f00 0000 0000 0000
        17. 0100 0000 00f9 ff01 1000 0000 0000 0000

  * This is likely the end, because the next two bytes (at`0x27b2`, `1000`) begin another round of this pattern.


