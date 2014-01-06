The first few KBytes
====================

Rough notes from reading the hexdump of the stuff before the ogg files.

Tables
------

The first 32-bit-word is an offset into the file.

At that position, there is a sequence of of more offsets (32-bits). I call this the *main table*.

Every object in the book has a 16 bit code number. When the user points at any object the tiptoi identified its 16 bit code, subtracts the first used code number and takes the result as the offset to find the corresponding jump table. The base for this offset is the beginning of the maintable + 8 (first two 32bit words)

For most files(?), the main table consists of
 * 32bit: last used code number
 * 32bit: first used code number
 * Then, 32-bit offsets that point to (what I call) *jump tables* (see below).
 * In between these offsets, there are streaks of 0xFFFFFFFF. These indicate that the corresponding code is not used within the book.
 * These correspond linearly to the OID codes.
   E.g. WWW_Bauernhof: The first piglet has OID-code 1499, the corresponding
   jump table is at `0x766A`. This offset is the 400ths entry of the main table. So possibly `OID - 1099 = main table index`. Question: Is this 1099 the same for every book?
 * The end of the offsets can be found at (maintable + 8 + 4*(last used code - first used code). Question: What is the first and last code used?

There is more data contained at the beginning of the file:
 * The second 32-bit-words is a pointer to the media table.
 * The forth 32-bit-word (`0x0001703b`) is an offset into the file. There is a
   number (32-bit, `0x000 000c` = 12) followed by that many offsets right after
   the list. These offsets begin are right the list and spread out towards the
   next known block (the media file table), so its objects are relatively large
   (~1k). The objects themselves contains numbers and offsets.
 * The fifth 32-bit-word is the same offset as the forth in WWW_Bauernhof.gme,
   but different in WWW_Feuerwehr.gme. There, the offset is `0x103ff`, which
   points in the middle of some jump table commands
 * The sixth 32-bit-word seems to be a small number.
 * The seventh 32-bit-word is a large number, but points in the middle of the
   media file area, so probably not an offset.
 * The eigth 32-bit-word is a small number.
 * Then follows the string `0CHOMPTECH DATA FORMAT CopyRight 2009 Ver2.5.090820111024`
   - The Zero in front means 0x30 Byte long string so it ends after 0908 then the date 'YYYYMMDD' of the file follows
   - see Wimmelbuch.gme for example there it is '1CHOMPTECH .... Ver2.10.090120130419'
 * there is another number at `0x70` or `0x71`. If read from `0x71` on, it is similar to the seventh 32-bit-word of the header.

Jump table pattern
------------------

There are many jump tables, and they seem to be important. Some are referenced from the main table, but not all.

The table consists of
 * A number,  16 bit. Commonly 16 or 17.
 * That many offsets, the first one commonly pointing directly after the list. These point to what I call *command lines*.

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

 * These point to these *command lines*:

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

  * This is likely the end, because the next two bytes (at`0x27b2`, `1000`) begin another round of this pattern. But in general it is not clear how the command lines are terminated.
  * This table probably defines the pages in the book, setting defaults (unverified)

Command lines
-------------

Command lines come in three variants
 * `02 0000 0000 F9FF01 mmmm 00 gg F9FF 01 aa00 bb00 gg00 pc... mmmm xs...` where
    - `mmmm` indicates the mode (*Wissen*, *Entdecken*, etc.)
    - `gg` is some number (a group of kinds), equal in both positions
    - `aa` tends to count within the lines of one mode and table, nothing more known so far
    - `bb` is the number of play commands in `pc...`
    - `pc` is a sequence of `bb` play commands, which always have `0000` in between.
    - `mmmm` is the number of media file indices in `xs...`, a list of 16-bit-numbers
    - Sometimes, byte 12 is `FB` instead of `F9`
 * `01 0000 0000 F9FF01 mmmm bb00 pad pc... mmmm xs...` where
    - `mmmm` indicates the mode (*Wissen*, *Entdecken*, etc.)
    - `bb` is the number of play commands in `pc...`
    - `pad` is empty if `bb=0`. Otherwise, it is usually, but not always `0000`.
    - `pc` is a sequence of `bb` play commands, which always have `0000` in between.
    - `mmmm` is the number of media file indices in `xs...`, a list of 16-bit-numbers
    - In one instance, byte 3 is `1E` instead of `00`


The first variant is used if there is more than one line for a mode within a
table, so I call this a **ML** line, while the second variant is used if there is only one line, so I call this a **SL**. The variants are written **ML'** and **SL'**.

I may seem strange to have the pad in **SL** depend on `b` without seeing
something similar in **ML**. But we simply have no example of a **ML** with
`b=0` yet... quite possibly the same mechanism works there.

The play comands are:
 * **A**: `E8FF01 mmmm`, where `m` is a 16-bit number (or a 8-bit-number, no large numbers found so far), the number of the media file to play.
 * **B**: `00FC01 aa bb`: Here `a` and `b` are 8-bit-values. This seems to be playing one of the samples from `a` to `b`, beginning with `a` and cycling through the list.
 * **C**: `FFFA01 FFFF`
 * **D**: `00FD01 nn`
 * **E**: `F0FF01 0100`
   - When activating this symbol again, execute the next line of this mode.
 * **F**: `F9FF01 nnnn` where `n` is a 16-bit number.
   - `n = 0`: When activating this symbol again, execute the first line of this mode.
   - `n ≠ 0`: Change the mode to `n`

The jump commands **F** and **E** only occur in **ML** lines.

If **D** occurs, then as the last entry. If **E** or **F** occurs, then as the first entry. **D** only occurs alone or with **F** before.

Jump table locations
--------------------

Where are the others referenced from?

For example there is a (very small, one line with one **F** command) jump-table at `0x35C0` in `WWW_Bauernhof.gme`, but that offset does not occur in the file.
