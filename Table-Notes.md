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

Command lines are of the form `tt nnnn nnnn cmds... nnnn ids..`:
 * the *tag* `tt` is either 1 or 2.
 * `n` is amost always `0000 0000`.
 * the commands are explained below, and
 * `ids` is a list of `n` 16-bit numbers, which references the media table (0-based).

The list of commands start with one of these sequences:
 * **S1**
 * **S2**
 * **S1** **S2** (only with tag 2)
 * **S1** **S3** (only with tag 2)

Their shape is
 * **S1**: `F9 FF01 nnnn 00xx` where `n` is a 16-bit number, and `x` one byte. If `x` is zero, then nothing follows, otherwise **S2** or **S3** must follow.
 * **S2**: `F9 FF01 nnnn yy 00 aa 00 pc... mmmm xs...` where
    - `n` is a 16-bit number
    - `y` is not zero
    - `a` is an 8-bit-number
    - `y` indicates the number of following play command (`pc`) which always have `0000` in between
    - `mmmm` indicates the number of media file indices in `xs`, which is a list of 16-bit-numbers.
 * **S3** is like **S2**, but starts with `FB` instead of `F9`. So maybe the byte is not really part of the command.

If we have **S1 S2**, then **S1**’s x is equal to **S2**’s a.

The first parameter of the first **S**-command is likely the mode (*Wissen*, *Entdecken*, *Spielen*, etc.).

The play comands are:
 * **A**: `E8FF01 mmmm`, where `m` is a 16-bit number (or a 8-bit-number, no large numbers found so far), the number of the media file to play.
 * **B**: `00FC01 aa bb`: Here `a` and `b` are 8-bit-values. This seems to be playing one of the samples from `a` to `b`, beginning with `a` and cycling through the list.
 * **C**: `FFFA01 FFFF`
 * **D**: `00FD01 nn`
 * **E**: `F0FF01 0100`
 * **F**: `F9FF01 nnnn` where `n` is a 16-bit number. This is very similar to `S1`.

If **D** occurs, then as the last entry. If **E** or **F** occurs, then as the first entry. **D** only occurs alone or with **F** before.

Jump table locations
--------------------

Where are the others referenced from?

For example there is a (very small, one line with one **F** command) jump-table at `0x35C0` in `WWW_Bauernhof.gme`, but that offset does not occur in the file.
