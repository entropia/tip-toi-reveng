The first few KBytes
====================

Rough notes from reading the hexdump of the stuff before the ogg files.

Tables
------

The first 32-bit-word is an offset into the file.

At that position, there is a sequence of of more offsets (32-bits). I call this the *main table*.

For most files, the main table consists of
 * At first, two 32-bit numbers that do not seem to be offsets. (What are they?)
 * Then, 32-bit offsets that point to (what I call) *jump tables* (see below).
 * In between these offsets, there are streaks of 0xFFFFFFFF.

Unclear: When does the main table end?

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

Thre are many jump tables, and they seem to be important. Some are referenced from the main table, but not all.

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

Command lines
-------------

Command lines have the form
 * `0200 0000 00` followed by commands, or
 * `0100 0000 00` followed by commands, or (precisely once so far)
 * `0100 001E 00`

The first byte seems to indicate the mode (*Wissen* or *Entdecken*). But where is *Spielen*?

There are three kind of start commands, which occur in these five combinations:
 * **S1**
 * **S2**
 * **S1** **S2**
 * **S1** **S3**

Their shape is
 * **S1**: `F9 FF01 nnnn 00xx` where `n` is a 16-bit number, and `x` one byte. If `x` is zero, then nothing follows, otherwise **S2** or **S3** must follow.
 * **S2**: `F9 FF01 nnnn yy 00 aa 00 pc... mmmm xs...` where
    - `n` is a 16-bit number
    - `y` is not zero
    - `a` is an 8-bit-number
    - `y` indicates the number of following play command (`pc`) which always have `0000` in between
    - `mmmm` indicates the number of media file indices in `xs`, which is a list of 16-bit-numbers.
 * **S3** is like **S2**, but starts with `FB` instead of `F9`.

If we have **S1 S2**, then **S1**’s x is equal to **S2**’s a.

The play comands are:
 * **A**: `E8FF01 mmmm`, where `m` is a 16-bit number, the number of the media file to play.
 * **B**: `00FC01 aa bb`: Here `a` and `b` are 8-bit-values. This seems to be playing one of the samples from `a` to `b`, beginning with `a` and cycling through the list.
 * **C**: `FFFA01 FFFF`
 * **D**: `00FD01 nn`
 * **E**: `F0FF01 0100`
 * **F**: `F9FF01 nnnn` where `n` is a 16-bit number

If **D** occurs, then as the last entry. If **E** or **F** occurs, then as the first entry. **D** only occurs alone or with **F1** before. 

Jump table locations
--------------------

Where are the others referenced from?

For example there is a (very small, one line with one **F1** command) jump-table at `0x35C0` in `WWW_Bauernhof.gme`, but that offset does not occur in the file.
