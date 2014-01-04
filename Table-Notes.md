The first few KBytes
====================

Rough notes from reading the hexdump of the stuff before the ogg file.

Consider `WWW_Feuerwehr.gme`. Everything up to `00026C59` is unknown.

Tables
------

The first 32-bit-word is an offset into the file.

At that position, there is a sequence of of more offsets (32-bits). I call this the *main table*.

For most files, the main table consists of
 * At first, two 32-bit numbers that do not seem to be offsets. (What are they?)
 * Then, 32-bit offsets that point to (what I call) *jump tables* (see below).
 * In between these offsets, there are streaks of 0xFFFFFFFF.

Unclear: When does the main table end?

Jump table pattern
------------------

Thre are many jump tables, and they seem to be important. some are referneced from the main table, but not all. (where the others referenced from?)

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

The first command is always command **F1** or **F2**. There is at most one **F2** or **G** command per line. **F2** or **G** gives the number of commands, but what if **F2** is not the first command? Then it follows one **F1** with non-zero `x`.

Commands are terminated by either `0x00`, or a **A**, **B**, or **C** command with a non-empty argument list. Before the terminating command, such commands to *not* occur. **C** only occurs in the last position.

This list of commands is exhaustive, but may nevertheless be wrong:
 * **A**: Command `E8FF01 mmmm nnnn xs...`, where `m` and `n` are 16-bit numbers, and `xs` a sequence of `n` 16-bit numbers. These 16-bit numbers are media indicies. The `m` number seems to play that file.
 * **C**: Command `FFFA01 FFFF nnnn xs` has the same format, with always `m = FFFF`
 * **B**: Command `00FC01 aa bb nnnn xs`: Here `a` and `b` are 8-bit-values. This seems to be playing things in an alternating order.
 has the same format.
 * **D**: Command `00FD01 nn 0000`
 * **E**: Command `F0FF01` is followed by four more bytes (so far only `0100 0000`).
 * Command `F9FF01` comes in two variants of differing lengths:
   - **F1**: `F9 FF01 nnnn 00xx` where `n` is a 16-bit number, and `xx` one byte
     If this is the first command in the line, it is followed by `0x00`, otherwise not. (huh?)
   - **F2**: `F9 FF01 nnnn yy 00 bb 00` where `n` is a 16-bit numbers, and `y` is not zero, and `a` is an 8-bit-number. In that case, `y` indicates the number of following commands in this line.
 * **G**: Command `FB FF01 aaaa bbbb cccc`. Seems to be simliar to **F2**, as `b` is the number of commands following.

Is maybe **A** actually `0000 E8FF01`, and without `nnnn xs`? There is always `0000` in front... The only commands that do not end with `0000` are:
 * **G**. Is always followed by **E**.
 * **F1** with non-zero `x`. Is followed by **F2**, **G**
 * **F2** with non-zero `b`. Is followed by **E** or **F1**

