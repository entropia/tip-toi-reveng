The first few KBytes
====================

Rough notes from reading the hexdump of the stuff before the ogg files.

Tables
------

The first 32-bit-word is an offset into the file.

At that position, there is a sequence of of more offsets (32-bits). I call this the *main table*.

Every object in the book has a 16 bit OID code. When the user points at any object the tiptoi identifies its 16 bit OID code, subtracts the first used OID code and takes the result as the offset to find the corresponding jump table. The base for this offset is the beginning of the maintable + 8 (first two 32bit words are skipped)

For most files(?), the main table consists of
 * 32bit: last used OID code
 * 32bit: first used OID code
 * Then, 32-bit offsets that point to (what I call) *jump tables* (see below).
 * At some of these offsets are `0xFFFFFFFF`. These indicate that the corresponding OID code is not used within the book.
 * These correspond linearly to the OID codes.
   E.g. WWW_Bauernhof: The first piglet has OID-code 1499, the corresponding
   jump table is at `0x766A`. This offset is the 100th entry of the main table (4 bytes for each entry). So possibly `4*(OID - 1401) = main table index + 8`. The Value 1401 in this example is taken from the second 32bit word in the main table, it represents the first used OIS code within the current book.
 * The end of the offsets can be found at (maintable + 8 + 4*(last used OID code - first used OID code).

There is more data contained at the beginning of the file:
 * `0x0000` the first 32-bit word pointer to the main table
 * `0x0004` The second 32-bit word is a pointer to the media table.
 * `0x0008` unknown 32-bit word, value always `0x000238b` 
 * `0x000c` points to a 16bit word that is alwaya `0x0000` (no exceptions found so far)
 * `0x0010` 32-bit-word (`0x0001703b`) is an offset into the file. This offset is always direcIts value is always 2 bytes after the There is a
   number (32-bit, `0x000 000c` = 12) followed by that many offsets right after
   the list. These offsets point to right after the list and spread out towards the
   next known block (the media file table), so its objects are relatively large
   (~1k). The objects themselves contains numbers and offsets.
 * `0x0014` The sixth 32-bit-word is a small number containing the product id code (== OID code of the power on symbol on page 1) 
 * `0x0018` The seventh 32-bit-word points to the data directly following the list of the welcome samples. Maybe this means something like "end of table data"?
 * `0x001c` The eighth 32-bit-word is a small number, differing between books.
 * `0x0020` is a byte that contains the lenght of the Chomptech data format version string. 
 * `0x0021` Then follows the string `CHOMPTECH DATA FORMAT CopyRight 2009 Ver2.5.0908` or similar, resembling the length given at `0x0020
 * directly after that the is a version strings like `20111024` followed by zero bytes up to `0x005f`
 * `0x0060` For some books a 32bit address pointing to a place where another pointer can be found. Otherwise 0
 * `0x0064` to `0x0070` are 0
 * `0x0071` This points to a list that contains the address of the welcome samples played when the power on symbol is pointed at.
 * `0x0075` to `0x008b` are 0
 * Beginning at `0x008c` some books have more data (otherwise 0):
   `0x008c`      `oooooooo` 32-bit offset 
   `0x0090`      `oooo` 16-bit offset?
   `0x0092`      `nnnn` 16-bit number
   `0x0094`      `oooo`
   `0x0096`      `nnnn`
   `0x0098`      `oooo`
   `0x009a`      `nnnn`
   `0x009c`      `0000`
   `0x009e`      `0000`
   `0x00a0`      `oooo`
   `0x00a2`      `nnnn`
   `0x00a4`      `0000` or `0001`
   `0x00a6`      `0000`
   `0x00a8`      `oooo`
   `0x00aa`      `nnnn`
   * from `0x00ac` to `0x01ff` all bytes are `0`


Jump table pattern
------------------

There are many jump tables. Some are referenced from the main table, but not all.

The table consists of
 * A number,  16 bit. Commonly 16 or 17, determining the number of offsets to follow.
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

Command lines comprise of three lists, headed by the number of the elements, and a unexplicable number of `0`: `aa00  conditionals... bb00  actions... cc00 media...` where
 * `a` is the number of conditionals,
   - Each conditional is prefixed with `00aa00`, and has 5 more bytes (see below)
 * `b` is the number of actions,
   - The actions have varying length, see below. Each is prefixed with `aa00`.
 * `c` is the number of media table indices
   - The media table indices are 16-bit numbers.

The conditionals are:
 * `F9FF01 mmmm` (written **S** in decode`s output)
   - Where `m` is a 8-bit or 16-bit number. Possible semantics: Check if the register mentioned in the prefix has this value.
 * `FBFF01 mmmm` (rare, written **S`**)
 * Guess: this is the same command that only differs by one bit. What could that mean? 

The actions are:
 * **A**: `E8FF01 mmmm`  where `m` is a 16-bit number (or a 8-bit-number, no large numbers found so far), the index of the media file to play.
 * **B**: `00FC01 aa bb` Here `a` and `b` are 8-bit-values. This plays a random sample from the sublist of the media table defined by `a` to `b`.
 * **C**: `FFFA01 mmmm`  where `m` is a 16-bit-number. This has only be found in mode 4 lines and seems to end the game mode.
 * **D**: `00FD01 nn`    This command activates the game `n`
 * **E**: `F0FF01 nnnn`  This command increments the register mentioned in the prefix by `n`.
 * **F**: `F9FF01 nnnn`  where `n` is a 16-bit number. This sets the register mentioned in the prefix to `n`.

The register manipulation commands **F** and **E** only occur in lines with two conditionals. They are used to store the next line that is executed when the same symbol is tipped again (and the mode has not been changed in the meantime)  

Some findings:
- If **D** occurs, then as the last entry.
- If **E** or **F** occurs, then as the first entry.
- **D** only occurs alone or preceeded by **F** .
- **C** only has been seen as the last command in mode 4 lines so far. 
- the only value for parameter **C** seen so far is `0xFFFF`

Jump table locations
--------------------

Where are the others referenced from?

For example there is a (very small, one line with one **F** command) jump-table at `0x35C0` in `WWW_Bauernhof.gme`, but that offset does not occur in the file.
