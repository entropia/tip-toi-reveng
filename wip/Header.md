The first few KBytes
====================

Rough notes from reading the hexdump of the stuff before the ogg files.

Header:
-------

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
 * directly after that there is a date string like `20111024` followed by zero bytes up to `0x005f`. For some books the date contains a language string, e.g `20111024GERMAN` or `20111002DUTCH`
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

Jump table locations
--------------------

Where are the others referenced from?

For example there is a (very small, one line with one **F** command) jump-table at `0x35C0` in `WWW_Bauernhof.gme`, but that offset does not occur in the file. Maybe just some dead code.
