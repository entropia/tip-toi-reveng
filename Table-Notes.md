The first few KBytes
====================

Rough notes from reading the hexdump of the stuff before the ogg files.

Header:
-------

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

Jump table locations
--------------------

Where are the others referenced from?

For example there is a (very small, one line with one **F** command) jump-table at `0x35C0` in `WWW_Bauernhof.gme`, but that offset does not occur in the file. Maybe just some dead code.
