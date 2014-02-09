The GME file format
===================

This is the result of reverse engineering (i.e. staring at hex codes for a long time), and might not always be accurate. Also, it is not complete; see the TODO list in [README.md].


The file format consists of these parts
 * a header, with offsets into
 * the data area, which includes the *play scripts*, the games, the audio files and other stuff, and finally
 * a checksum.

All offsets are always relative to the start of the file. If we say something is an 8-bit number followed by a zero byte, it might of course also be a 16-bit number, and we just did not see high values yet.

All values are stored in little-endian format; offsets in this document are hexadecimal numbers, printed in the usual (big-endian) format. So if you see `d5b2 0100` in the hexdump, it is an offset to position `0x1B2D5`.


The header
----------

The header begins with these 8 32-bit numbers, listed with their offset:
 * `0x0000`: 32bit offset to the *play script table*
 * `0x0004`: 32bit offset to the *media file table*
 * `0x0008`: 32bit. If you change this value in a working game file it will no longer be accepted. Its value is 0x0000238b for all TipToi products seen so far.
 * `0x000C`: 32bit. The offset to an *additional script table*. Purpose unknown.
 * `0x0010`: 32bit. The offset to the *game table*
 * `0x0014`: 32bit. Product id code (== OID code of the power on symbol on page 1)
 * `0x0018`: 32bit. Pointer to register init values (16bit counter followed by n×16bit values. First value is register $0, followed by $1 and so on.)
 * `0x001C`: XOR value index. This 8-bit number is an index into a lookup table in the firmware, which contains the real XOR value to be used when decoding the media files.
 * Next (at `0x0020`), is a variable length string, consisting of its length (8-bit), and that many characters. Commonly `CHOMPTECH DATA FORMAT CopyRight 2009 Ver2.x.yyy` (varies between books)
 * Next is a 8-byte long date (`20111024`). For some books the date is directly followes by a language string, e.g `20111024GERMAN` or `20111002DUTCH`. If the language string is provided in the book it must match the language of the firmware that is running on the pen (it is unclear where is is checked; the file .tiptoi.log is NOT taken into account here!) or the pen will ignore it. If the language is missing any TipToi pen will accept the file. The date string seems optional, the only condition is that the language string must be preceded by at least one ASCII number. At the end there is sequence of zeros up to position 0x5f.
 * 0x0060: unknown, some files have a 32bit offset here.
 * 0x0071: pointer to the power-on sound (played, when the book is recognized. If 0, no sound is played). This pointer leads to a 16bit counter with the value 1 followed by one 32bit pointer to a media list (16bit count, n×16bit media number)

The rest of the header is dubious, and contains a few more 16 or 32 bit numbers.


The play scripts
----------------

At the position referenced at `0x0000` (commonly `0x0200`), is the play script table. It consists of
 * 32 bit: last used OID code
 * 32 bit: first used OID code
 * Then, 32bit offsets that point to (what we call) *play scripts* (see below). These correspond linearly to the OID codes.
   E.g. WWW_Bauernhof: The first piglet on page 6 has OID code 1499, the corresponding  jump table is at `0x766A`. This offset is the 100th entry of the main table (4 bytes for each entry). So possibly `4×(OID - 1401) = main table index + 8`. The value 1401 in this example is taken from the second 32bit word in the main table, it represents the first used OID code within the current book.
 * Some of these offsets are `0xFFFFFFFF`. This indicates that the corresponding OID code is not used within the book.
 * The end of the offsets can be found at (maintable + 8 + 4×(last used OID code - first used OID code).

A play script contains of another table (16bit number followed by that many offsets), which points to one or more *script lines*. A script line consists of a list of *conditional*, a list of *actions*, and a list of *media file indices*.

A script line has the format  `aaaa  conditionals... bbbb  actions... cccc media...` where
 * `a` is the number of conditionals,
   - Each conditional consists of 8 bytes.
 * `b` is the number of actions,
   - Each action is 7 bytes.
 * `c` is the number of media table indices
   - The media table indices are 16bit numbers.

The conditionals are of the format `t1 aaaa cccc t2 bbbb`
 * `t1` & `t2` (uint8) type of `a` and `b` resp. (0 == register, 1 == value)
 * `a` & `b` (uint16) value or id of register
 * `c` (uint16) is the comparison operator

Known comparison operators are:
 * `FFF9`  equal == (written `$r==m?` in decode's output): Only continue with this line if register `$r` has value `m`.
 * `FFFB`  lesser < (written `$r<m?` in decode's output): Only continue with this line if register `$r` has a value lower than `m`.
 * `FFFD`  greater or equal >= (written `$r>=m?` in decode's output): Only continue with this line if register `$r` has a value greater or equal `m`.
 * `FFFF` not equal !=  (written `$r!=m?` in decode's output): Only continue with this line if register `$r` has not value `m`.

Currently unknown comparison operators are `FFFA` & `FFFE`.

The actions are of the format `rrrr cccc tt mmmm`
 * `r` (uint16) id of register
 * `c` (uint16) is the command
 * `t` (uint8) type of `m` (0 == register, 1 == value)
 * `m` (uint16) value or id of register

Known commands are:
 * `FFF9` (written `$r:=m`): Set register `$r` to `m` or value of `$m`
 * `FFF0` (written `$r+=m`): Increment register `$r` by `m` or value of `$m`
 * `FFE8` (written `P(m)`): Play audio referenced by the `m`th entry in the indices list.
 * `FC00` (written `P(b-a)`): Play a random sample from that inclusive range. `a` := lowbyte(`m`), `b` := highbyte(`m`)
 * `FD00` (written `G(m)`): Begin game `m`.
 * `FAFF` (written `C`): Cancel game mode.

Currently unknown commands are `F8FF`, `FB00`, `FE00`, `FF00`, `FFE0`, `FFE1`, `FFF1`, `FFF3` & `FFF4`.

The commands `P`, `G` and `C` seem to ignore their registers, `C` also its parameter (which always is `FFFF`).

The number of registers and their intial size is set in the array referenced at `0x0018`. A register can hold 16bit values.

The audio file table
--------------------

The audio file table consists of pairs of offsets and length (both 16bit), and ends at the position of the first entry. (No explicit length found).

The audio files themselves are encrypted using a simple scheme, using a magic XOR value (`x`):
   - The values `0x00`, `0xFF`, `x` and `x XOR 0xFF` are left alone
   - Everything else is XORed bytewise by x.

The magic XOR value can be found by finding the number which makes the first 4 bytes of the first media file read `OggS` or `RIFF`.

In `Leserabe_een.gme`, the audio table is repeated right after itself. The reason is unknown, it might be a bug in Ravensburger's creation software.

Additional script table
-----------------------

Used only in 'Puzzle Ponyhof' and 'Reise durch die Jahreszeiten' and their FR,
NL, IT Version. The format is that of a play script (see above). In most files,
it is an empty play script, i.e. simply `0x0000` . In 'Reise durch die
Jahreszeiten' it is a playscript with empty script lines.

TODO: When is this executed?


The checksum
------------

The last 4 bytes of the file are a simple additive check-sum over the file, which is not checked by the pen.
