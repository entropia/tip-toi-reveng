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
 * `0x0008`: 32bit. If you change this value in a working game file it will no longer be accepted. Its value is 0x0000238b for all tiptoi products seen so far. Maybe this is the Ravensburger customer number at Chomptech. 
 * `0x000C`: 32bit. The offset to an *additional script table*. Purpose unknown.
 * `0x0010`: 32bit. The offset to the *game table*
 * `0x0014`: 32bit. Product id code (== OID code of the power on symbol on page 1)
 * `0x0018`: 32bit. Pointer to register init values (16bit counter followed by n×16bit values. First value is register $0, followed by $1 and so on.)
 * `0x001C`: raw XOR value (8bit), see below at media table explanation. 
 * `0x001D`: three bytes with unknown meaning, 0 for all products seen so far. 
 * `0x0020`: a variable length string, consisting of its length (8bit), and that many characters. Commonly `CHOMPTECH DATA FORMAT CopyRight 2009 Ver2.x.yyy` (varies between products)
 * Next is a 8 date string (`20111024`). The date string seems optional with one condition: if a language string follows the date must consist of at least one ASCII number.
 * Next is an optional language string (currently known: `GERMAN`, `DUTCH`, `FRENCH`, `ITALIAN`. If the language string is provided it must match the language of the firmware that is running on the pen (it is unclear where is is checked; the file .tiptoi.log is NOT taken into account here!) or the pen will ignore it. If the language is missing any TipToi pen will accept the file. 
 * Next there is sequence of zeros up to and including to position 0x5f.
 * `0x0060`: 32bit offset, purpose unknown. Some products have 0 here.
 * `0x0071`: 32bit offset to the playlistlist for the the power-on sound (played, when the product is recognized. If 0, no sound is played.) 

The rest of the header is dubious, and contains a few more 16 or 32 bit numbers.


The script table
----------------

At the position referenced at `0x0000` (commonly `0x0200`), is the script table. It consists of
 * 32 bit: last used OID code
 * 32 bit: first used OID code
 * Then, 32bit offsets that point to *play scripts* (see below). These correspond linearly to the OID codes.
   E.g. WWW_Bauernhof: The first piglet on page 6 has OID code 1499, the corresponding play script is located at `0x766A`. The 100th 32bit word in the script table contains that offset. So `4×(OID - 1401) = script table index + 8`. The value 1401 in this example is the first used OID code in the product (second 32bit word in the script table).
 * Some of these offsets are `0xFFFFFFFF`. This indicates that the corresponding OID code is not used within the product.
 * The end of the offsets can be found at (script table + 8 + 4×(last used OID code - first used OID code).


The play script
---------------

A play script consists of a 16bit number (defining the length of the list to follow) and a list of 32bit offsets. The offsets point to *script lines*.

The script line
---------------

A script line consists of a list of *conditionals*, a list of *actions*, and a so called *playlist* consisting *media file table* indices.

A script line has the format  `aaaa  conditionals... bbbb  actions... cccc media...` where
 * `a` is the number of conditionals. Each conditional consists of 8 bytes.
 * `b` is the number of actions. Each action is 7 bytes.
 * `c` is a playlist

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

The playlist
------------

A playlist consists of a 16bit number (defining the length of the list to follow) and a list of 16bit indices to the media file table.  Because the "play random" command only knows 8bit values for the range definition the size of a playlist might be limited to 256 indices.


The playlistlist
----------------

A list that references playlists. A 16bit value followed by that many 32bit offsets to playlists.


The register init values
------------------------

This is a 16bit value defining the number of registers used in the product followed by that many 16bit values. The first value is the init value for register $0, followed by $1 and so on
Registers can hold 16bit values and are referenced in the play scripts.


The media file table
--------------------

The media file table consists of pairs of offsets and length (both 16bit), and ends right before the position referenced by the the first entry. There is no explicit definition of the number of records in this list. 

The media files themselves are encrypted using a simple scheme, using a magic XOR value (`x`):
   - The values `0x00`, `0xFF`, `x` and `x XOR 0xFF` are left alone
   - Everything else is XORed bytewise by x.

The magic XOR value can be found by finding the number which makes the first 4 bytes of the first media file read `OggS` or `RIFF`.

In the header at position `0x001c` there is raw XOR value that is used by the firmware to deduct the real XOR value. It might use some algorithm or a lookup table for that. For all seen TipToi gme files same raw XOR value here leads to same real XOR value. As of now ~55 different combinations have been identified by looking at exisiting gme files.

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
