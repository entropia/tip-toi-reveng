The GME file format
===================

This is the result of reverse engineering — first by staring at hex codes for a long time, later also by analysing the pen's firmware — and might not always be accurate. Also, it is not complete; see the TODO list in [README.md](README.md).

The format, including the script language, is the same across all pen generations (the same book works on every pen).

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
 * `0x0008`: 32bit magic number, `0x0000238b` for all tiptoi products seen so far. The pen checks it when mounting a GME file and rejects files with a different value. (Maybe the value is the Ravensburger customer number at Chomptech.)
 * `0x000C`: 32bit offset to the *timer script* (historically called the *additional script table*): the script executed by the timer armed with command `FE00` (see the section below).
 * `0x0010`: 32bit offset to the *game table* (see the section below)
 * `0x0014`: 32bit. Product id code (== OID code of the power on symbol on page 1). The pen compares it against the tapped OID code when mounting the file.
 * `0x0018`: 32bit. Pointer to register init values (16bit counter followed by n×16bit values. First value is register $0, followed by $1 and so on.)
 * `0x001C`: raw XOR value (8bit), see below at media table explanation. 
 * `0x001D`: three bytes padding the previous value to 32 bit; the pen never reads them. 0 for all products seen so far.
 * `0x0020`: a variable length string, consisting of its length (8bit), and that many characters. Commonly `CHOMPTECH DATA FORMAT CopyRight 2009 Ver2.xx.yyyy` (varies between products, xx can also be one digit only)
 * Next is a 8 date string (`20111024`). The date string seems optional with one condition: if a language string follows the date must consist of at least one ASCII number.
 * Next is an optional language string (currently known: `GERMAN`, `DUTCH`, `FRENCH`, `ITALIAN`, `RUSSIA`. If the language string is provided it must match the language configured in the pen — the pen compares it when mounting the file; the file .tiptoi.log plays no role in this — or the pen will ignore the file. If the language is missing any TipToi pen will accept the file. In the YAML file, this can be set using, for example, `gme-lang: FRENCH`
 * Next there is sequence of zeros up to and including to position 0x5f (padding).
 * `0x0060`: 32bit offset to the *game media file table* (historically called the *additional media file table*): the built-in games read their media through this table, while the play scripts use the main table; see the media file table section below.
 * `0x0071`: 32bit offset to the playlistlist for the the power-on sound (played, when the product is recognized. If 0, no sound is played.)

 The following entries might exist only from Version 2.10.0901
 * `0x008C`: 32bit offset to the *media flag table*: a list of <number of mediafiles> 32bit values (0 or 1), one flag per media file. Some products have 0 here. The flag (called "VoiceNumberNeedRep" by the firmware) is consulted when playing a sample, to suppress playing the same sample twice in a row. Why individual products set it for individual samples has not been investigated.
 * `0x0090`: 32bit offset to the *game binaries table* (probably the games for the ZC3201)
 * `0x0094`: 32bit offset to the *special OID list*
 * `0x0098`: 32bit offset to an *additional game binaries table* (probably the games for the ZC3202N)
 * `0x009C`: 32bit. purpose unknown, can be 0. The pen does not read this field.
 * `0x00A0`: 32bit offset to a game binaries table, which consists of a single binary: the main binary for the ZC3201 (see the game binaries table section).
 * `0x00A4`: 32bit flag, can be 0 or 1 (0 means the rest of the header is filled with 0; 1 means the rest of the header contains one or more of the following offsets). The pen reads it as a single byte.
 * `0x00A8`: 32bit offset to another game binaries table, which also consists of a single binary: the main binary for the ZC3202N.
 * `0x00C8`: 32bit offset to an *additional* game binaries table with a single binary: the main binary for the ZC3203L.
 * `0x00CC`: 32bit offset to another *additional* game binaries table (probably the games for the ZC3203L)

The range from 0x00D0-0x01FF is always 00 (verified up to version 2.10.0901). Probably header bytes reserved for future versions.

The trailing checksum (see below) is never verified: when loading a file, the pen checks only the magic number at `0x0008`, the product id and the language.


The script table
----------------

At the position referenced at `0x0000` (commonly `0x0200`), is the script table. It consists of
 * 32 bit: last used OID code
 * 32 bit: first used OID code
 * Then, 32bit offsets that point to *play scripts* (see below). These correspond linearly to the OID codes.
   E.g. WWW_Bauernhof: The first piglet on page 6 has OID code 1499, the corresponding play script is located at `0x766A`. The 100th 32bit word in the script table contains that offset. So `4×(OID - 1401) = script table index + 8`. The value 1401 in this example is the first used OID code in the product (second 32bit word in the script table).
 * Some of these offsets are `0xFFFFFFFF`. This indicates that the corresponding OID code is not used within the product.
 * The end of the offsets can be found at (script table + 8 + 4×(last used OID code - first used OID code).

The OID codes here are the logical code numbers, as used everywhere by tttool. The scrambled mapping between these and the raw numbers actually encoded in the printed dot patterns is *not* implemented in the pen's firmware: the OID sensor chip already delivers the decoded code number, and the pen uses it as-is. So that mapping lives entirely in the sensor chip (equivalently: in how the patterns are generated), and the empirical table in tttool's `KnownCodes.hs` remains the reference for it.


The play script
---------------

A play script consists of a 16bit number (defining the length of the list to follow) and a list of 32bit offsets. The offsets point to *script lines*.

The script line
---------------

A script line consists of a list of *conditionals*, a list of *actions*, and a so called *playlist* consisting *media file table* indices.

A script line has the format  `aaaa  conditionals... bbbb  actions... cccc media...` where
 * `a` is the number of conditionals. Each conditional consists of 8 bytes.
 * `b` is the number of actions. Each action is 7 bytes. The pen executes at most 8 actions per line and ignores any further ones (hence tttool's warning about such lines).
 * `c` is a playlist

The conditionals are of the format `t1 aaaa cccc t2 bbbb`
 * `t1` & `t2` (uint8) type of `a` and `b` resp. (0 == register, 1 == value)
 * `a` & `b` (uint16) value or id of register
 * `c` (uint16) is the comparison operator
The rest of the line is only considered when the comparison between the
register and the value or other register holds.

The comparison operators are:
 * `FFF9` (written `$r==m?` in tttool's output and the yaml files): Equality
 * `FFFA` (written `$r>m?` in tttool's output): Greather than
 * `FFFB` (written `$r<m?` in tttool's output): Less than
 * `FFFC`: also equality — an alias of `FFF9`. Not seen in GME files so far.
 * `FFFD` (written `$r>=m?` in tttool's output): Greater or equal
 * `FFFE` (written `$r<=?` in tttool's output): Less or equal
 * `FFFF` (written `$r!=m?` in tttool's output): Not equal

This list is complete; the pen treats a condition with any other comparison operator as unsatisfied.

The actions are of the format `rrrr cccc tt mmmm`
 * `r` (uint16) id of register
 * `c` (uint16) is the command
 * `t` (uint8) type of `m` (0 == register, 1 == value)
 * `m` (uint16) value or id of register

The commands are:
 * `FFF0` (written `$r+=m`): Increment register `$r` by `m` or value of `$m`
 * `FFF1` (written `$r-=m`): Decrement register `$r` by `m` or value of `$m`
 * `FFF2` (written `$r*=m`): Multiply register `$r` by `m` or value of `$m`
 * `FFF3` (written `$r%=m`): Set register `$r` to `$r` mod `m`
 * `FFF4` (written `$r/=m`): Set register `$r` to `$r` div `m`
 * `FFF5` (written `$r&=m`): Bitwise add to register `$r` the value of `m`
 * `FFF6` (written `$r|=m`): Bitwise or to register `$r` the value of `m`
 * `FFF7` (written `$r^=m`): Bitwise xor to register `$r` the value of `m`
 * `FFF8` (written `Neg($r)`): Negate register `$r`.
 * `FFF9` (written `$r:=m`): Set register `$r` to `m` or value of `$m`
 * `FFE0` (written `P*`): play one sample of the media list. The pen plays `playlist[n mod length]`, where `n` is an internal event counter. The counter advances once per event the pen processes — which includes an idle poll firing about ten times a second, not just touches — so in practice the pick is timing-dependent, i.e. effectively random.
 * `FFE1` (written `PA*`: play all samples of the media list
 * `FFE8` (written `P(m)`): Play audio referenced by the `m`th entry in the indices list.
 * `FB00` (written `PA(b-a)`): Play all samples from that inclusive range. `a` := lowbyte(`m`), `b` := highbyte(`m`)
 * `FC00` (written `P(b-a)`): Play one sample from that inclusive range, pseudo-randomly: the pick is `c mod (a-b+1) + b` for a free-running counter `c`. `a` := lowbyte(`m`), `b` := highbyte(`m`). (The pen has no true random number generator; all "random" commands reduce to such counters.)
 * `FD00` (written `G(m)`): Begin game `m` (see the game table section).
 * `F8FF` (written `J(m)`): Jump to script `m`.
 * `FAFF` (written `C`): Cancel game mode.
 * `FF00` (written `T($r,m)`): Writes an internal counter to `$r` with values in the range 0..m, i.e. `$r := counter mod (m+1)`. The counter is a tick count incremented on every timer interrupt; the firmware calls this command `Rand` — it is the script language's random number source. Quirk: the register/value flag of the parameter is not checked, `m` is always taken literally.
 * `FE00` (written `AT(m)`): Arms the *script timer*, a periodic software timer that fires every `m`×100 ms. Every time it fires, and the pen is otherwise idle, the pen evaluates the *timer script* (header `0x000C`) and executes the first line whose conditions hold. The register field of this command is ignored, `m` is always taken literally, and arming replaces any previously armed timer.
 * `FEFF` (written `CT`): Cancels the script timer armed by `FE00`.
 * `FEE0`–`FEE7`: select one of eight built-in sound profiles (`FEE8` does nothing). Not seen in GME files so far.
 * `FFA1` (written `P?(m)`, "coin-flip play"): plays the `m`th entry of the indices list — exactly like `FFE8` — but only if the event counter (the same one `FFE0` uses) is even at the moment the action executes; otherwise the command does nothing. The counter's parity at that moment is effectively random, so the sample plays with probability ½. When the pen internally re-runs a line's actions (as happens around games and interrupted audio), the original outcome is repeated, not re-flipped. The register field and the register/value flag are ignored, `m` is always taken literally. Not seen in GME files so far.

This is the complete command set of the pen's script interpreter.

The commands `P`, `G` , `J` and `C` seem to ignore their registers, `C` also its parameter (which always is `FFFF`).

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

The media file table consists of pairs of offsets and length (both 32bit).
There seems to be no explicit definition of the number of records in this list,
so we simply look until either
 * the first media content referenced in the list begins,
 * or the *game media file table* appears, which is then followed by the first
   media content.

The firmware plays Ogg/Vorbis and WAV (both plain PCM and IMA-ADPCM) media. The ZC3202N firmware's media library additionally contains decoders for MP3, FLAC and AMR (among others), so such files also work in a GME on that pen generation. Official products stick to Ogg/Vorbis (typically mono, 22050 Hz) and IMA-ADPCM WAV. A media file in a format the pen does not recognize is skipped silently.

The media files themselves are encrypted using a simple scheme, using a magic XOR value (`x`):
   - The values `0x00`, `0xFF`, `x` and `x XOR 0xFF` are left alone
   - Everything else is XORed bytewise by x.

The magic XOR value can be found by finding the number which makes the first 4 bytes of the first media file read `OggS` or `RIFF`.

In the header at position `0x001c` there is a raw XOR value, from which the pen derives the real XOR value by a plain 256-byte lookup table in its firmware: `real = table[raw]`. The table is a permutation of all 256 byte values, so every raw value is valid. The ~55 raw/real combinations identified empirically from existing gme files are consistent with it. The full mapping can be extracted from a firmware update image; in the ZC3202N's `Update3202MT` firmware, whose main program is based at `0x08009000`, the table is the 256 bytes at `0x080381DA`.

The *game media file table* is what the built-in games resolve their media indices through, while the play scripts use the main table. So far it has been equal to the main media file table in all instances, so this makes no observable difference.


The timer script
----------------

The format of the timer script is that of a play script (see above). The command `FE00` arms a periodic software timer; every time that timer fires, and the pen is otherwise idle, the pen evaluates this table and executes the first line whose conditions hold. Registers and conditionals here thus implement delayed or periodically repeating actions.

Only 'Puzzle Ponyhof' and 'Reise durch die Jahreszeiten' (and their FR, NL, IT versions) ship a non-trivial table. In most files, it is an empty play script, i.e. simply `0x0000`; in 'Reise durch die Jahreszeiten' it is a playscript with empty script lines.


The game table
--------------

The game table configures the pen's *built-in games*: the script command `G(m)` (`FD00`) enters game number `m`, indexing this table. The games themselves are state machines compiled into the pen's firmware; the game table only supplies data to them — which OID codes belong to the game and which playlists accompany each game phase. (This is why the games are hard to reconstruct from the file alone: most of the logic lives in the pen, not in the GME.)

The table is a 32bit count followed by that many 32bit offsets to *game records*.

A game record starts with a 16bit *game type*. The type selects which of the firmware's built-in game engines runs the record (WWW Bauernhof contains one game of each of the types 1–8):

 * types 1, 2, 3 and 5 run one shared question-and-answer engine: each round randomly draws a not-yet-played subgame, plays its announcement, and the player answers by tapping OIDs, with correct/decoy/hint feedback from the subgame's playlists (see below) and the score cascade at the end. Types 1, 3 and 5 are treated *identically* — the engine contains no code distinguishing them. The only variant is type 2, which plays the hint playlist *sequentially* (an escalating hint chain), where the others pick randomly.
 * type 4 is a memory-sequence game: the pen pre-draws *rounds* subgames as a sequence, each round appends one more step, and there is no scoring. Type 40 is the same game with a no-repeat draw.
 * type 6 is a game with a *bonus stage*: reaching the *bonus target* score in the main stage unlocks a second stage with its own subgames, rounds and playlists.
 * type 7 groups its subgames: each round plays one group, its subgames in listed order (no score cascade).
 * type 8 is a *game-select menu*: tapping a menu OID launches the game record listed for it (the same mechanism as the `G(m)` command).
 * type 9 dispatches to fully product-specific game code in the firmware, which keeps only the subgame count; its 75 extra playlist-lists are a cue pool whose individual roles are hard-coded per product in the firmware.
 * type 10 is an endless variant of the common engine: rounds never run out, and the round-complete jingle doubles as the per-answer feedback.
 * type 16 lets the player *choose* the subgame by tapping its selector OID (the record's extra OIDs), instead of the random draw. (For product id 2 only, the firmware runs a different engine on the same record layout — a fixed eight-question quiz.)
 * types 17–23 each have their own dedicated engine in the firmware; their behaviour has not been studied yet. A few published products carry such records.
 * type 253 is an empty placeholder record (just the type, no further data).

The dispatch uses only the low byte of the type word. Any type number not listed above is a silent no-op: entering such a game does nothing, not even an error sound. Published products do carry such records — type 0 (very common) as empty dummy entries, sporadically 11–15 and 30 — and they all belong to products that ship an embedded binary, which handles game taps before the built-in dispatch whenever the GME has a game binaries table at `0x0098`.

After the type, the common layout continues with these 16bit words (type 6 differs, see below):
 * *subgame count*
 * *rounds*: the total number of rounds; each round plays one randomly drawn, not-yet-played subgame.
 * `c`: the *find-all-targets flag*: 0 ⇒ the first correct tap completes the round; 1 ⇒ every OID of the subgame's target list must be found, with "right, keep going" and "already found that one" feedback in between.
 * *early rounds*: the round number at which the round announcement switches from the *round start* to the *later round start* playlists (0 ⇒ no round announcements at all).
 * *repeat OID*: a per-game control OID: tapping it replays the current prompt — the open question, the last hint after a wrong answer, or the last announcement.
 * `x`, `w`, `v`: read and then discarded by the pen — dead fields. In published products they hold 0/0/0 (most records) or 0/111/222 (a few products, e.g. WWW Bauernhof); apparently authoring-tool metadata.

Then follow five 32bit offsets to playlistlists for the game phases: *game start*, *round end*, *finish*, *round start*, *later round start*. Then *subgame count* 32bit offsets to the subgames, ten 16bit *target scores*, and ten 32bit offsets to *finish playlistlists*: at game end, the score (one point per completed round) is cascaded against the target scores, and the matching finish playlist is played. A type-specific tail follows (menu tables for type 8, subgame groups for type 7, extra OIDs and playlists for type 16, …; see tttool's `GMEParser.hs` for the exact layouts).

In type 6, the words after *rounds* are instead: *bonus subgame count*, *bonus rounds*, *bonus target* (the minimum main-stage score that unlocks the bonus stage), `i` (this layout's slot of the find-all-targets flag), *early rounds*, `q` (the bonus stage's *early rounds*), then *repeat OID*, `x`, `w`, `v` as above; it also carries separate bonus-stage playlistlists, target scores and subgame ids.

A *subgame* consists of a 20-byte header (ten 16bit words), three OID lists (each a 16bit count plus that many 16bit codes), and nine 32bit offsets to playlistlists.

The three OID lists are: the *target OIDs* (the correct answers); the *decoys* (known-wrong answers with dedicated feedback); and the subgame's remaining *active OIDs* (which get hint feedback). Taps outside all three lists get a "that's not part of this game" response.

Of the ten words in the subgame header, the pen uses the first five:
 * word 0: how the correct-answer feedback playlist is indexed — randomly, by the target's list position, or by the number of targets found so far;
 * word 1: a strict "targets in listed order" mode. (Cleanly implemented for type 16; the generic engine's version reads a stale value — a firmware bug — so its effect there is unreliable and content should leave it 0.)
 * word 2: how the wrong-answer feedback playlist is indexed (random or by decoy position);
 * words 3 and 4: the maximum number of wrong taps on the decoy resp. remaining OIDs before the pen plays the give-up/solution playlist and ends the round (0 ⇒ unlimited);
 * words 5–9: never used.

The nine subgame playlistlists have fixed roles: 1 = question/round announcement, 2 = correct-answer feedback, 3 = decoy-wrong feedback, 4 = "not part of this game", 5 = "already found that one" (find-all mode), 6 = hint chain for other wrong taps (played sequentially — escalating hints), 7 = round-complete jingle, 8 = give-up/solution, 9 = never read.

During a game the pen additionally reacts to the in-game control OIDs `0x162A`–`0x1631` — the printed next / repeat / stop / yes / no control symbols, remapped to internal codes.


Special OID list
----------------

This consists of 40 bytes (twenty 16bit words). It contains the OIDs that have a
special meaning/function in the book.

Example from Puzzle Ponyhof:

0x00F05E40:             68 18 00 00 67 18 64 18 65 18 00 00
                        aa aa bb bb cc cc dd dd ee ee pp pp
0x00F05E50: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            pp pp pp pp pp pp pp pp pp pp pp pp pp pp pp pp
0x00F05E60: 00 00 00 00 00 00 00 00 01 00 66 18
            pp pp pp pp pp pp pp pp ff ff gg gg

a: OID for the Replay symbol
b: OID for the Stop symbol
c: OID for the Skip symbol
d: unknown
e: unknown (in this example: OID for game mode)
p: unknown, 0 in all products seen so far
f: 0 or 1: enables the recording of played samples for the replay function (which is why it is always 1 when g ≠ 0)
g: unknown (in this example: discover mode)

The pen itself reads exactly three of the twenty words: `a` and `c` (treated identically, as the two control symbols that must not count as content taps) and `f`. All the other words — including `b` and the seemingly-padding `p` words — are handed as a parameter block to the GME's embedded binary programs (see the game binaries tables), so their meaning is defined by each product's binary.

That the pen never reads `b` natively is surprising for a "Stop symbol", but consistent: during the built-in games, stop (like next, repeat and yes/no) is one of the fixed control codes `0x162A`–`0x1631` that the firmware recognizes directly (`0x162B` = stop), independent of this list; and outside the games any new tap preempts the running audio anyway. What, if anything, the base firmware does with a tap on the `b` OID specifically has not been traced — as far as analyzed, per-product stop behaviour lives in the embedded binaries.



The game binaries table
-----------------------

Existing at least in 'WWW Weltatlas', this table holds (ARM) binary data, recognizable from the different strings contained in the binaries.
The table consists of a 16bit? length and another 112bit (14 bytes) padding, adding up to 16 bytes. Then, the entries of the table follow:
Each entry consists of a 32bit pointer to the binary data, a 32bit length of the binary data and 8bytes for an abbreviated binary name (Non-zero terminated).

Very likely, the `0`-th entry of the table is skipped, because it starts in the same place as the table length entry.
The same layout can be found in the additional game binaries table.

Each pen generation loads and runs a single embedded *main binary* from "its" single-binary table: the ZC3201 from header `0x00A0`, the ZC3202N from `0x00A8`, the ZC3203L from `0x00C8`. The loader reads the table's (address, length), loads the binary, hands it a function table (a `system_api`) through which the binary can call back into the pen (file access, audio, OID helpers, …), and jumps into it. A single GME file can thus carry a per-pen binary in each of those tables and run on all pen generations.



The checksum
------------

The last 4 bytes of the file are a simple additive check-sum over the file. The pen never verifies it (see the header section: only magic, product id and language are checked when a file is loaded).
