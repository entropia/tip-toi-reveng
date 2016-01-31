# Changelog for tttool

## 1.6 (2016-01-31)

 * Completely re-done option parsing, so there now proper support for short and
   long options, arbitrary order of options, better error messages and
   per-command `--help` output.

   **Incompatibility:** Some commands are now called differently. In
   particular, to generate PNG files with all codes from a YAML file, use
   `./tttool oid-codes`, not `./tttool oid-code`.
 * The size of the OID codes written by `./tttool oid-code` is configurable.
 * The png files written by `./tttool oid-code` and `./tttool oid-codes` store
   their DPI value, so that importing them into DTP programs is likely to yield
   the desired result.
 * New command `tttool oid-table` that prints all OIDs of a given YAML file as
   a nice and handy table in a PDF file
 * The output of `./tttool games` is more detailed.
 * Games are not exported by `tttool export` and assembled by `tttool
   assemble`. This way, existing Tiptoi products can be modified without losing
   them.
 * The “other” play commands (FFE0, FFE1, FB00) are implemented in the parser,
   printer and serializer, with syntax `P*(..)`, `PA*(..)` and `PA(..)`. These
   are not interesting to you unless you want to understand existing Tiptoi
   products.
 * Support for the “timer” action (FF00) with syntax `T($register,123)`.
 * `./tttool assemble` will print all errors due to missing audio files, not
   just the first.
 * Fix various crashes in corner cases, mostly involving games. Thanks to
   @ToniMahagoni for reporting these!

## 1.5.1 (2015-11-11)

 * Do not assign object IDs >= 15000, as these are not recognized.

## 1.5 (2015-10-28)

 * New commands FB00 (alternative play range command), FFE0 (alternative
   play random sample) and FFE1 (alternative play all samples)
 * New pixel formats -d 1200d and -d 600d,w hich double the size of the
   dots.
 * Assign object IDs dependent on project IDs, to avoid overlap.
 * Fix missing support in tttool play for > and <=

## 1.4 (2015-04-29)

 * The `tttool play` command is greatly enhanced:
   + Audio samples are actually played (On linux, install `sox`).
   + You can use the name of your scripts instead of the OID codes.
   + The prompt supports a persistent history (press ↑).
   + You can tab-complete your input.
 * `tttool` knows its own version.
 * `tttool assemble` warns if the comment field is too long for the GME file.

## 1.3 (2015-03-18)

 * The `tttool play` command now supports jump commands.

## 1.2 (2015-03-08)

 * The `language` fields takes arbitrary strings, which will hopefully be
   understood by the text-to-speak engine.

## 1.1 (2015-02-15)

 * The yaml files support a `language` field, specifying the default language
   for the text-to-speech feature.
 * The `speak` section of the yaml file can have subsections with differing
   languge settings, to allow multi-language files.
 * The text-to-speech feature will use either pico2wave or espeak, depending on
   what is available.
 * The windows release zipfile now comes with espeak and oggenc, so Windows users
   can use text-to-speech out of the box.

## 1.0 (2015-01-29)

 * Last release without a changelog
