# Changelog for tttool

## 1.3 (2015-03-18)

 * The `tttoo play` command now supports jump commands.

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
