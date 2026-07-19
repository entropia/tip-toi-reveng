"""Shared infrastructure for the emulator tests (not a test itself).

* ``media`` — deterministic sine-beep WAV generation, one pitch per media name
* ``audio_check`` — tone detection on captured session WAVs
* ``gme_patch`` — byte-surgery on assembled GMEs (for opcodes tttool cannot
  write yet), checksum-fixing included
* ``assembler`` — runs ``tttool assemble`` on a test's book in a temp dir

Each test module ``test_x.py`` has its book as ``test_x.yaml`` next to it
(same basename); the ``book`` fixture in ``conftest.py`` pairs them.
"""
