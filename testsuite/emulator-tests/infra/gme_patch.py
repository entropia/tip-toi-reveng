"""Minimal GME surgery for tests: patch script actions and playlist entries.

Opcodes that tttool cannot write yet (like the deferred-play action 0xFFA1,
see ``test_deferred.py``) are tested by assembling a normal book and then
patching the assembled GME:

* an action's opcode is rewritten (e.g. ``$sink := m`` → 0xFFA1 with operand
  ``m``: same 7-byte action, opcode 0xFFF9 → 0xFFA1, register zeroed), or
* a playlist entry is rewritten to a raw value,

followed by fixing the trailing additive checksum. Every patch asserts the
bytes it expects to replace, so a change in tttool's output fails loudly
instead of corrupting the test.

Layout walked here (see GME-Format.md): header 0x0000 → script table, whose
first two u32 are the last/first OID, then one u32 script offset per OID; a
script is a u16 line count plus u32 line offsets; a line is
``u16 ncond, ncond*8, u16 nact, nact*7, u16 nplay, nplay*2``.
"""

from __future__ import annotations

import struct
from pathlib import Path

SENTINEL = 0xFFA1


def _line_offset(gme: bytes, oid: int, line: int) -> int:
    (script_table,) = struct.unpack_from("<I", gme, 0x0000)
    last, first = struct.unpack_from("<II", gme, script_table)
    assert first <= oid <= last, f"OID {oid} outside script table [{first},{last}]"
    (script,) = struct.unpack_from("<I", gme, script_table + 8 + 4 * (oid - first))
    assert script != 0xFFFFFFFF, f"OID {oid} has no script"
    (lines,) = struct.unpack_from("<H", gme, script)
    assert line < lines, f"script {oid} has {lines} lines, wanted line {line}"
    (offset,) = struct.unpack_from("<I", gme, script + 2 + 4 * line)
    return offset


def _actions_offset(gme: bytes, line_offset: int) -> tuple[int, int]:
    (ncond,) = struct.unpack_from("<H", gme, line_offset)
    at = line_offset + 2 + 8 * ncond
    (nact,) = struct.unpack_from("<H", gme, at)
    return at + 2, nact


def patch_action_opcode(
    gme: bytearray, oid: int, *, expect_opcode: int, new_opcode: int, line: int = 0, action: int = 0
) -> None:
    """Rewrite one action's opcode (and zero its register field)."""
    start, nact = _actions_offset(gme, _line_offset(gme, oid, line))
    assert action < nact, f"line has {nact} actions, wanted action {action}"
    at = start + 7 * action
    register, opcode = struct.unpack_from("<HH", gme, at)
    assert opcode == expect_opcode, f"action at {at:#x} has opcode {opcode:#06x}, expected {expect_opcode:#06x}"
    struct.pack_into("<HH", gme, at, 0, new_opcode)


def patch_playlist_entry(
    gme: bytearray, oid: int, *, expect: int, new: int, line: int = 0, entry: int = 0
) -> None:
    """Rewrite one entry of a line's playlist."""
    start, nact = _actions_offset(gme, _line_offset(gme, oid, line))
    playlist = start + 7 * nact
    (nplay,) = struct.unpack_from("<H", gme, playlist)
    assert entry < nplay, f"playlist has {nplay} entries, wanted entry {entry}"
    at = playlist + 2 + 2 * entry
    (value,) = struct.unpack_from("<H", gme, at)
    assert value == expect, f"playlist entry at {at:#x} is {value}, expected {expect}"
    struct.pack_into("<H", gme, at, new)


def fix_checksum(gme: bytearray) -> None:
    """Recompute the trailing additive checksum (sum of all preceding bytes)."""
    struct.pack_into("<I", gme, len(gme) - 4, sum(gme[:-4]) & 0xFFFFFFFF)


def patch_file(path: Path, patch) -> None:
    """Load ``path``, apply ``patch(bytearray)``, fix the checksum, write back."""
    gme = bytearray(path.read_bytes())
    patch(gme)
    fix_checksum(gme)
    path.write_bytes(bytes(gme))
