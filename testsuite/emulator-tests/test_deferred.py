"""Deferred play (the undocumented action opcode 0xFFA1), measured end to end.

The opcode cannot be written in tttool YAML yet, so this test assembles a
normal book and byte-patches two ``$sink := m`` actions (same 7-byte layout)
into 0xFFA1 actions (see ``infra/gme_patch.py``), then observes the firmware.

What the experiments on the emulated ZC3202N ("MT") pen established — which
refines the current one-line description in GME-Format.md:

* **The latch**: executing 0xFFA1 first *clears* the latch flag; only when the
  firmware's per-dispatch counter is even does it store its raw operand
  (truncated to a byte; the register/value flag is ignored) and set the flag.
  Script dispatches also happen on idle polling, so whether a tap lands on
  even parity is effectively timing-dependent — roughly a coin flip per tap.
  (Game context: +0x125 dispatch counter, +0xDDF flag, +0xDE0 value; offsets
  from the firmware disassembly, read via ``fw_mt.AKOID_PTR``.)

* **The deferred play**: a line's actions run one audio boundary at a time.
  When the sequence reaches a queued 0xFFA1 slot *and the latch flag is set*,
  the pen inserts one extra playback there — selected by the latched index —
  before the line continues. With the flag cleared (odd-parity dispatch) the
  slot is skipped silently. Hence the assertion below: tapping
  ``P(hello) 0xFFA1(1) P(tick)`` repeatedly yields three media playbacks on
  latched taps and two on unlatched ones.

* What does **not** work: putting the raw value 0xFFA1 into a *playlist* (the
  reading currently suggested in GME-Format.md's opcode list) did not play
  the latched media in these experiments — the deferred play is a property of
  the action sequence, not of playlist entries.

Media playback is counted via the emulator's play-media event log (the same
channel tt-emu's own tests use); pitch analysis cannot separate the
back-to-back clips here.
"""

from __future__ import annotations

#: game-context offsets (ZC3202N MT firmware disassembly)
LATCH_FLAG = 0xDDF
LATCH_INDEX = 0xDE0


def test_deferred_play(book, firmware, tmp_path) -> None:
    from tt_emu import Emulator
    from tt_emu.firmware import mt as fw_mt

    from infra.gme_patch import patch_action_opcode, patch_file

    gme, yaml = book

    def patch(data: bytearray) -> None:
        # defer script: "$sink := 2"  ->  0xFFA1(2)   (pure latch, no playlist)
        patch_action_opcode(data, 4716, expect_opcode=0xFFF9, new_opcode=0xFFA1)
        # trigger script: P(hello) [$sink := 1 -> 0xFFA1(1)] P(tick)
        patch_action_opcode(data, 4717, expect_opcode=0xFFF9, new_opcode=0xFFA1, action=1)

    patched = tmp_path / "deferred-patched.gme"
    patched.write_bytes(gme.read_bytes())
    patch_file(patched, patch)

    with Emulator(firmware, gme=patched, yaml=yaml) as pen:
        pen.tap("product")
        assert pen.mounted == 902

        def game_ctx(offset: int) -> int:
            return pen.machine.read_u8(pen.machine.read_u32(fw_mt.AKOID_PTR) + offset)

        # The latch half: re-tap the pure-latch script until a tap lands on
        # even dispatch parity (see docstring); then the flag is set and the
        # raw operand (2) is stored.
        for _ in range(8):
            pen.tap("defer")
            pen.wait("200ms")
            if game_ctx(LATCH_FLAG) == 1:
                break
        pen.expect(game_ctx(LATCH_FLAG) == 1, "0xFFA1 never latched on even parity in 8 taps")
        pen.expect(game_ctx(LATCH_INDEX) == 2, f"latched value should be 2, got {game_ctx(LATCH_INDEX)}")

        # The play half: P(hello) 0xFFA1(1) P(tick) plays 3 media when the
        # 0xFFA1 dispatch latched (hello, the deferred extra play, tick) and
        # 2 when it did not. Tap until both outcomes were observed.
        counts: list[int] = []
        for _ in range(8):
            events_before = len(pen._audio_events)
            pen.tap("trigger")
            pen.wait("1500ms")
            counts.append(
                len([e for e in pen._audio_events[events_before:] if e.kind == "media"])
            )
            if 2 in counts and 3 in counts:
                break
        pen.expect(
            3 in counts,
            f"a latched 0xFFA1 slot should insert an extra playback (per-tap counts: {counts})",
        )
        pen.expect(
            2 in counts,
            f"an unlatched 0xFFA1 slot should be skipped (per-tap counts: {counts})",
        )
        pen.expect(
            set(counts) <= {2, 3},
            f"only 2 or 3 playbacks per trigger tap expected (per-tap counts: {counts})",
        )
