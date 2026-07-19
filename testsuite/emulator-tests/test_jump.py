"""J() jump timing: when does a jump actually happen, and does J/P order matter?

The firmware implements 0xF8FF (``J``) as a *deferred* jump: the interpreter
only records "jump pending, target = …" and takes the jump when the current
audio playback ends. ``test_jump.yaml`` measures the consequences on the
emulated ZC3202N ("MT") pen; jump arrival is observed via a register the
target script increments, timed against the emulated clock (faithful DAC
pacing, so playback takes its real duration; the "long" clip is 2.0 s).

Measured (and asserted below):

* ``P(long) J(mark)`` and ``J(mark) P(long)`` behave **identically** (jump
  taken ~2.2 s after the tap, when the 2 s clip ends): the order of ``J`` and
  ``P`` within a line does not matter, and the line's own playlist plays
  before the jump fires.
* ``J(mark)`` with no audio at all jumps essentially **immediately**
  (~0.1 s). The community lore that a ``J`` without a following ``P`` makes
  second-generation pens pause for ~2 seconds does *not* reproduce on this
  (second-generation) firmware — also not when the jump target itself plays
  audio (its playback starts right away).
* A jump target's audio starts only after the source line's audio has
  finished — the familiar "delay" when chaining scripts with ``J`` is simply
  the jump waiting for the running playback to end.

The generous upper bounds keep the test robust; the discriminating margins
(2 s vs. sub-second) are far larger than any scheduling jitter.
"""

from __future__ import annotations

import pytest


def test_jump_timing(book, firmware) -> None:
    from tt_emu import Emulator
    from tt_emu.emulator import SESSION_INSTRUCTIONS_PER_TICK

    instructions_per_second = SESSION_INSTRUCTIONS_PER_TICK * 50

    gme, yaml = book
    with Emulator(firmware, gme=gme, yaml=yaml, dac_pacing="faithful") as pen:
        pen.tap("product")
        assert pen.mounted == 903

        arrivals = 0

        def jump_delay(script: str) -> float:
            """Tap ``script``; seconds until the jump target has run."""
            nonlocal arrivals
            pen.tap(script)
            start = pen.machine.clock
            arrivals += 1
            pen.wait_until(lambda: pen.registers["arrived"] == arrivals, timeout="15s")
            delay = (pen.machine.clock - start) / instructions_per_second
            pen.wait("3s")  # let any remaining audio finish before the next tap
            return delay

        # P-then-J and J-then-P: both wait for the 2 s clip to end; order is
        # irrelevant (measured 2.22 s vs 2.18 s).
        d_pj = jump_delay("pj")  # $m:=1 P(long) J(mark)
        d_jp = jump_delay("jp")  # $m:=2 J(mark) P(long)
        pen.expect(1.8 <= d_pj <= 3.5, f"P(long) J(mark): jump after {d_pj:.2f}s, want ~2.2s")
        pen.expect(1.8 <= d_jp <= 3.5, f"J(mark) P(long): jump after {d_jp:.2f}s, want ~2.2s")
        pen.expect(abs(d_pj - d_jp) < 0.5, f"order must not matter: {d_pj:.2f}s vs {d_jp:.2f}s")

        # A jump with no audio is immediate (measured 0.06-0.08 s) — no trace
        # of the rumoured ~2 s penalty for a J without a P.
        d_jonly = jump_delay("jonly")  # $m:=3 J(mark)
        pen.expect(d_jonly < 0.5, f"J with no audio should be immediate, took {d_jonly:.2f}s")

        # A short clip delays the jump only for its own duration (~0.36 s for
        # the 0.25 s beep).
        d_jshort = jump_delay("jshort")  # $m:=4 J(mark) P(beep)
        pen.expect(d_jshort < 1.0, f"J + 0.25s beep: jump after {d_jshort:.2f}s, want ~0.4s")
        pen.expect(d_jshort < d_pj, "a shorter clip must mean an earlier jump")

        # The famous chaining delay: the target's own playback starts only
        # once the source clip has finished (measured: tick starts 2.26 s
        # after long began).
        events_before = len(pen._audio_events)
        pen.tap("jplay")  # $m:=5 P(long) J(sing); sing plays tick
        start = pen.machine.clock
        arrivals += 1
        pen.wait_until(lambda: pen.registers["arrived"] == arrivals, timeout="15s")
        pen.wait("2s")
        media = [e for e in pen._audio_events[events_before:] if e.kind == "media"]
        pen.expect(len(media) >= 2, f"source and target playback expected, got {media}")
        gap = (media[1].start_clock - media[0].start_clock) / instructions_per_second
        pen.expect(1.8 <= gap <= 3.5, f"target audio must wait for the 2s source clip, gap {gap:.2f}s")
