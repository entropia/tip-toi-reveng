"""A tttool-assembled book behaves correctly on the emulated pen.

Assembles ``test_minimal.yaml`` and drives it through tt-emu's scripted
:class:`~tt_emu.Emulator`: product mount, script dispatch, register arithmetic,
conditionals and named jumps — asserted on the pen's live ``$``-register file
(the ground truth of script execution) plus playback evidence.

Playback evidence comes in two forms:

* *events* — the firmware entering its play-media routine. These fire while a
  ``tap()`` is still latching, so baselines are taken **before** the tap (the
  emulator's ``expect_play`` arms too late for one-line scripts, and its
  media-*name* join is unreliable for minimal books; counting events via the
  emulator's event log — same channel tt-emu's own test suite uses — is exact).
* *audio* — at the end, the captured DAC output must contain the distinct sine
  pitches of the media that played audibly (see ``infra/audio_check.py``), proving
  the generated PCM WAV media really decode on the pen. Not every event is
  audible: tt-emu has a known bug where playback restarted after ~2 s of
  emulated idle stays silent, so the audio assertion is deliberately lenient.

One session per module: booting the pen is by far the slowest step.
"""

from __future__ import annotations

from infra.audio_check import has_tone, tone_segments
from infra.media import TONES


def test_minimal_book(book, firmware, tmp_path) -> None:
    from tt_emu import Emulator

    gme, yaml = book
    with Emulator(firmware, gme=gme, yaml=yaml) as pen:
        # Booted into book mode; nothing mounted yet.
        assert pen.state == "book"
        assert pen.mounted is None

        # The product tap mounts the game (and plays the welcome).
        pen.tap("product")
        assert pen.mounted == 900
        assert pen.registers["count"] == 0
        assert pen.registers["mode"] == 0

        # A plain P() script plays exactly one media.
        events_before = len(pen._audio_events)
        pen.tap("hello")
        pen.wait("500ms")
        hello_events = [e for e in pen._audio_events[events_before:] if e.kind == "media"]
        pen.expect(len(hello_events) == 1, f"one play from tap(hello), got {hello_events}")

        # Register arithmetic: two counter taps increment $count twice.
        pen.tap("counter")
        pen.wait("500ms")
        pen.expect(pen.registers["count"] == 1, "first counter tap")
        pen.tap("counter")
        pen.wait("500ms")
        pen.expect(pen.registers["count"] == 2, "second counter tap")

        # := plus a named jump plus a conditional: modeset sets $mode:=2 and
        # jumps to gate, whose first line fires only because $mode == 2.
        events_before = len(pen._audio_events)
        pen.tap("modeset")
        pen.wait("500ms")
        pen.expect(pen.registers["mode"] == 2, "modeset must set $mode := 2")
        gate_events = [e for e in pen._audio_events[events_before:] if e.kind == "media"]
        pen.expect(len(gate_events) == 1, f"the gate script plays once, got {gate_events}")

        session_wav = tmp_path / "minimal-session.wav"
        stats = pen.save_wav(session_wav)
        assert stats.total_bytes > 0

    # The captured DAC output contains our sine media, audibly decoded by the
    # pen: at least the tap(hello) "hello" beep and one "tick". ("secret" often
    # falls into tt-emu's silent-restart-after-idle bug, so it is not required.)
    segments = tone_segments(session_wav)
    assert len(segments) >= 2, f"expected audible playback, got {segments}"
    assert has_tone(segments, TONES["hello"]), f"no 'hello' beep in {segments}"
    assert has_tone(segments, TONES["tick"]), f"no 'tick' beep in {segments}"
