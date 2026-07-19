Emulator test suite
===================

Behavioural tests for `tttool`: each test **assembles** a small book with
the `tttool` under test and **runs it on the emulated pen** —
[tt-emu](https://github.com/nomeata/tt-emu) boots the real, unmodified pen
firmware — then taps OID codes and asserts on what the firmware actually does
(mounted product, the live `$`-register file, played media, captured audio).

This complements the golden-output suite one directory up: that one checks
tttool's *output files* stay stable, this one checks the assembled GMEs
*behave* correctly on a pen.

Running locally
---------------

Needs [uv](https://docs.astral.sh/uv/) (which fetches tt-emu straight from
GitHub) and the pen firmware:

    TT_EMU_DOWNLOAD_FIRMWARE=1 TTTOOL=/path/to/tttool \
        uv run --project testsuite/emulator-tests pytest testsuite/emulator-tests -v

* `TTTOOL` — the tttool binary to test (defaults to `tttool` on the `PATH`).
* `TT_EMU_FIRMWARE` — path to an `update3202MT.upd`; or set
  `TT_EMU_DOWNLOAD_FIRMWARE=1` to let tt-emu download the official image from
  Ravensburger's servers (SHA-256-verified, cached under `~/.cache/tt-emu`).

Without tttool or firmware the tests *skip* rather than fail. A full run is a
few minutes: booting the emulated pen takes tens of seconds per session, so
the suite keeps to one emulator session per test module.

Layout
------

One pair of files per test, with the same basename: `test_x.py` is the test,
`test_x.yaml` next to it is the book it assembles (the `book` fixture pairs
them structurally). Shared helpers — media generation, audio analysis, GME
byte-patching, the assembler — live in `infra/`.

The test books
--------------

* `test_minimal.yaml` — mounting, script dispatch, `P()` playback, register
  arithmetic, conditionals, named jumps.
* `test_timer.yaml` — the GME script timer: `AT(2)` arms the periodic timer,
  the `timer:` script counts fires in a register, `CT` cancels. Requires a
  tttool with the timer feature (the book fails to assemble otherwise).
* `test_jump.yaml` — `J()` timing: jumps are deferred until the running
  audio ends (that wait is the familiar "delay" when chaining scripts); the
  order of `J` and `P` in a line does not matter, and a jump with no audio is
  immediate. See `test_jump.py` for the measured numbers.
* `test_deferred.yaml` — the undocumented deferred-play opcode 0xFFA1
  (byte-patched into the GME, since tttool cannot write it yet): its
  parity-gated latch and the extra playback it inserts at an audio boundary.
  See `test_deferred.py`.

The audio media are deterministic sine beeps, one pitch per media name,
generated at test time (`infra/media.py`) as plain 16-bit PCM WAV — a format the
pen decodes natively. That lets the tests verify *which* media the firmware
audibly played by frequency analysis of the captured DAC output
(`infra/audio_check.py`), independent of any emulator bookkeeping.

CI
--

`.github/workflows/emulator-tests.yml` builds `tttool` with GHC/cabal (with
dependency caching), then runs this suite with a cached firmware download.
