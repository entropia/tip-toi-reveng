"""The GME script timer (``AT``/``CT`` and the ``timer:`` YAML field), end to end.

``test_timer.yaml`` arms the pen's periodic script timer with ``AT(2)``
(2 × 100 ms = one fire per 200 ms of idle pen time) and counts the fires in
``$t`` from the timer script; ``CT`` cancels the timer. Running it on the real
firmware under tt-emu pins down the semantics tttool implements:

* the timer is periodic — ``$t`` advances by ~10 per 2 s of idle time,
* it only fires while the pen is idle,
* ``CT`` really cancels — ``$t`` freezes afterwards.

Register assertions only: timer fires happen without any audio, and registers
are the ground truth of script execution (deterministic pacing makes the fire
counts reproducible; the ranges below leave margin for tap-timing shifts).
"""

from __future__ import annotations


def test_script_timer(book, firmware) -> None:
    from tt_emu import Emulator

    gme, yaml = book

    with Emulator(firmware, gme=gme, yaml=yaml) as pen:
        pen.tap("product")
        assert pen.mounted == 901
        assert pen.registers["t"] == 0

        # Arm the timer: AT(2) -> one fire per 200 ms of idle pen time.
        pen.tap("arm")
        pen.wait("2s")
        t1 = pen.registers["t"]
        pen.expect(6 <= t1 <= 14, f"~10 timer fires expected after 2s of AT(2), got {t1}")

        # It keeps firing periodically.
        pen.wait("2s")
        t2 = pen.registers["t"]
        pen.expect(t2 - t1 >= 6, f"timer must keep firing: {t1} -> {t2}")

        # CT cancels: no further fires, even after generous waiting.
        pen.tap("disarm")
        t3 = pen.registers["t"]
        pen.wait("2s")
        t4 = pen.registers["t"]
        pen.expect(t4 == t3, f"CT must stop the timer: {t3} -> {t4}")
