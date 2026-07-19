"""Tone detection on a captured session WAV.

The test media are pure sine beeps with one pitch per media name
(see ``infra/media.py``), so the captured DAC output identifies *which* media the
firmware actually decoded and played — independent of any event bookkeeping.
Frequency is estimated per non-silent segment by zero-crossing rate, which is
plenty for telling 440 from 660 from 880 Hz.
"""

from __future__ import annotations

import struct
import wave
from pathlib import Path


def tone_segments(path: Path) -> list[tuple[float, float]]:
    """Non-silent segments of ``path`` as ``(duration_seconds, frequency_hz)``."""
    with wave.open(str(path), "rb") as w:
        frames, rate, channels = w.getnframes(), w.getframerate(), w.getnchannels()
        data = w.readframes(frames)
    mono = struct.unpack(f"<{frames * channels}h", data)[:: channels]

    threshold = 500  # amplitude floor: our beeps peak at 12000
    gap = rate // 10  # 100 ms of quiet ends a segment
    segments: list[tuple[float, float]] = []
    start: int | None = None
    quiet = 0
    for i, sample in enumerate(mono):
        if abs(sample) > threshold:
            if start is None:
                start = i
            quiet = 0
        elif start is not None:
            quiet += 1
            if quiet > gap:
                segments.append(_measure(mono[start : i - quiet], rate))
                start = None
    if start is not None:
        segments.append(_measure(mono[start:], rate))
    return segments


def _measure(segment: tuple[int, ...], rate: int) -> tuple[float, float]:
    crossings = sum(
        1 for j in range(1, len(segment)) if (segment[j - 1] < 0) != (segment[j] < 0)
    )
    duration = len(segment) / rate
    frequency = crossings / 2.0 / duration if duration else 0.0
    return duration, frequency


def has_tone(segments: list[tuple[float, float]], frequency: float, tolerance: float = 80.0) -> bool:
    """Whether some segment's dominant frequency is within ``tolerance`` of ``frequency``."""
    return any(abs(f - frequency) <= tolerance for _, f in segments)
