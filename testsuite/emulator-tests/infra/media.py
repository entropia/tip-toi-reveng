"""Deterministic test media: tiny sine-beep WAV files.

The pen decodes Ogg/Vorbis and (IMA-ADPCM / plain PCM) RIFF WAV; plain 16-bit
PCM WAV is the one format we can generate dependency-free and reproducibly with
the stdlib, so the test books use that. Each media name gets its own pitch, so
a human listening to a captured session WAV can tell the clips apart.
"""

from __future__ import annotations

import math
import struct
import wave
from pathlib import Path

SAMPLE_RATE = 22050
DURATION_SECONDS = 0.25
AMPLITUDE = 12000

#: media name -> sine frequency (Hz); one entry per file used by the books
TONES = {
    "hello": 440.0,
    "tick": 660.0,
    "secret": 880.0,
    "beep": 1046.0,
    "long": 550.0,
}

#: clip length overrides (seconds); everything else is DURATION_SECONDS short
DURATIONS = {
    "long": 2.0,
}


def write_tone(path: Path, frequency: float, duration: float = DURATION_SECONDS) -> None:
    frames = int(SAMPLE_RATE * duration)
    samples = bytearray()
    for i in range(frames):
        # a short fade-in/out avoids clicks and keeps the clip clearly non-silent
        envelope = min(1.0, i / 200.0, (frames - i) / 200.0)
        value = int(AMPLITUDE * envelope * math.sin(2.0 * math.pi * frequency * i / SAMPLE_RATE))
        samples += struct.pack("<h", value)
    with wave.open(str(path), "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(SAMPLE_RATE)
        w.writeframes(bytes(samples))


def write_all(directory: Path) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    for name, frequency in TONES.items():
        write_tone(directory / f"{name}.wav", frequency, DURATIONS.get(name, DURATION_SECONDS))
