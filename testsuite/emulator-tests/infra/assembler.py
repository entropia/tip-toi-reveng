"""Assemble test books with the tttool under test, in a temporary directory.

Books are assembled outside the checkout (together with freshly generated
media, see :mod:`infra.media`), so the repository stays clean and the
``.codes.yaml`` side files never leak into it.
"""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

from .media import write_all


class Assembler:
    def __init__(self, tttool: str, directory: Path) -> None:
        self.tttool = tttool
        self.directory = directory

    def assemble(self, book_yaml: Path) -> tuple[Path, Path]:
        """Assemble the book ``book_yaml``; return ``(gme, yaml)`` paths.

        The book is assembled under an 8.3-safe basename (``test_timer.yaml``
        → ``timer.gme``): a ``.gme`` whose name needs a VFAT long filename
        still mounts on the emulated pen, but the firmware's re-open by name
        misses it and the register file never initialises (tt-emu quirk).

        Raises :class:`subprocess.CalledProcessError` if tttool rejects the
        book.
        """
        short = book_yaml.stem.removeprefix("test_")
        assert len(short) <= 8, f"book stem {short!r} does not fit an 8.3 name"
        yaml = self.directory / f"{short}.yaml"
        gme = yaml.with_suffix(".gme")
        if gme.exists():
            return gme, yaml
        write_all(self.directory / "media")
        shutil.copy(book_yaml, yaml)
        subprocess.run(
            [self.tttool, "assemble", yaml.name],
            cwd=self.directory,
            check=True,
            capture_output=True,
            text=True,
        )
        assert gme.exists(), f"tttool assemble succeeded but {gme} is missing"
        return gme, yaml
