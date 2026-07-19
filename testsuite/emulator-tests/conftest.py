"""Shared fixtures: locate tttool, locate/download the firmware, assemble books.

The suite needs two external ingredients:

* **tttool** — the binary under test. Resolution: the ``$TTTOOL`` environment
  variable, else ``tttool`` on the ``PATH``; otherwise every test is skipped.
* **the pen firmware** — the official ``update3202MT.upd``. Resolution: the
  ``$TT_EMU_FIRMWARE`` environment variable (a path); else, if
  ``$TT_EMU_DOWNLOAD_FIRMWARE`` is set, tt-emu downloads and caches it
  (SHA-256-pinned) from Ravensburger's servers; otherwise every emulator test
  is skipped. This mirrors tt-emu's own test-suite conventions.

Layout convention: each test module ``test_x.py`` keeps its book as
``test_x.yaml`` right next to it — the ``book`` fixture assembles the YAML
with the same basename as the requesting test file. Shared helpers live in
``infra/``.
"""

from __future__ import annotations

import os
import shutil
from pathlib import Path

import pytest

from infra.assembler import Assembler


def _tttool() -> str | None:
    env = os.environ.get("TTTOOL")
    if env:
        return env if Path(env).exists() else None
    return shutil.which("tttool")


@pytest.fixture(scope="session")
def tttool() -> str:
    exe = _tttool()
    if exe is None:
        pytest.skip("tttool not found (set $TTTOOL or put tttool on the PATH)")
    return exe


@pytest.fixture(scope="session")
def firmware() -> str | None:
    """Path to the ``.upd``, or None to let tt-emu download-and-cache it."""
    env = os.environ.get("TT_EMU_FIRMWARE")
    if env:
        if not Path(env).exists():
            pytest.skip(f"$TT_EMU_FIRMWARE={env} does not exist")
        return env
    if os.environ.get("TT_EMU_DOWNLOAD_FIRMWARE"):
        return None  # Emulator(firmware=None) downloads and caches, SHA-pinned
    pytest.skip("firmware not available (set $TT_EMU_FIRMWARE or $TT_EMU_DOWNLOAD_FIRMWARE=1)")


@pytest.fixture(scope="session")
def assembler(tttool: str, tmp_path_factory: pytest.TempPathFactory) -> Assembler:
    return Assembler(tttool, tmp_path_factory.mktemp("books"))


@pytest.fixture()
def book(assembler: Assembler, request: pytest.FixtureRequest) -> tuple[Path, Path]:
    """The requesting test's own book, assembled: ``(gme, yaml)`` paths.

    The book is the ``.yaml`` with the same basename as the test file
    (``test_x.py`` ↔ ``test_x.yaml``), so the pairing is structural.
    """
    return assembler.assemble(request.path.with_suffix(".yaml"))
