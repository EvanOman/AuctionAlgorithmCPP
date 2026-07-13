"""ctypes binding to the ``libauction`` shared library.

Locates the shared library via the ``AUCTION_BUILD_DIR`` environment variable,
defaulting to ``<repo_root>/build`` where ``repo_root`` is two parents up from
the ``evals`` directory that contains this package.
"""

from __future__ import annotations

import ctypes
import os
from functools import lru_cache
from pathlib import Path

import numpy as np

# evals/src/auction_evals/native.py -> auction_evals -> src -> evals -> repo_root
_REPO_ROOT = Path(__file__).resolve().parents[3]

_ERROR_MEANINGS = {
    1: "bad arguments (null pointer or n == 0)",
    2: "invalid input (e.g. |cost| > 2^30)",
    3: "internal failure",
}


def build_dir() -> Path:
    """Return the directory expected to contain the built C++ artifacts."""
    override = os.environ.get("AUCTION_BUILD_DIR")
    if override:
        return Path(override)
    return _REPO_ROOT / "build"


def library_path() -> Path:
    """Return the path to the auction shared library."""
    return build_dir() / "libauction.so"


@lru_cache(maxsize=1)
def _load_library() -> ctypes.CDLL:
    path = library_path()
    if not path.exists():
        raise RuntimeError(
            f"auction shared library not found at {path}. "
            "Build the C++ project or set AUCTION_BUILD_DIR."
        )
    lib = ctypes.CDLL(str(path))
    lib.auction_solve.argtypes = [
        ctypes.POINTER(ctypes.c_int64),
        ctypes.c_size_t,
        ctypes.c_int,
        ctypes.POINTER(ctypes.c_size_t),
        ctypes.POINTER(ctypes.c_int64),
    ]
    lib.auction_solve.restype = ctypes.c_int
    return lib


def solve(costs: np.ndarray, maximize: bool) -> tuple[list[int], int]:
    """Solve an n-by-n assignment problem via the native ``auction_solve`` ABI.

    Args:
        costs: an (n, n) int64 ndarray, row-major; costs[i, j] is the cost of
            assigning object j to person i.
        maximize: if True, maximize total cost; otherwise minimize.

    Returns:
        A tuple (assignment, total_cost) where assignment[i] is the object
        assigned to person i.

    Raises:
        RuntimeError: if the native call reports a nonzero error code, or the
            input fails basic shape/dtype validation.
    """
    if costs.ndim != 2 or costs.shape[0] != costs.shape[1]:
        raise RuntimeError(f"costs must be a square 2-D array, got shape {costs.shape}")

    n = costs.shape[0]
    costs_c = np.ascontiguousarray(costs, dtype=np.int64)

    lib = _load_library()
    assignment = np.empty(n, dtype=np.uintp)
    total_cost = ctypes.c_int64(0)

    rc = lib.auction_solve(
        costs_c.ctypes.data_as(ctypes.POINTER(ctypes.c_int64)),
        ctypes.c_size_t(n),
        ctypes.c_int(1 if maximize else 0),
        assignment.ctypes.data_as(ctypes.POINTER(ctypes.c_size_t)),
        ctypes.byref(total_cost),
    )
    if rc != 0:
        meaning = _ERROR_MEANINGS.get(rc, "unknown error")
        raise RuntimeError(f"auction_solve failed with code {rc}: {meaning}")

    return assignment.astype(np.int64).tolist(), int(total_cost.value)
