"""Instance generators for assignment-problem benchmarking and testing.

Every generator has the signature ``(n: int, rng: np.random.Generator) ->
np.ndarray`` and returns an (n, n) int64 cost matrix.
"""

from __future__ import annotations

import numpy as np


def uniform_wide(n: int, rng: np.random.Generator) -> np.ndarray:
    """Costs drawn uniformly from [1, 1000 * n] -- few ties."""
    hi = max(1, 1000 * n)
    return rng.integers(1, hi + 1, size=(n, n), dtype=np.int64)


def uniform_narrow(n: int, rng: np.random.Generator) -> np.ndarray:
    """Costs drawn uniformly from [1, n] -- ties-heavy for small n."""
    hi = max(1, n)
    return rng.integers(1, hi + 1, size=(n, n), dtype=np.int64)


def machol_wien(n: int, rng: np.random.Generator) -> np.ndarray:
    """The classic Machol-Wien instance: cost[i, j] = (i + 1) * (j + 1).

    Deterministic; ``rng`` is accepted for signature uniformity but unused.
    """
    del rng
    i = np.arange(1, n + 1, dtype=np.int64).reshape(n, 1)
    j = np.arange(1, n + 1, dtype=np.int64).reshape(1, n)
    return i * j


def geometric(n: int, rng: np.random.Generator) -> np.ndarray:
    """Costs spanning several orders of magnitude: 10 ** uniform(0, 6), rounded to int."""
    exponents = rng.uniform(0.0, 6.0, size=(n, n))
    return np.floor(10.0**exponents).astype(np.int64)


def constant(n: int, rng: np.random.Generator) -> np.ndarray:
    """All costs equal -- every assignment is optimal."""
    del rng
    return np.full((n, n), 7, dtype=np.int64)


FAMILIES = {
    "uniform_wide": uniform_wide,
    "uniform_narrow": uniform_narrow,
    "machol_wien": machol_wien,
    "geometric": geometric,
    "constant": constant,
}
