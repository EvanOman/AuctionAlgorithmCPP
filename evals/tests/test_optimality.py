"""Cross-check the native solver's optimality against scipy and closed forms.

These tests are the referee for the C++ solver: any disagreement with scipy
or the Machol-Wien closed form is treated as a real bug in the C++ code, not
a test to be loosened.
"""

from __future__ import annotations

import numpy as np
import pytest
from scipy.optimize import linear_sum_assignment

from auction_evals.generators import constant, geometric, machol_wien, uniform_narrow, uniform_wide
from auction_evals.native import solve

RANDOM_FAMILIES = {
    "uniform_narrow": uniform_narrow,
    "uniform_wide": uniform_wide,
    "geometric": geometric,
    "constant": constant,
}

# n -> number of seeds to try; keep total instances around ~250 while giving
# larger n (slower scipy) fewer repeats.
SIZE_SEED_COUNTS = {
    1: 5,
    2: 5,
    3: 5,
    5: 4,
    8: 4,
    13: 3,
    21: 3,
    50: 2,
    100: 2,
    200: 1,
}


def _assert_permutation(assignment: list[int], n: int) -> None:
    assert sorted(assignment) == list(range(n))


def _scipy_total(costs: np.ndarray, maximize: bool) -> int:
    matrix = -costs if maximize else costs
    row_ind, col_ind = linear_sum_assignment(matrix)
    return int(costs[row_ind, col_ind].sum())


def _cases() -> list[tuple[str, int, int]]:
    cases = []
    for family_name in RANDOM_FAMILIES:
        for n, num_seeds in SIZE_SEED_COUNTS.items():
            for seed in range(num_seeds):
                cases.append((family_name, n, seed))
    return cases


@pytest.mark.parametrize("family_name,n,seed", _cases())
def test_native_matches_scipy_minimize(family_name: str, n: int, seed: int) -> None:
    rng = np.random.default_rng(seed)
    costs = RANDOM_FAMILIES[family_name](n, rng)

    assignment, total = solve(costs, maximize=False)
    _assert_permutation(assignment, n)

    expected = _scipy_total(costs, maximize=False)
    assert total == expected, (
        f"minimize mismatch family={family_name} n={n} seed={seed}: "
        f"native={total} scipy={expected}\ncosts=\n{costs.tolist()}"
    )


@pytest.mark.parametrize("family_name,n,seed", _cases())
def test_native_matches_scipy_maximize(family_name: str, n: int, seed: int) -> None:
    rng = np.random.default_rng(seed)
    costs = RANDOM_FAMILIES[family_name](n, rng)

    assignment, total = solve(costs, maximize=True)
    _assert_permutation(assignment, n)

    expected = _scipy_total(costs, maximize=True)
    assert total == expected, (
        f"maximize mismatch family={family_name} n={n} seed={seed}: "
        f"native={total} scipy={expected}\ncosts=\n{costs.tolist()}"
    )


MACHOL_WIEN_SIZES = [2, 5, 10, 25, 50]


def _machol_wien_min_optimum(n: int) -> int:
    # Optimal minimizing assignment pairs person i (1-indexed) with object
    # (n - i + 1): cost sum_{i=1}^{n} i * (n - i + 1).
    return sum(i * (n - i + 1) for i in range(1, n + 1))


def _machol_wien_max_optimum(n: int) -> int:
    # Optimal maximizing assignment pairs person i with object i: cost i^2.
    return sum(i * i for i in range(1, n + 1))


@pytest.mark.parametrize("n", MACHOL_WIEN_SIZES)
def test_machol_wien_minimize_closed_form(n: int) -> None:
    rng = np.random.default_rng(0)
    costs = machol_wien(n, rng)

    assignment, total = solve(costs, maximize=False)
    _assert_permutation(assignment, n)

    expected_closed_form = _machol_wien_min_optimum(n)
    expected_scipy = _scipy_total(costs, maximize=False)
    assert total == expected_closed_form == expected_scipy


@pytest.mark.parametrize("n", MACHOL_WIEN_SIZES)
def test_machol_wien_maximize_closed_form(n: int) -> None:
    rng = np.random.default_rng(0)
    costs = machol_wien(n, rng)

    assignment, total = solve(costs, maximize=True)
    _assert_permutation(assignment, n)

    expected_closed_form = _machol_wien_max_optimum(n)
    expected_scipy = _scipy_total(costs, maximize=True)
    assert total == expected_closed_form == expected_scipy
