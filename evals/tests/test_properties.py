"""Algebraic invariants the native solver must satisfy regardless of instance."""

from __future__ import annotations

import numpy as np
import pytest

from auction_evals.generators import geometric, uniform_narrow, uniform_wide
from auction_evals.native import solve

FAMILIES = {"uniform_narrow": uniform_narrow, "uniform_wide": uniform_wide, "geometric": geometric}
SIZES = [1, 2, 3, 5, 8, 13, 21]


def _instances() -> list[tuple[str, int, int]]:
    return [(name, n, seed) for name in FAMILIES for n in SIZES for seed in range(3)]


@pytest.mark.parametrize("family_name,n,seed", _instances())
def test_min_equals_negated_max(family_name: str, n: int, seed: int) -> None:
    """min(C) total == -max(-C) total."""
    rng = np.random.default_rng(seed)
    costs = FAMILIES[family_name](n, rng)

    _, min_total = solve(costs, maximize=False)
    _, neg_max_total = solve(-costs, maximize=True)

    assert min_total == -neg_max_total


@pytest.mark.parametrize("family_name,n,seed", _instances())
def test_constant_shift_changes_total_by_n_times_shift(family_name: str, n: int, seed: int) -> None:
    """Adding a constant k to every entry shifts the optimal total by n*k
    and leaves the optimal assignment unchanged (every assignment's total
    shifts identically, so the argmin/argmax set is invariant)."""
    rng = np.random.default_rng(seed)
    costs = FAMILIES[family_name](n, rng)
    k = 17

    assignment, total = solve(costs, maximize=False)
    shifted_assignment, shifted_total = solve(costs + k, maximize=False)

    assert shifted_total == total + n * k
    assert sorted(shifted_assignment) == list(range(n))
    # The shifted problem has the identical cost total under the *original*
    # assignment, so the shifted-optimal total must match it exactly.
    original_cost_under_shifted_solution = sum(
        int(costs[i, shifted_assignment[i]]) for i in range(n)
    )
    assert original_cost_under_shifted_solution == total


@pytest.mark.parametrize("family_name,n,seed", _instances())
def test_transpose_preserves_optimal_total(family_name: str, n: int, seed: int) -> None:
    """Solving C^T gives the same optimal total as solving C (the assignment
    inverts: person/object roles swap)."""
    rng = np.random.default_rng(seed)
    costs = FAMILIES[family_name](n, rng)

    _, total = solve(costs, maximize=False)
    _, transposed_total = solve(costs.T, maximize=False)

    assert total == transposed_total


@pytest.mark.parametrize("family_name,n,seed", _instances())
def test_transpose_assignment_inverts(family_name: str, n: int, seed: int) -> None:
    """The assignment for C^T is (compatible with) the inverse permutation
    of the assignment for C, when costs are checked back against C."""
    rng = np.random.default_rng(seed)
    costs = FAMILIES[family_name](n, rng)

    assignment, total = solve(costs, maximize=False)
    transposed_assignment, transposed_total = solve(costs.T, maximize=False)

    # transposed_assignment[j] = i means object i assigned to person j in C^T,
    # i.e. person i assigned to object j in C. That should reproduce the same
    # optimal total when re-scored against the original cost matrix.
    inverse_total = sum(int(costs[transposed_assignment[j], j]) for j in range(n))
    assert inverse_total == total
    assert total == transposed_total


def test_duplicate_row_instance_yields_valid_permutation() -> None:
    """An instance with a duplicated row (multiple equally-good options for
    two people) must still produce a valid permutation."""
    rng = np.random.default_rng(0)
    for n in (2, 3, 5, 8, 13):
        base = uniform_narrow(n, rng)
        costs = base.copy()
        costs[1] = costs[0]  # duplicate row 0 into row 1

        assignment, _total = solve(costs, maximize=False)
        assert sorted(assignment) == list(range(n))

        assignment_max, _total_max = solve(costs, maximize=True)
        assert sorted(assignment_max) == list(range(n))


def test_all_duplicate_rows_constant_matrix_yields_valid_permutation() -> None:
    for n in (1, 2, 5, 10):
        costs = np.full((n, n), 3, dtype=np.int64)
        assignment, total = solve(costs, maximize=False)
        assert sorted(assignment) == list(range(n))
        assert total == 3 * n
