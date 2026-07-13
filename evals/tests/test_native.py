"""Error-path and validation tests for the ctypes native binding."""

from __future__ import annotations

import numpy as np
import pytest

from auction_evals.native import solve


def test_cost_exceeding_limit_raises() -> None:
    costs = np.array([[2**30 + 1, 0], [0, 0]], dtype=np.int64)
    with pytest.raises(RuntimeError, match="2\\^30"):
        solve(costs, maximize=False)


def test_negative_cost_exceeding_limit_raises() -> None:
    costs = np.array([[-(2**30 + 1), 0], [0, 0]], dtype=np.int64)
    with pytest.raises(RuntimeError):
        solve(costs, maximize=False)


def test_cost_at_limit_succeeds() -> None:
    costs = np.array([[2**30, 0], [0, 0]], dtype=np.int64)
    assignment, total = solve(costs, maximize=False)
    assert sorted(assignment) == [0, 1]
    assert total == 0


def test_non_square_shape_raises() -> None:
    costs = np.zeros((2, 3), dtype=np.int64)
    with pytest.raises(RuntimeError, match="square"):
        solve(costs, maximize=False)


def test_1d_shape_raises() -> None:
    costs = np.zeros(4, dtype=np.int64)
    with pytest.raises(RuntimeError, match="square"):
        solve(costs, maximize=False)


def test_non_int64_dtype_is_coerced() -> None:
    # native.solve should coerce to int64 rather than erroring on e.g. int32/float input.
    costs = np.array([[4, 1], [2, 0]], dtype=np.int32)
    assignment, total = solve(costs, maximize=False)
    assert sorted(assignment) == [0, 1]
    # 4,1 / 2,0 -> min is object1->person0 (1) + object0->person1 (2) = 3
    assert total == 3


def test_float_dtype_truncates_like_int_cast() -> None:
    costs = np.array([[4.0, 1.0], [2.0, 0.0]], dtype=np.float64)
    assignment, total = solve(costs, maximize=False)
    assert sorted(assignment) == [0, 1]
    # 4,1 / 2,0 -> min is object1->person0 (1) + object0->person1 (2) = 3
    assert total == 3


def test_zero_size_raises() -> None:
    costs = np.zeros((0, 0), dtype=np.int64)
    with pytest.raises(RuntimeError):
        solve(costs, maximize=False)


def test_n1_solve() -> None:
    costs = np.array([[42]], dtype=np.int64)
    assignment, total = solve(costs, maximize=False)
    assert assignment == [0]
    assert total == 42
