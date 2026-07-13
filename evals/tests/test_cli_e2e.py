"""End-to-end tests driving the compiled `auction` CLI binary as a subprocess."""

from __future__ import annotations

import numpy as np
import pytest
from scipy.optimize import linear_sum_assignment

from auction_evals.cli import binary_path, format_problem, run_cli, solve_file
from auction_evals.generators import uniform_narrow, uniform_wide


def _scipy_total(costs: np.ndarray, maximize: bool) -> int:
    matrix = -costs if maximize else costs
    row_ind, col_ind = linear_sum_assignment(matrix)
    return int(costs[row_ind, col_ind].sum())


@pytest.mark.parametrize("family", [uniform_narrow, uniform_wide])
@pytest.mark.parametrize("n", [1, 2, 5, 13, 30])
@pytest.mark.parametrize("objective", ["min", "max"])
@pytest.mark.parametrize("json_output", [True, False])
def test_solve_matches_scipy(family, n, objective, json_output) -> None:
    rng = np.random.default_rng(n * 7 + (1 if objective == "max" else 0))
    costs = family(n, rng)
    text = format_problem(costs, objective)

    result = solve_file(text, json_output=json_output)

    assert result["n"] == n
    assert result["objective"] == objective
    assignment = result["assignment"]
    assert sorted(assignment) == list(range(n))

    expected = _scipy_total(costs, maximize=(objective == "max"))
    assert result["total_cost"] == expected


def test_negative_costs_round_trip() -> None:
    costs = np.array([[-5, 2, 0], [3, -1, -4], [1, 1, -2]], dtype=np.int64)
    text = format_problem(costs, "min")
    result = solve_file(text, json_output=True)

    assert sorted(result["assignment"]) == [0, 1, 2]
    expected = _scipy_total(costs, maximize=False)
    assert result["total_cost"] == expected


def test_format_problem_includes_objective_and_comments_ignored() -> None:
    costs = np.array([[1, 2], [3, 4]], dtype=np.int64)
    text = format_problem(costs, "max")
    assert text.splitlines()[0] == "objective max"

    commented = "# a leading comment\n\n" + text + "# trailing comment\n"
    result = solve_file(commented, json_output=True)
    assert result["objective"] == "max"
    assert result["total_cost"] == _scipy_total(costs, maximize=True)


def test_default_objective_is_min_when_omitted() -> None:
    text = "n 2\n1 2\n3 4\n"
    result = solve_file(text, json_output=True)
    assert result["objective"] == "min"
    costs = np.array([[1, 2], [3, 4]], dtype=np.int64)
    assert result["total_cost"] == _scipy_total(costs, maximize=False)


def test_generate_pipe_to_solve_round_trip() -> None:
    generate = run_cli(["generate", "12", "--seed", "99", "--objective", "max"])
    assert generate.returncode == 0
    assert generate.stdout.strip() != ""

    solved = run_cli(["solve", "-", "--json"], stdin_text=generate.stdout)
    assert solved.returncode == 0

    import json

    result = json.loads(solved.stdout)
    assert result["n"] == 12
    assert result["objective"] == "max"
    assert sorted(result["assignment"]) == list(range(12))


def test_n1_solve_via_cli() -> None:
    text = "n 1\n7\n"
    result = solve_file(text, json_output=True)
    assert result["n"] == 1
    assert result["assignment"] == [0]
    assert result["total_cost"] == 7


class TestMalformedFiles:
    def test_missing_n(self) -> None:
        result = run_cli(["solve", "-"], stdin_text="objective min\n1 2\n3 4\n")
        assert result.returncode == 2
        assert "line" in result.stderr

    def test_short_row(self) -> None:
        result = run_cli(["solve", "-"], stdin_text="n 2\n1 2\n3\n")
        assert result.returncode == 2
        assert "line" in result.stderr

    def test_too_few_rows(self) -> None:
        result = run_cli(["solve", "-"], stdin_text="n 3\n1 2 3\n4 5 6\n")
        assert result.returncode == 2
        assert "line" in result.stderr

    def test_too_many_rows(self) -> None:
        result = run_cli(["solve", "-"], stdin_text="n 1\n1\n2\n")
        assert result.returncode == 2
        assert "line" in result.stderr

    def test_junk_row(self) -> None:
        result = run_cli(["solve", "-"], stdin_text="n 2\nfoo bar\n1 2\n")
        assert result.returncode == 2
        assert "line" in result.stderr

    def test_empty_input(self) -> None:
        result = run_cli(["solve", "-"], stdin_text="")
        assert result.returncode == 2
        assert "line" in result.stderr

    def test_bad_objective_value(self) -> None:
        result = run_cli(["solve", "-"], stdin_text="objective sideways\nn 1\n1\n")
        assert result.returncode == 2
        assert "line" in result.stderr

    def test_duplicate_n_line(self) -> None:
        result = run_cli(["solve", "-"], stdin_text="n 1\nn 2\n1\n")
        assert result.returncode == 2
        assert "line" in result.stderr


def test_binary_exists() -> None:
    assert binary_path().exists()
