"""Benchmark harness comparing the native auction solver against scipy.

Cross-checks optimality against ``scipy.optimize.linear_sum_assignment`` on
every instance timed (not just a sample), and prints a GitHub-markdown table
of median wall-clock time per solver, per (family, size).
"""

from __future__ import annotations

import argparse
import statistics
import time
import zlib

import numpy as np
from scipy.optimize import linear_sum_assignment

from auction_evals.generators import FAMILIES
from auction_evals.native import solve

DEFAULT_SIZES = [100, 250, 500, 1000, 2000]
DEFAULT_FAMILIES = ["uniform_narrow", "uniform_wide"]
QUICK_SIZES = [50, 100, 200]
QUICK_TRIALS = 2


def _time_native(costs: np.ndarray) -> tuple[float, int]:
    start = time.perf_counter()
    _assignment, total = solve(costs, maximize=False)
    elapsed_ms = (time.perf_counter() - start) * 1000.0
    return elapsed_ms, total


def _time_scipy(costs: np.ndarray) -> tuple[float, int]:
    start = time.perf_counter()
    row_ind, col_ind = linear_sum_assignment(costs)
    elapsed_ms = (time.perf_counter() - start) * 1000.0
    total = int(costs[row_ind, col_ind].sum())
    return elapsed_ms, total


def run_benchmark(
    sizes: list[int], families: list[str], trials: int
) -> list[dict[str, float | int | str]]:
    """Run the benchmark grid, returning one row dict per (family, size)."""
    rows: list[dict[str, float | int | str]] = []
    for family_name in families:
        generator = FAMILIES[family_name]
        for n in sizes:
            native_times: list[float] = []
            scipy_times: list[float] = []
            for trial in range(trials):
                seed = zlib.crc32(f"{family_name}:{n}:{trial}".encode())
                rng = np.random.default_rng(seed)
                costs = generator(n, rng)

                native_ms, native_total = _time_native(costs)
                scipy_ms, scipy_total = _time_scipy(costs)

                if native_total != scipy_total:
                    raise AssertionError(
                        f"optimality mismatch for family={family_name} n={n} trial={trial}: "
                        f"native={native_total} scipy={scipy_total}\ncosts=\n{costs.tolist()}"
                    )

                native_times.append(native_ms)
                scipy_times.append(scipy_ms)

            native_median = statistics.median(native_times)
            scipy_median = statistics.median(scipy_times)
            ratio = native_median / scipy_median if scipy_median > 0 else float("inf")
            rows.append(
                {
                    "family": family_name,
                    "n": n,
                    "auction_ms": native_median,
                    "scipy_ms": scipy_median,
                    "ratio": ratio,
                }
            )
    return rows


def format_table(rows: list[dict[str, float | int | str]]) -> str:
    header = "| family | n | auction median ms | scipy median ms | ratio (auction/scipy) |"
    separator = "| --- | ---: | ---: | ---: | ---: |"
    lines = [header, separator]
    for row in rows:
        lines.append(
            f"| {row['family']} | {row['n']} | {row['auction_ms']:.3f} | "
            f"{row['scipy_ms']:.3f} | {row['ratio']:.3f} |"
        )
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Benchmark the native auction solver against scipy.linear_sum_assignment."
    )
    parser.add_argument(
        "--sizes",
        default=",".join(str(s) for s in DEFAULT_SIZES),
        help="comma-separated list of problem sizes",
    )
    parser.add_argument(
        "--families",
        default=",".join(DEFAULT_FAMILIES),
        help="comma-separated list of instance families",
    )
    parser.add_argument("--trials", type=int, default=3, help="trials per (family, size)")
    parser.add_argument(
        "--quick", action="store_true", help="use small sizes and fewer trials for a fast smoke run"
    )
    args = parser.parse_args()

    if args.quick:
        sizes = QUICK_SIZES
        trials = QUICK_TRIALS
    else:
        sizes = [int(s) for s in args.sizes.split(",")]
        trials = args.trials

    families = [f.strip() for f in args.families.split(",")]
    unknown = set(families) - set(FAMILIES)
    if unknown:
        raise SystemExit(f"unknown families: {sorted(unknown)}; known: {sorted(FAMILIES)}")

    rows = run_benchmark(sizes, families, trials)
    print(format_table(rows))


if __name__ == "__main__":
    main()
