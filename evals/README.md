# auction-evals

End-to-end evals and benchmarks for the auction assignment solver in
`../build` (`libauction.so` / `auction` CLI). These tests treat the C++
solver as the system under test: they cross-check its output against
`scipy.optimize.linear_sum_assignment` and closed-form optima, and exercise
both the ctypes ABI and the CLI binary.

## Layout

- `src/auction_evals/native.py` — ctypes binding to `libauction.so`.
- `src/auction_evals/cli.py` — helpers to drive the `auction` CLI binary.
- `src/auction_evals/generators.py` — random/structured cost-matrix families.
- `src/auction_evals/bench.py` — benchmark harness (`auction-bench` script).
- `tests/` — pytest suite: optimality, CLI round-trips, algebraic
  properties, and native-binding error paths.

## Setup

```sh
cd evals
uv sync --dev
```

By default the solver artifacts are located at `../build` relative to this
directory (i.e. `<repo_root>/build`). Override with `AUCTION_BUILD_DIR` if
you built elsewhere.

## Running

```sh
# Full test suite (the referee for the C++ solver)
uv run pytest

# Lint / format / typecheck
uv run ruff format .
uv run ruff check .
uv run ty check .

# Benchmark: native solver vs. scipy, with an optimality cross-check
uv run auction-bench --quick
uv run auction-bench --sizes 100,500,1000 --families uniform_narrow,uniform_wide,machol_wien --trials 3
```
