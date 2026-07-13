# Auction Algorithm CPP

A C++17 implementation of [Bertsekas' auction algorithm](http://dspace.mit.edu/bitstream/handle/1721.1/3233/P-2064-24690022.pdf?sequence=1) for the n-by-n assignment problem: optimally assigning n objects to n people given a cost matrix. Originally written in 2016 as a single `auction.cpp` file; modernized in 2026 into a proper library, CLI, test suite, and Python evals package.

[![CI](https://github.com/EvanOman/AuctionAlgorithmCPP/actions/workflows/ci.yml/badge.svg)](https://github.com/EvanOman/AuctionAlgorithmCPP/actions/workflows/ci.yml)

**[Live demo](https://evanoman.github.io/AuctionAlgorithmCPP/)** — the solver compiled to WebAssembly, with an animated visualization, a problem playground, and an explanation of the algorithm (`site/`, deployed by the Pages workflow).

## What's here

- **Library** (`include/auction/`, `src/`) — `auction::solve`, built as both a shared library (`libauction.so`, for FFI) and a static library (for embedding), plus a C ABI (`auction_solve`) for consuming it from other languages.
- **CLI** (`apps/`) — the `auction` executable: `solve` a problem file and `generate` random ones.
- **Tests** (`tests/`) — a [doctest](https://github.com/doctest/doctest) unit test suite covering the solver and the problem file format.
- **Evals** (`evals/`) — a Python (uv) package of correctness checks against `scipy.optimize.linear_sum_assignment` and performance benchmarks.

## Building

Requires a C++17 compiler, CMake, and [just](https://github.com/casey/just) (evals additionally need [uv](https://github.com/astral-sh/uv)).

```bash
just build
# or, directly:
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
```

## CLI usage

An example problem file (`# comments` and blank lines are ignored; `objective` defaults to `min`):

```
# example: 3x3 assignment problem
objective min
n 3
4 1 3
2 0 5
3 2 2
```

Solving it:

```console
$ ./build/auction solve example.txt
objective: min
n: 3
total_cost: 5
phases: 3
rounds: 7
assignment:
  0 -> 1
  1 -> 0
  2 -> 2
```

Add `--json` for machine-readable output, or pipe a problem in on stdin with `auction solve -`. Use `auction generate N [--lo A --hi B --seed S --objective min|max]` to emit a random N-by-N problem file. Run `auction --help` for full usage.

## Library usage

```cpp
#include "auction/auction.hpp"

std::vector<std::int64_t> costs = {4, 1, 3,
                                    2, 0, 5,
                                    3, 2, 2};  // row-major, n=3
auction::Options options;
options.objective = auction::Objective::Minimize;

auction::Result result = auction::solve(costs, /*n=*/3, options);
// result.assignment[i] = object assigned to person i
// result.total_cost = total cost of the assignment
```

For FFI, the shared library exports a C ABI:

```c
extern "C" int auction_solve(const std::int64_t* costs, std::size_t n, int maximize,
                             std::size_t* assignment_out, std::int64_t* total_cost_out);
```

It's consumable from Python via `ctypes` — see `evals/src/auction_evals/native.py` for a working binding.

## Testing

```bash
just test-all   # C++ (ctest) + Python (pytest) suites
just test       # C++ only
just test-py    # Python evals only
```

## Benchmarks

```bash
just bench            # full benchmark suite
just bench --quick    # fast subset
```

## Bugs fixed in the 2026 modernization

The original implementation could return suboptimal assignments due to an off-by-one in its epsilon-scaling termination condition, among other issues. See [`docs/BUGS.md`](docs/BUGS.md) for the full writeup with reproductions. The original source is preserved unmodified at [`legacy/auction_original.cpp`](legacy/auction_original.cpp).
