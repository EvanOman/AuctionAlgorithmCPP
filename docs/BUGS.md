# Bugs in the original 2016 implementation

Deep review of `legacy/auction_original.cpp` (the repo's original `auction.cpp`),
with each finding verified empirically where possible. Line numbers refer to
that file.

## 1. ε-scaling exits one phase too early → suboptimal assignments (correctness)

```cpp
while(epsilon > 1.0/N)
{
    reset(&assignment, INF);
    while (...) auctionRound(...);
    epsilon = epsilon * .25;
}
```

The auction algorithm's optimality guarantee for integer costs requires the
*final completed phase* to run with `N * epsilon < 1`. This loop checks the
condition *before* the phase and scales *after* it, so the last phase that
actually executes uses an epsilon in `(1/N, 4/N]` — up to four times too
coarse. The result is only guaranteed to be within `N * epsilon ≈ 4` of
optimal, not exactly optimal.

**Empirical proof:** maximizing over

```
1 3 3
2 3 1
1 1 2
```

the original returns assignment `[0, 1, 2]` with total **6**; the optimum is
`[2, 1, 0]` (or `[1, 0, 2]`) with total **7**. Found by sweeping 210 random
instances against `scipy.optimize.linear_sum_assignment`; tie-heavy matrices
(costs drawn from a narrow range) trigger it. With the loop restructured so a
final phase runs at `epsilon < 1/N`, the same 210 instances all solve to
optimality.

**Fix:** run the phase first, then exit only after a phase completed with
`epsilon < 1/N` (see `src/auction.cpp`).

## 2. `N = 1` produces no assignment at all (correctness)

With `N = 1` the outer loop condition is `epsilon > 1.0/N` → `1.0 > 1.0` →
false, so **no auction phase ever runs** and the assignment vector stays at
its `INF` sentinel. The original then reports success (and under `VERBOSE`
prints "Person 0 gets object 2147483647").

## 3. Transposed matrix conventions (latent correctness / doc bug)

Three different index conventions coexist:

- `auctionRound` reads person *i*, object *j* as `C[j + i*N]` (row-major),
- `makeRandC` writes `mat[i + j*size]` (column-major — harmless only because
  the data is i.i.d. random),
- `printMatrix` prints `mat[i + j*size]` (column-major).

So the `VERBOSE` "cost matrix" printout is the **transpose** of the matrix the
algorithm actually solves. Anyone adapting the code to a real (non-random)
cost matrix inherits a silent transposition bug.

## 4. Uninitialized variables (undefined behavior)

`optObjForI` and `secOptObjForI` are declared uninitialized. On the first
loop iteration `secOptObjForI = optObjForI` copies an indeterminate value —
technically UB, and `optObjForI` itself remains indeterminate if every
`curVal` underflows the `-INF` sentinel (possible once prices grow large,
since `-INF` here is only `-INT_MAX`, not `-∞`).

## 5. Single-object bid overflow (edge-case correctness)

`bidForI = optValForI - secOptValForI + epsilon` uses `-INF` (= `-INT_MAX`)
as the "no second object" sentinel, so for `N = 1` (reachable only after
fixing bug 2) the bid becomes ≈ 2.1e9 and the price explodes. The sentinel
needed to be a true `-infinity` with an explicit no-second-object branch.

## 6. `vector<auto>` is not standard C++ (portability)

`void printMatrix(vector<auto>* mat, ...)`, `reset(vector<auto>*, auto)`, and
`printVec(vector<auto>*)` are not valid C++14 (nor valid C++20 — `auto` as a
*template argument* was never standardized). GCC accepts it as an extension
with a warning; Clang and MSVC reject it. The advertised build command
(`g++-5 -std=c++14`) relies on that extension.

## 7. Quadratic bid aggregation (performance)

After bidding, the awarding loop calls `getIndicesWithVal(&tmpBidded, j)` for
**every** object `j`, scanning the full bid list N times → O(N²) per round on
top of the O(unassigned × N) bidding work, plus a `find` over the assignment
vector per round and a linear scan to locate each object's previous owner.
A single pass keeping the best bid per object (and an owner map) removes all
of these.

## 8. `int` indexing overflows for large N (scale limit)

`C->at(j + i*N)` computes the index in `int`; for N ≥ 46,341, `i*N` overflows
`int` (UB). Matrix indices need `size_t`.

## 9. Minor issues

- `srand(time(NULL))` is re-seeded inside `makeRandC` on every call.
- The `VERBOSE` displacement message swaps its arguments: it prints the *new
  bidder* as the displaced person and the *person index* as the object.
- `iter` starts at 1 and increments before the first round, over-counting by
  one; it also counts rounds, not "iterations" per the label.
- "Total time" prints seconds while "Total CPU time" prints milliseconds,
  with no units on either.
- `prices` are initialized to 1 rather than 0 (harmless: a uniform price
  offset never changes any argmax), and the ε-schedule ignores the cost
  magnitude (`epsilon = 1.0` regardless of cost range), which hurts
  performance on wide cost ranges.

## Verification

Bugs 1 and 2 are locked in as regression tests in `tests/test_auction.cpp`
(`regression: tie-heavy 3x3 ...`, `n=1 assigns ...`). The rewrite in
`src/auction.cpp` addresses all of the above; `evals/` re-checks optimality
against SciPy's Hungarian solver end-to-end on every CI run.
