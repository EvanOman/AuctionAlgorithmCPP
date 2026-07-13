#include <algorithm>
#include <cstdint>
#include <limits>
#include <numeric>
#include <set>
#include <stdexcept>
#include <vector>

#include "auction/auction.hpp"
#include "doctest.h"

namespace {

using auction::Objective;
using auction::Options;
using auction::Result;
using auction::solve;

bool is_permutation_of_0_to_n(const std::vector<std::size_t>& assignment, std::size_t n) {
    if (assignment.size() != n) return false;
    std::set<std::size_t> seen(assignment.begin(), assignment.end());
    return seen.size() == n && *seen.begin() == 0 && *seen.rbegin() == n - 1;
}

// Exhaustive reference solver for small n.
std::int64_t brute_force(const std::vector<std::int64_t>& costs, std::size_t n, Objective obj) {
    std::vector<std::size_t> perm(n);
    std::iota(perm.begin(), perm.end(), std::size_t{0});
    const bool maximize = obj == Objective::Maximize;
    std::int64_t best = maximize ? std::numeric_limits<std::int64_t>::min()
                                 : std::numeric_limits<std::int64_t>::max();
    do {
        std::int64_t total = 0;
        for (std::size_t i = 0; i < n; ++i) total += costs[i * n + perm[i]];
        best = maximize ? std::max(best, total) : std::min(best, total);
    } while (std::next_permutation(perm.begin(), perm.end()));
    return best;
}

// Deterministic PRNG so tests need no external dependencies.
struct Lcg {
    std::uint64_t state;
    explicit Lcg(std::uint64_t seed) : state(seed) {}
    std::int64_t next(std::int64_t lo, std::int64_t hi) {
        state = state * 6364136223846793005ULL + 1442695040888963407ULL;
        const std::uint64_t span = static_cast<std::uint64_t>(hi - lo + 1);
        return lo + static_cast<std::int64_t>((state >> 33) % span);
    }
};

Options with_objective(Objective obj) {
    Options options;
    options.objective = obj;
    return options;
}

}  // namespace

TEST_CASE("n=1 assigns the only person to the only object") {
    // Regression: the original implementation's scaling loop never ran for
    // n=1 (epsilon > 1/n was 1 > 1), so no assignment was ever produced.
    const Result result = solve({7}, 1);
    CHECK(result.assignment == std::vector<std::size_t>{0});
    CHECK(result.total_cost == 7);
}

TEST_CASE("regression: tie-heavy 3x3 the original solved suboptimally") {
    // Maximizing over this matrix, the 2016 implementation returned
    // assignment [0,1,2] with total 6; the optimum is [2,1,0] with total 7.
    // Root cause: the final epsilon phase ran with n*epsilon > 1.
    const std::vector<std::int64_t> costs = {1, 3, 3,  //
                                             2, 3, 1,  //
                                             1, 1, 2};
    const Result result = solve(costs, 3, with_objective(Objective::Maximize));
    CHECK(result.total_cost == 7);
}

TEST_CASE("known 3x3 minimization") {
    const std::vector<std::int64_t> costs = {4, 1, 3,  //
                                             2, 0, 5,  //
                                             3, 2, 2};
    const Result result = solve(costs, 3);
    CHECK(result.total_cost == 5);  // 1 + 2 + 2
    CHECK(is_permutation_of_0_to_n(result.assignment, 3));
}

TEST_CASE("Machol-Wien instances: cost[i][j] = (i+1)*(j+1)") {
    // A classic stress family for assignment solvers. Minimization pairs
    // the largest row with the smallest column (reverse diagonal);
    // maximization picks the identity.
    for (std::size_t n : {2, 3, 5, 8, 16}) {
        std::vector<std::int64_t> costs(n * n);
        for (std::size_t i = 0; i < n; ++i)
            for (std::size_t j = 0; j < n; ++j)
                costs[i * n + j] = static_cast<std::int64_t>((i + 1) * (j + 1));

        std::int64_t min_expected = 0;
        std::int64_t max_expected = 0;
        for (std::size_t i = 0; i < n; ++i) {
            min_expected += static_cast<std::int64_t>((i + 1) * (n - i));
            max_expected += static_cast<std::int64_t>((i + 1) * (i + 1));
        }

        CHECK(solve(costs, n).total_cost == min_expected);
        CHECK(solve(costs, n, with_objective(Objective::Maximize)).total_cost == max_expected);
    }
}

TEST_CASE("all-equal costs: any permutation is optimal") {
    const std::size_t n = 6;
    const std::vector<std::int64_t> costs(n * n, 5);
    const Result result = solve(costs, n);
    CHECK(is_permutation_of_0_to_n(result.assignment, n));
    CHECK(result.total_cost == 30);
}

TEST_CASE("negative costs") {
    const std::vector<std::int64_t> costs = {-5, -1,  //
                                             -2, -7};
    CHECK(solve(costs, 2).total_cost == -12);
    CHECK(solve(costs, 2, with_objective(Objective::Maximize)).total_cost == -3);
}

TEST_CASE("maximize on C equals negated minimize on -C") {
    Lcg rng(99);
    const std::size_t n = 7;
    std::vector<std::int64_t> costs(n * n);
    std::vector<std::int64_t> negated(n * n);
    for (std::size_t k = 0; k < n * n; ++k) {
        costs[k] = rng.next(-50, 50);
        negated[k] = -costs[k];
    }
    const Result max_result = solve(costs, n, with_objective(Objective::Maximize));
    const Result min_result = solve(negated, n);
    CHECK(max_result.total_cost == -min_result.total_cost);
}

TEST_CASE("matches brute force on random instances") {
    Lcg rng(2016);
    for (std::size_t n : {2, 3, 4, 5, 6, 7}) {
        for (int trial = 0; trial < 25; ++trial) {
            std::vector<std::int64_t> costs(n * n);
            // Narrow ranges force ties, the regime that exposed the
            // original epsilon-scaling bug.
            const std::int64_t hi = trial % 2 == 0 ? static_cast<std::int64_t>(n) : 100;
            for (auto& c : costs) c = rng.next(1, hi);

            for (const Objective obj : {Objective::Minimize, Objective::Maximize}) {
                const Result result = solve(costs, n, with_objective(obj));
                CHECK(is_permutation_of_0_to_n(result.assignment, n));
                CHECK(result.total_cost == brute_force(costs, n, obj));
            }
        }
    }
}

TEST_CASE("custom epsilon options still reach the exact optimum") {
    const std::vector<std::int64_t> costs = {1, 3, 3,  //
                                             2, 3, 1,  //
                                             1, 1, 2};
    Options options = with_objective(Objective::Maximize);
    options.epsilon_init = 40.0;
    options.scaling_factor = 2.0;
    CHECK(solve(costs, 3, options).total_cost == 7);
}

TEST_CASE("input validation") {
    CHECK_THROWS_AS(solve({}, 0), std::invalid_argument);
    CHECK_THROWS_AS(solve({1, 2, 3}, 2), std::invalid_argument);  // size != n*n

    Options bad_scaling;
    bad_scaling.scaling_factor = 1.0;
    CHECK_THROWS_AS(solve({1, 2, 3, 4}, 2, bad_scaling), std::invalid_argument);

    const std::int64_t too_big = (std::int64_t{1} << 30) + 1;
    CHECK_THROWS_AS(solve({too_big}, 1), std::invalid_argument);
}

TEST_CASE("C API round-trip") {
    const std::vector<std::int64_t> costs = {4, 1, 3,  //
                                             2, 0, 5,  //
                                             3, 2, 2};
    std::vector<std::size_t> assignment(3);
    std::int64_t total = 0;
    CHECK(auction_solve(costs.data(), 3, /*maximize=*/0, assignment.data(), &total) == 0);
    CHECK(total == 5);
    CHECK(is_permutation_of_0_to_n(assignment, 3));

    CHECK(auction_solve(nullptr, 3, 0, assignment.data(), &total) == 1);
    CHECK(auction_solve(costs.data(), 0, 0, assignment.data(), &total) == 1);
}
