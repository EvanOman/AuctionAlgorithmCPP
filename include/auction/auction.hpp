#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>

namespace auction {

/// Which way to optimize the total cost of the assignment.
enum class Objective { Minimize, Maximize };

struct Options {
    Objective objective = Objective::Minimize;

    /// Starting value for the epsilon-scaling schedule. <= 0 selects
    /// automatically (half the largest absolute cost, at least 1).
    double epsilon_init = 0.0;

    /// Factor epsilon is divided by between scaling phases. Must be > 1.
    double scaling_factor = 4.0;
};

struct Result {
    /// assignment[i] = object assigned to person i (a permutation of 0..n-1).
    std::vector<std::size_t> assignment;

    /// Total cost of the assignment in the caller's original cost terms.
    std::int64_t total_cost = 0;

    /// Final auction prices per object (diagnostic; dual variables).
    std::vector<double> prices;

    /// Number of epsilon-scaling phases run.
    std::uint64_t phases = 0;

    /// Total number of bidding rounds across all phases.
    std::uint64_t rounds = 0;
};

/// Solve the n-by-n assignment problem with Bertsekas' auction algorithm.
///
/// `costs` is row-major: costs[i * n + j] is the cost of assigning object j
/// to person i. Requires costs.size() == n * n and n >= 1.
///
/// Costs must satisfy |c| <= 2^30 so that epsilon-scaled arithmetic stays
/// well clear of double-precision rounding; throws std::invalid_argument
/// otherwise.
///
/// The returned assignment is exactly optimal (not just epsilon-optimal):
/// the final scaling phase runs with n * epsilon < 1, which for integer
/// costs guarantees optimality.
Result solve(const std::vector<std::int64_t>& costs, std::size_t n, const Options& options = {});

}  // namespace auction

/// C ABI for consuming the shared library from other languages (e.g. Python
/// ctypes). Returns 0 on success, nonzero on error:
///   1 = bad arguments (null pointers, n == 0)
///   2 = cost magnitude out of supported range
///   3 = internal failure
extern "C" int auction_solve(const std::int64_t* costs, std::size_t n, int maximize,
                             std::size_t* assignment_out, std::int64_t* total_cost_out);
