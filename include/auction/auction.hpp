#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
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

/// Sentinel for "no person" in trace events.
constexpr std::size_t kNoPerson = static_cast<std::size_t>(-1);

/// One object award during the auction: in round `round` of phase `phase`
/// (running at `epsilon`), `object` was won by `winner`, displacing previous
/// owner `displaced` (kNoPerson if the object was unowned), and its price
/// rose to `price_after`.
struct TraceEvent {
    std::uint64_t phase;
    std::uint64_t round;
    double epsilon;
    std::size_t object;
    std::size_t winner;
    std::size_t displaced;
    double price_after;
};

struct Trace {
    std::vector<TraceEvent> events;

    /// Recording stops (solving continues) once events reaches this size.
    std::size_t max_events = 100000;
    bool truncated = false;
};

/// Same as solve(), additionally recording every object award into `trace`
/// (used by the web demo to animate the algorithm's real execution).
Result solve_traced(const std::vector<std::int64_t>& costs, std::size_t n, const Options& options,
                    Trace& trace);

/// Serialize a result (and optionally its trace) as a JSON object.
std::string result_to_json(const Result& result, Objective objective, const Trace* trace = nullptr);

}  // namespace auction

/// C ABI for consuming the shared library from other languages (e.g. Python
/// ctypes). Returns 0 on success, nonzero on error:
///   1 = bad arguments (null pointers, n == 0)
///   2 = cost magnitude out of supported range
///   3 = internal failure
extern "C" int auction_solve(const std::int64_t* costs, std::size_t n, int maximize,
                             std::size_t* assignment_out, std::int64_t* total_cost_out);

/// Traced variant returning a malloc'd JSON document (solution + per-award
/// events; see solve_traced). Caller must release it with auction_free_json.
/// Returns NULL on error.
extern "C" char* auction_solve_trace_json(const std::int64_t* costs, std::size_t n, int maximize,
                                          std::size_t max_events);

extern "C" void auction_free_json(char* json);
