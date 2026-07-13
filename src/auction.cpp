#include "auction/auction.hpp"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <string>

namespace auction {

namespace {

constexpr std::size_t kUnassigned = std::numeric_limits<std::size_t>::max();

// Largest |cost| accepted. Prices and values are doubles; this bound keeps
// every intermediate quantity far away from the range where double rounding
// could disturb the epsilon-optimality argument.
constexpr std::int64_t kMaxAbsCost = std::int64_t{1} << 30;

void record_award(Trace* trace, std::uint64_t phase, std::uint64_t round, double epsilon,
                  std::size_t object, std::size_t winner, std::size_t displaced,
                  double price_after) {
    if (trace == nullptr) return;
    if (trace->events.size() >= trace->max_events) {
        trace->truncated = true;
        return;
    }
    trace->events.push_back({phase, round, epsilon, object, winner, displaced, price_after});
}

// One epsilon phase of the Jacobi auction: starting from an empty assignment
// (but keeping the prices learned by earlier phases), rounds of simultaneous
// bidding run until every person is assigned. Returns the number of rounds.
std::uint64_t run_phase(const std::vector<double>& benefits, std::size_t n, double epsilon,
                        std::vector<double>& prices, std::vector<std::size_t>& assignment,
                        Trace* trace, std::uint64_t phase_number) {
    constexpr double kNegInf = -std::numeric_limits<double>::infinity();

    std::fill(assignment.begin(), assignment.end(), kUnassigned);
    std::vector<std::size_t> owner(n, kUnassigned);

    std::vector<std::size_t> unassigned(n);
    for (std::size_t i = 0; i < n; ++i) unassigned[i] = i;

    std::vector<double> best_bid(n);
    std::vector<std::size_t> best_bidder(n);

    std::uint64_t rounds = 0;
    while (!unassigned.empty()) {
        ++rounds;
        std::fill(best_bidder.begin(), best_bidder.end(), kUnassigned);

        // Bidding: every unassigned person bids on their most valuable
        // object, offering the best/second-best value gap plus epsilon.
        for (std::size_t i : unassigned) {
            double best = kNegInf;
            double second = kNegInf;
            std::size_t best_j = 0;
            const double* row = benefits.data() + i * n;
            for (std::size_t j = 0; j < n; ++j) {
                const double value = row[j] - prices[j];
                if (value > best) {
                    second = best;
                    best = value;
                    best_j = j;
                } else if (value > second) {
                    second = value;
                }
            }
            // With a single object there is no second-best; any positive
            // increment is valid since there is no competition to outbid.
            const double increment = (second == kNegInf) ? 1.0 + epsilon : best - second + epsilon;

            if (best_bidder[best_j] == kUnassigned || increment > best_bid[best_j]) {
                best_bidder[best_j] = i;
                best_bid[best_j] = increment;
            }
        }

        // Awarding: each object that received bids goes to its highest
        // bidder; the displaced previous owner rejoins the unassigned pool.
        std::vector<std::size_t> displaced;
        for (std::size_t j = 0; j < n; ++j) {
            const std::size_t winner = best_bidder[j];
            if (winner == kUnassigned) continue;
            prices[j] += best_bid[j];
            const std::size_t prev = owner[j];
            if (prev != kUnassigned) {
                assignment[prev] = kUnassigned;
                displaced.push_back(prev);
            }
            owner[j] = winner;
            assignment[winner] = j;
            record_award(trace, phase_number, rounds, epsilon, j, winner,
                         prev == kUnassigned ? kNoPerson : prev, prices[j]);
        }

        // Next round's bidders: this round's losers plus the displaced.
        std::vector<std::size_t> next_unassigned;
        next_unassigned.reserve(unassigned.size());
        for (std::size_t i : unassigned) {
            if (assignment[i] == kUnassigned) next_unassigned.push_back(i);
        }
        next_unassigned.insert(next_unassigned.end(), displaced.begin(), displaced.end());
        unassigned = std::move(next_unassigned);
    }
    return rounds;
}

Result solve_impl(const std::vector<std::int64_t>& costs, std::size_t n, const Options& options,
                  Trace* trace) {
    if (n == 0) throw std::invalid_argument("auction::solve: n must be >= 1");
    if (costs.size() != n * n) {
        throw std::invalid_argument("auction::solve: costs.size() must equal n * n");
    }
    if (!(options.scaling_factor > 1.0)) {
        throw std::invalid_argument("auction::solve: scaling_factor must be > 1");
    }
    if (!std::isfinite(options.epsilon_init)) {
        throw std::invalid_argument("auction::solve: epsilon_init must be finite");
    }

    std::int64_t max_abs = 0;
    for (const std::int64_t c : costs) {
        max_abs = std::max<std::int64_t>(max_abs, std::abs(c));
    }
    if (max_abs > kMaxAbsCost) {
        throw std::invalid_argument("auction::solve: |cost| must be <= 2^30");
    }

    // The auction maximizes total benefit; minimization negates the costs.
    const bool maximize = options.objective == Objective::Maximize;
    std::vector<double> benefits(costs.size());
    for (std::size_t k = 0; k < costs.size(); ++k) {
        benefits[k] = maximize ? static_cast<double>(costs[k]) : -static_cast<double>(costs[k]);
    }

    Result result;
    result.prices.assign(n, 0.0);
    result.assignment.assign(n, kUnassigned);

    // Epsilon scaling. Each phase reuses the prices of the previous one.
    // For integer costs the assignment is exactly optimal once a full phase
    // has run with n * epsilon < 1, so the loop breaks only after a phase
    // at epsilon < 1/n has completed. (The original implementation exited
    // before running that final phase, yielding suboptimal assignments.)
    double epsilon = options.epsilon_init > 0 ? options.epsilon_init
                                              : std::max(1.0, static_cast<double>(max_abs) / 2.0);
    const double threshold = 1.0 / static_cast<double>(n);
    while (true) {
        result.rounds += run_phase(benefits, n, epsilon, result.prices, result.assignment, trace,
                                   result.phases + 1);
        ++result.phases;
        if (epsilon < threshold) break;
        epsilon /= options.scaling_factor;
    }

    result.total_cost = 0;
    for (std::size_t i = 0; i < n; ++i) {
        result.total_cost += costs[i * n + result.assignment[i]];
    }
    return result;
}

}  // namespace

Result solve(const std::vector<std::int64_t>& costs, std::size_t n, const Options& options) {
    return solve_impl(costs, n, options, nullptr);
}

Result solve_traced(const std::vector<std::int64_t>& costs, std::size_t n, const Options& options,
                    Trace& trace) {
    trace.events.clear();
    trace.truncated = false;
    return solve_impl(costs, n, options, &trace);
}

std::string result_to_json(const Result& result, Objective objective, const Trace* trace) {
    std::ostringstream json;
    json << "{\"n\": " << result.assignment.size() << ", \"objective\": \""
         << (objective == Objective::Maximize ? "max" : "min")
         << "\", \"total_cost\": " << result.total_cost << ", \"phases\": " << result.phases
         << ", \"rounds\": " << result.rounds << ", \"assignment\": [";
    for (std::size_t i = 0; i < result.assignment.size(); ++i) {
        if (i > 0) json << ", ";
        json << result.assignment[i];
    }
    json << "]";
    if (trace != nullptr) {
        json << ", \"truncated\": " << (trace->truncated ? "true" : "false") << ", \"events\": [";
        for (std::size_t k = 0; k < trace->events.size(); ++k) {
            const TraceEvent& e = trace->events[k];
            if (k > 0) json << ", ";
            json << "{\"phase\": " << e.phase << ", \"round\": " << e.round
                 << ", \"epsilon\": " << e.epsilon << ", \"object\": " << e.object
                 << ", \"winner\": " << e.winner << ", \"displaced\": "
                 << (e.displaced == kNoPerson ? std::string("-1") : std::to_string(e.displaced))
                 << ", \"price\": " << e.price_after << "}";
        }
        json << "]";
    }
    json << "}";
    return json.str();
}

}  // namespace auction

extern "C" int auction_solve(const std::int64_t* costs, std::size_t n, int maximize,
                             std::size_t* assignment_out, std::int64_t* total_cost_out) {
    if (costs == nullptr || assignment_out == nullptr || total_cost_out == nullptr || n == 0) {
        return 1;
    }
    try {
        auction::Options options;
        options.objective = maximize ? auction::Objective::Maximize : auction::Objective::Minimize;
        const std::vector<std::int64_t> cost_vec(costs, costs + n * n);
        const auction::Result result = auction::solve(cost_vec, n, options);
        std::copy(result.assignment.begin(), result.assignment.end(), assignment_out);
        *total_cost_out = result.total_cost;
        return 0;
    } catch (const std::invalid_argument&) {
        return 2;
    } catch (...) {
        return 3;
    }
}

extern "C" char* auction_solve_trace_json(const std::int64_t* costs, std::size_t n, int maximize,
                                          std::size_t max_events) {
    if (costs == nullptr || n == 0) return nullptr;
    try {
        auction::Options options;
        options.objective = maximize ? auction::Objective::Maximize : auction::Objective::Minimize;
        const std::vector<std::int64_t> cost_vec(costs, costs + n * n);
        auction::Trace trace;
        if (max_events > 0) trace.max_events = max_events;
        const auction::Result result = auction::solve_traced(cost_vec, n, options, trace);
        const std::string text = auction::result_to_json(result, options.objective, &trace);
        char* out = static_cast<char*>(std::malloc(text.size() + 1));
        if (out == nullptr) return nullptr;
        std::memcpy(out, text.c_str(), text.size() + 1);
        return out;
    } catch (...) {
        return nullptr;
    }
}

extern "C" void auction_free_json(char* json) { std::free(json); }
