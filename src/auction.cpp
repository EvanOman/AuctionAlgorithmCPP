#include "auction/auction.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace auction {

namespace {

constexpr std::size_t kUnassigned = std::numeric_limits<std::size_t>::max();

// Largest |cost| accepted. Prices and values are doubles; this bound keeps
// every intermediate quantity far away from the range where double rounding
// could disturb the epsilon-optimality argument.
constexpr std::int64_t kMaxAbsCost = std::int64_t{1} << 30;

// One epsilon phase of the Jacobi auction: starting from an empty assignment
// (but keeping the prices learned by earlier phases), rounds of simultaneous
// bidding run until every person is assigned. Returns the number of rounds.
std::uint64_t run_phase(const std::vector<double>& benefits, std::size_t n, double epsilon,
                        std::vector<double>& prices, std::vector<std::size_t>& assignment) {
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

}  // namespace

Result solve(const std::vector<std::int64_t>& costs, std::size_t n, const Options& options) {
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
        result.rounds += run_phase(benefits, n, epsilon, result.prices, result.assignment);
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
