#include <cstdint>
#include <fstream>
#include <iostream>
#include <optional>
#include <random>
#include <string>
#include <vector>

#include "auction/auction.hpp"
#include "auction/problem.hpp"

namespace {

int usage(std::ostream& out, int exit_code) {
    out << "auction - solve n-by-n assignment problems (Bertsekas auction algorithm)\n"
           "\n"
           "Usage:\n"
           "  auction solve [FILE] [--json]     Solve a problem file (stdin if FILE is\n"
           "                                    omitted or '-').\n"
           "  auction generate N [options]      Emit a random N-by-N problem file.\n"
           "    --lo A          minimum cost (default 1)\n"
           "    --hi B          maximum cost (default N)\n"
           "    --seed S        RNG seed (default 12345)\n"
           "    --objective D   'min' (default) or 'max'\n"
           "  auction --help\n"
           "\n"
           "Problem file format:\n"
           "  # comment lines and blank lines are ignored\n"
           "  objective min    # optional: min (default) or max\n"
           "  n 3              # problem size, before the matrix\n"
           "  4 1 3            # n rows of n integer costs;\n"
           "  2 0 5            # row i, column j = cost of object j for person i\n"
           "  3 2 2\n";
    return exit_code;
}

int solve_command(const std::vector<std::string>& args) {
    bool json = false;
    std::optional<std::string> path;
    for (const std::string& arg : args) {
        if (arg == "--json") {
            json = true;
        } else if (!path) {
            path = arg;
        } else {
            std::cerr << "error: unexpected argument '" << arg << "'\n";
            return 2;
        }
    }

    auction::Problem problem;
    try {
        if (!path || *path == "-") {
            problem = auction::parse_problem(std::cin);
        } else {
            std::ifstream file(*path);
            if (!file) {
                std::cerr << "error: cannot open '" << *path << "'\n";
                return 2;
            }
            problem = auction::parse_problem(file);
        }
    } catch (const std::exception& e) {
        std::cerr << "error: invalid problem file: " << e.what() << "\n";
        return 2;
    }

    auction::Options options;
    options.objective = problem.objective;
    auction::Result result;
    try {
        result = auction::solve(problem.costs, problem.n, options);
    } catch (const std::exception& e) {
        std::cerr << "error: " << e.what() << "\n";
        return 1;
    }

    const char* objective_name = problem.objective == auction::Objective::Maximize ? "max" : "min";
    if (json) {
        std::cout << "{\"n\": " << problem.n << ", \"objective\": \"" << objective_name
                  << "\", \"total_cost\": " << result.total_cost
                  << ", \"phases\": " << result.phases << ", \"rounds\": " << result.rounds
                  << ", \"assignment\": [";
        for (std::size_t i = 0; i < problem.n; ++i) {
            if (i > 0) std::cout << ", ";
            std::cout << result.assignment[i];
        }
        std::cout << "]}\n";
    } else {
        std::cout << "objective: " << objective_name << "\n"
                  << "n: " << problem.n << "\n"
                  << "total_cost: " << result.total_cost << "\n"
                  << "phases: " << result.phases << "\n"
                  << "rounds: " << result.rounds << "\n"
                  << "assignment:\n";
        for (std::size_t i = 0; i < problem.n; ++i) {
            std::cout << "  " << i << " -> " << result.assignment[i] << "\n";
        }
    }
    return 0;
}

int generate_command(const std::vector<std::string>& args) {
    if (args.empty()) {
        std::cerr << "error: generate requires a size N\n";
        return 2;
    }
    long long n = 0;
    try {
        n = std::stoll(args[0]);
    } catch (const std::exception&) {
        n = 0;
    }
    if (n < 1) {
        std::cerr << "error: N must be a positive integer\n";
        return 2;
    }

    std::int64_t lo = 1;
    std::int64_t hi = n;
    std::uint64_t seed = 12345;
    auction::Objective objective = auction::Objective::Minimize;
    for (std::size_t k = 1; k < args.size(); ++k) {
        const std::string& arg = args[k];
        auto next_value = [&](const char* flag) -> std::optional<std::string> {
            if (k + 1 >= args.size()) {
                std::cerr << "error: " << flag << " requires a value\n";
                return std::nullopt;
            }
            return args[++k];
        };
        try {
            if (arg == "--lo") {
                auto v = next_value("--lo");
                if (!v) return 2;
                lo = std::stoll(*v);
            } else if (arg == "--hi") {
                auto v = next_value("--hi");
                if (!v) return 2;
                hi = std::stoll(*v);
            } else if (arg == "--seed") {
                auto v = next_value("--seed");
                if (!v) return 2;
                seed = std::stoull(*v);
            } else if (arg == "--objective") {
                auto v = next_value("--objective");
                if (!v) return 2;
                if (*v == "min") {
                    objective = auction::Objective::Minimize;
                } else if (*v == "max") {
                    objective = auction::Objective::Maximize;
                } else {
                    std::cerr << "error: --objective must be 'min' or 'max'\n";
                    return 2;
                }
            } else {
                std::cerr << "error: unknown option '" << arg << "'\n";
                return 2;
            }
        } catch (const std::exception&) {
            std::cerr << "error: invalid value for " << arg << "\n";
            return 2;
        }
    }
    if (lo > hi) {
        std::cerr << "error: --lo must be <= --hi\n";
        return 2;
    }

    auction::Problem problem;
    problem.n = static_cast<std::size_t>(n);
    problem.objective = objective;
    problem.costs.resize(problem.n * problem.n);
    std::mt19937_64 rng(seed);
    std::uniform_int_distribution<std::int64_t> dist(lo, hi);
    for (std::int64_t& cost : problem.costs) cost = dist(rng);

    std::cout << "# random assignment problem: n=" << n << " costs in [" << lo << ", " << hi
              << "] seed=" << seed << "\n"
              << auction::format_problem(problem);
    return 0;
}

}  // namespace

int main(int argc, char** argv) {
    std::vector<std::string> args(argv + 1, argv + argc);
    if (args.empty()) return usage(std::cerr, 2);
    if (args[0] == "--help" || args[0] == "-h" || args[0] == "help") return usage(std::cout, 0);

    const std::string command = args[0];
    const std::vector<std::string> rest(args.begin() + 1, args.end());
    if (command == "solve") return solve_command(rest);
    if (command == "generate") return generate_command(rest);

    std::cerr << "error: unknown command '" << command << "'\n\n";
    return usage(std::cerr, 2);
}
