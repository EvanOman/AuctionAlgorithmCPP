#include "auction/problem.hpp"

#include <cctype>
#include <cstdlib>
#include <cstring>
#include <sstream>
#include <stdexcept>

namespace auction {

namespace {

[[noreturn]] void fail(std::size_t line_number, const std::string& message) {
    throw std::runtime_error("line " + std::to_string(line_number) + ": " + message);
}

bool is_blank_or_comment(const std::string& line) {
    for (const char c : line) {
        if (c == '#') return true;
        if (!std::isspace(static_cast<unsigned char>(c))) return false;
    }
    return true;
}

}  // namespace

Problem parse_problem(std::istream& in) {
    Problem problem;
    bool have_n = false;
    bool have_objective = false;
    std::size_t rows_read = 0;

    std::string line;
    std::size_t line_number = 0;
    while (std::getline(in, line)) {
        ++line_number;
        if (is_blank_or_comment(line)) continue;

        std::istringstream tokens(line);
        std::string first;
        tokens >> first;

        if (first == "objective") {
            if (have_objective) fail(line_number, "duplicate 'objective' line");
            if (have_n && rows_read > 0)
                fail(line_number, "'objective' must come before matrix rows");
            std::string direction;
            if (!(tokens >> direction))
                fail(line_number, "expected 'min' or 'max' after 'objective'");
            if (direction == "min") {
                problem.objective = Objective::Minimize;
            } else if (direction == "max") {
                problem.objective = Objective::Maximize;
            } else {
                fail(line_number, "objective must be 'min' or 'max', got '" + direction + "'");
            }
            have_objective = true;
            continue;
        }

        if (first == "n") {
            if (have_n) fail(line_number, "duplicate 'n' line");
            long long n_value = 0;
            if (!(tokens >> n_value) || n_value < 1) {
                fail(line_number, "expected a positive integer after 'n'");
            }
            problem.n = static_cast<std::size_t>(n_value);
            problem.costs.reserve(problem.n * problem.n);
            have_n = true;
            continue;
        }

        // Anything else must be a matrix row.
        if (!have_n) fail(line_number, "expected 'n <size>' before matrix rows");
        if (rows_read == problem.n) fail(line_number, "more matrix rows than n");

        std::istringstream row(line);
        for (std::size_t j = 0; j < problem.n; ++j) {
            std::int64_t cost = 0;
            if (!(row >> cost)) {
                fail(line_number, "expected " + std::to_string(problem.n) +
                                      " integers in matrix row, got " + std::to_string(j));
            }
            problem.costs.push_back(cost);
        }
        std::string extra;
        if (row >> extra) {
            if (extra[0] != '#') fail(line_number, "unexpected trailing token '" + extra + "'");
        }
        ++rows_read;
    }

    if (!have_n) fail(line_number, "missing 'n <size>' line");
    if (rows_read != problem.n) {
        fail(line_number, "expected " + std::to_string(problem.n) + " matrix rows, got " +
                              std::to_string(rows_read));
    }
    return problem;
}

namespace {

std::string json_escape(const std::string& text) {
    std::string out;
    out.reserve(text.size());
    for (const char c : text) {
        if (c == '"' || c == '\\') {
            out += '\\';
            out += c;
        } else if (c == '\n') {
            out += "\\n";
        } else if (static_cast<unsigned char>(c) < 0x20) {
            out += ' ';
        } else {
            out += c;
        }
    }
    return out;
}

}  // namespace

std::string format_problem(const Problem& problem) {
    std::ostringstream out;
    out << "objective " << (problem.objective == Objective::Maximize ? "max" : "min") << "\n";
    out << "n " << problem.n << "\n";
    for (std::size_t i = 0; i < problem.n; ++i) {
        for (std::size_t j = 0; j < problem.n; ++j) {
            if (j > 0) out << ' ';
            out << problem.costs[i * problem.n + j];
        }
        out << "\n";
    }
    return out.str();
}

}  // namespace auction

extern "C" char* auction_solve_problem_text_json(const char* text, std::size_t max_events) {
    std::string body;
    try {
        if (text == nullptr) {
            body = "{\"error\": \"null input\"}";
        } else {
            std::istringstream in(text);
            const auction::Problem problem = auction::parse_problem(in);
            auction::Options options;
            options.objective = problem.objective;
            auction::Trace trace;
            if (max_events > 0) trace.max_events = max_events;
            const auction::Result result =
                auction::solve_traced(problem.costs, problem.n, options, trace);
            body = auction::result_to_json(result, problem.objective, &trace);
        }
    } catch (const std::exception& e) {
        body = std::string("{\"error\": \"") + auction::json_escape(e.what()) + "\"}";
    } catch (...) {
        body = "{\"error\": \"internal failure\"}";
    }
    char* out = static_cast<char*>(std::malloc(body.size() + 1));
    if (out == nullptr) return nullptr;
    std::memcpy(out, body.c_str(), body.size() + 1);
    return out;
}
