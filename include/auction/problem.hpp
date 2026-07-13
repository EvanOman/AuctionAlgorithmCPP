#pragma once

#include <cstdint>
#include <istream>
#include <string>
#include <vector>

#include "auction/auction.hpp"

namespace auction {

/// An assignment problem parsed from the text problem format.
struct Problem {
    std::size_t n = 0;
    Objective objective = Objective::Minimize;
    std::vector<std::int64_t> costs;  // row-major n x n
};

/// Parse the auction problem text format:
///
///   # Blank lines and lines starting with '#' are ignored.
///   objective min        # optional: "min" (default) or "max"
///   n 3                  # required, before the matrix rows
///   4 1 3                # then n rows of n integer costs
///   2 0 5                # row i, column j = cost of object j for person i
///   3 2 2
///
/// Throws std::runtime_error with a line number on malformed input.
Problem parse_problem(std::istream& in);

/// Serialize a problem in the same text format.
std::string format_problem(const Problem& problem);

}  // namespace auction
