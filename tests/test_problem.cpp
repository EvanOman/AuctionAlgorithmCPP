#include <sstream>
#include <stdexcept>
#include <vector>

#include "auction/problem.hpp"
#include "doctest.h"

namespace {

auction::Problem parse(const std::string& text) {
    std::istringstream in(text);
    return auction::parse_problem(in);
}

}  // namespace

TEST_CASE("parses a minimal problem") {
    const auction::Problem p = parse("n 2\n1 2\n3 4\n");
    CHECK(p.n == 2);
    CHECK(p.objective == auction::Objective::Minimize);
    CHECK(p.costs == std::vector<std::int64_t>{1, 2, 3, 4});
}

TEST_CASE("parses objective, comments, blank lines, negative costs") {
    const auction::Problem p = parse(
        "# a comment\n"
        "\n"
        "objective max\n"
        "n 2\n"
        "-1 2   # trailing comment\n"
        "  30 -4\n");
    CHECK(p.objective == auction::Objective::Maximize);
    CHECK(p.costs == std::vector<std::int64_t>{-1, 2, 30, -4});
}

TEST_CASE("format_problem round-trips") {
    auction::Problem p;
    p.n = 2;
    p.objective = auction::Objective::Maximize;
    p.costs = {5, -3, 0, 9};
    const auction::Problem back = parse(auction::format_problem(p));
    CHECK(back.n == p.n);
    CHECK(back.objective == p.objective);
    CHECK(back.costs == p.costs);
}

TEST_CASE("rejects malformed input") {
    CHECK_THROWS_AS(parse(""), std::runtime_error);                      // empty
    CHECK_THROWS_AS(parse("1 2\n3 4\n"), std::runtime_error);            // no n
    CHECK_THROWS_AS(parse("n 2\n1 2\n"), std::runtime_error);            // too few rows
    CHECK_THROWS_AS(parse("n 2\n1 2\n3 4\n5 6\n"), std::runtime_error);  // too many rows
    CHECK_THROWS_AS(parse("n 2\n1\n3 4\n"), std::runtime_error);         // short row
    CHECK_THROWS_AS(parse("n 2\n1 2 9\n3 4\n"), std::runtime_error);     // long row
    CHECK_THROWS_AS(parse("n 0\n"), std::runtime_error);                 // bad n
    CHECK_THROWS_AS(parse("n -3\n"), std::runtime_error);                // bad n
    CHECK_THROWS_AS(parse("n two\n"), std::runtime_error);               // bad n
    CHECK_THROWS_AS(parse("objective sideways\nn 1\n1\n"), std::runtime_error);
    CHECK_THROWS_AS(parse("n 1\nobjective max\nobjective min\n1\n"), std::runtime_error);
    CHECK_THROWS_AS(parse("n 1\n1 x\n"), std::runtime_error);  // trailing junk
}

TEST_CASE("error messages carry line numbers") {
    try {
        parse("n 2\n1 2\nbogus row\n");
        FAIL("expected exception");
    } catch (const std::runtime_error& e) {
        CHECK(std::string(e.what()).find("line 3") != std::string::npos);
    }
}
