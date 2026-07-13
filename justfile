set shell := ["bash", "-cu"]

cpp_sources := "include/auction/*.hpp src/*.cpp apps/*.cpp tests/test_*.cpp"

# clang-format from PATH, falling back to the Homebrew keg-only install.
clang_format := `command -v clang-format || echo /home/linuxbrew/.linuxbrew/opt/clang-format/bin/clang-format`

# List available recipes.
default:
    @just --list

# Configure and build the C++ project (Release).
build:
    cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
    cmake --build build -j

# Run the C++ test suite via ctest.
test: build
    ctest --test-dir build --output-on-failure

# Run the Python evals/benchmarks test suite.
test-py: build
    cd evals && uv sync --dev && uv run pytest

# Run both the C++ and Python test suites.
test-all: test test-py

# Run the benchmark suite (pass-through args, e.g. `just bench --quick`).
bench *ARGS: build
    cd evals && uv run auction-bench {{ ARGS }}

# Format C++ sources and Python evals code in place.
fmt:
    {{ clang_format }} -i {{ cpp_sources }}
    cd evals && uv run ruff format .

# Check formatting without modifying files.
format-check:
    {{ clang_format }} --dry-run -Werror {{ cpp_sources }}
    cd evals && uv run ruff format --check .

# Run all linters (format check + ruff check + ty).
lint: format-check
    cd evals && uv run ruff check . && uv run ty check .

# Format, autofix lint issues, then run the full test suite. Run before committing.
fc:
    just fmt
    cd evals && uv run ruff check --fix .
    just test-all

# Run everything CI runs.
ci: format-check lint test-all

# Build the WebAssembly module for the demo site (requires docker).
wasm:
    mkdir -p site/wasm
    docker run --rm -v {{ justfile_directory() }}:/src -w /src -u $(id -u):$(id -g) \
        emscripten/emsdk:3.1.61 emcc src/auction.cpp src/problem.cpp \
        -O3 -std=c++17 -Iinclude -fwasm-exceptions \
        -sMODULARIZE=1 -sEXPORT_NAME=createAuctionModule -sWASM_BIGINT -sALLOW_MEMORY_GROWTH=1 \
        -sEXPORTED_FUNCTIONS=_auction_solve,_auction_solve_trace_json,_auction_solve_problem_text_json,_auction_free_json,_malloc,_free \
        -sEXPORTED_RUNTIME_METHODS=cwrap,UTF8ToString,stringToNewUTF8,HEAP64,HEAPU32 \
        -o site/wasm/auction.js

# Remove build artifacts.
clean:
    rm -rf build

# Install the library and CLI to the given prefix.
install prefix="/usr/local":
    cmake --install build --prefix {{ prefix }}
