"""Helpers for driving the ``auction`` CLI binary as a subprocess."""

from __future__ import annotations

import json
import os
import subprocess
from pathlib import Path
from typing import Any

import numpy as np

# evals/src/auction_evals/cli.py -> auction_evals -> src -> evals -> repo_root
_REPO_ROOT = Path(__file__).resolve().parents[3]


def build_dir() -> Path:
    """Return the directory expected to contain the built C++ artifacts."""
    override = os.environ.get("AUCTION_BUILD_DIR")
    if override:
        return Path(override)
    return _REPO_ROOT / "build"


def binary_path() -> Path:
    """Return the path to the auction CLI binary."""
    return build_dir() / "auction"


def format_problem(costs: np.ndarray, objective: str = "min") -> str:
    """Render a cost matrix into the auction text problem format.

    Args:
        costs: an (n, n) integer ndarray.
        objective: "min" or "max".

    Returns:
        Text in the problem file format understood by ``auction solve``.
    """
    if objective not in ("min", "max"):
        raise ValueError(f"objective must be 'min' or 'max', got {objective!r}")
    if costs.ndim != 2 or costs.shape[0] != costs.shape[1]:
        raise ValueError(f"costs must be a square 2-D array, got shape {costs.shape}")

    n = costs.shape[0]
    lines = [f"objective {objective}", f"n {n}"]
    for i in range(n):
        lines.append(" ".join(str(int(c)) for c in costs[i]))
    return "\n".join(lines) + "\n"


def run_cli(args: list[str], stdin_text: str | None = None) -> subprocess.CompletedProcess[str]:
    """Run the auction CLI binary with the given arguments.

    Args:
        args: CLI arguments, e.g. ["solve", "-", "--json"].
        stdin_text: text to feed on stdin, or None.

    Returns:
        The completed subprocess.CompletedProcess (not checked for exit code).
    """
    binary = binary_path()
    if not binary.exists():
        raise RuntimeError(
            f"auction CLI binary not found at {binary}. "
            "Build the C++ project or set AUCTION_BUILD_DIR."
        )
    return subprocess.run(
        [str(binary), *args],
        input=stdin_text,
        capture_output=True,
        text=True,
    )


def _parse_plain_output(text: str) -> dict[str, Any]:
    """Parse the non-JSON `auction solve` text output into an equivalent dict."""
    result: dict[str, Any] = {}
    assignment: dict[int, int] = {}
    for line in text.splitlines():
        line = line.strip()
        if not line or line == "assignment:":
            continue
        if "->" in line:
            person, obj = line.split("->")
            assignment[int(person.strip())] = int(obj.strip())
            continue
        key, _, value = line.partition(":")
        key = key.strip()
        value = value.strip()
        if key in ("n", "total_cost", "phases", "rounds"):
            result[key] = int(value)
        elif key == "objective":
            result[key] = value
    result["assignment"] = [assignment[i] for i in range(len(assignment))]
    return result


def solve_file(text: str, json_output: bool = True) -> dict[str, Any]:
    """Solve a problem given as problem-file text via the CLI, over stdin.

    Args:
        text: problem file text.
        json_output: if True, pass --json and parse stdout as JSON; otherwise
            pass no --json flag and parse the plain-text output into an
            equivalent dict.

    Returns:
        A dict with keys n, objective, total_cost, phases, rounds, assignment.

    Raises:
        RuntimeError: if the CLI exits nonzero.
    """
    args = ["solve", "-"]
    if json_output:
        args.append("--json")
    result = run_cli(args, stdin_text=text)
    if result.returncode != 0:
        raise RuntimeError(
            f"auction solve failed with exit code {result.returncode}: {result.stderr.strip()}"
        )
    if json_output:
        return json.loads(result.stdout)
    return _parse_plain_output(result.stdout)
