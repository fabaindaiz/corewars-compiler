#!/usr/bin/env python3
"""Check the repository against the rules it writes down about itself.

Every check below enforces a rule written in AGENTS.md, docs/decisions.md, docs/architecture.md or
docs/semantics.md; a rule's "Enforced in" names the check by its name here. If a rule changes
there, change it here too; a check with no rule should not be failing the build.

Two severities: a failure exits 1; an advisory is printed and stops nothing. A check listed in
KNOWN_FAILING records a bug that is not fixed yet (d-7d2612-c5bb3c): it must fail, and when it
starts passing the audit fails until the entry is removed and the roadmap item moves.

Usage: python3 tools/audit.py        (Python 3.11+, standard library only)
"""
from __future__ import annotations

import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

# check name -> the roadmap item that records the bug it detects.
KNOWN_FAILING = {
    "single-error-type": "i-7d2612-888db5",
}

# Documents whose repository paths must exist (the map and everything it sends a reader to).
INSTRUCTION_DOCS = [
    "AGENTS.md",
    "docs/architecture.md",
    "docs/decisions.md",
    "docs/roadmap.md",
    "docs/semantics.md",
    ".claude/skills/verify/SKILL.md",
    ".claude/skills/run-warrior/SKILL.md",
    ".claude/skills/troubleshoot-redcode/SKILL.md",
    ".claude/skills/state-review/SKILL.md",
]
# Generated output: named on purpose, absent from a fresh clone (dune, tools/pmars-host.sh, tools/behave.py).
GENERATED_ROOT = "_build/"
# Top-level names that make a backticked token a repository path.
PATH_ROOTS = ("src/", "execs/", "bbctests/", "behtests/", "examples/", "pmars/", "tools/", "docs/",
              ".claude/", ".agents/", ".github/", GENERATED_ROOT)
ROOT_FILES = ("AGENTS.md", "CLAUDE.md", "README.md", "REFERENCE.md", "LANGUAGE.md", "TUTORIAL.md",
              "Makefile", "Dockerfile", "commands.md", "dune-project", "dune-workspace")
# Paths named on purpose although they do not exist in the tree, each with its reason.
PATH_EXEMPT = {
    ".agents/proposals/": "exists only once a proposal is written",
}

CHECKS: dict[str, object] = {}


def check(name: str):
    def wrap(fn):
        CHECKS[name] = fn
        return fn
    return wrap


def read(rel: str) -> str:
    return (ROOT / rel).read_text(encoding="utf-8")


def tracked(*patterns: str) -> list[str]:
    out = subprocess.run(["git", "ls-files", "--", *patterns], cwd=ROOT, capture_output=True, text=True)
    return out.stdout.split()


@check("claude-imports-agents")  # d-7d2612-7ea6bd
def _():
    text = read("CLAUDE.md").strip()
    return [] if text == "@AGENTS.md" else [f"CLAUDE.md must contain only `@AGENTS.md`, found {text[:40]!r}"]


@check("agents-budget")  # AGENTS.md is loaded on every request: under 200 lines
def _():
    n = len(read("AGENTS.md").splitlines())
    return [] if n <= 200 else [f"AGENTS.md has {n} lines (budget 200)"]


@check("doc-paths-exist")  # a dead pointer is worse than no pointer
def _():
    problems = []
    for doc in INSTRUCTION_DOCS:
        if not (ROOT / doc).is_file():
            problems.append(f"{doc} is in the map but does not exist")
            continue
        for token in re.findall(r"`([^`\s]+)`", read(doc)):
            token = token.rstrip(".,:;)")
            if not (token.startswith(PATH_ROOTS) or token in ROOT_FILES) or token in PATH_EXEMPT \
                    or token.startswith(GENERATED_ROOT):
                continue
            if any(ch in token for ch in "<>*{}|$"):
                continue  # a pattern or a placeholder, not a path
            if not (ROOT / token).exists():
                problems.append(f"{doc} names `{token}`, which does not exist")
    return problems


@check("enforcers-exist")  # principle: an enforcer that is cited must be real
def _():
    problems = []
    names = set(CHECKS)
    for doc in ("AGENTS.md", "docs/decisions.md"):
        for cited in re.findall(r"`tools/audit\.py` \(`([a-z-]+)`\)", read(doc)):
            if cited not in names:
                problems.append(f"{doc} cites audit check `{cited}`, which tools/audit.py does not define")
    return problems


@check("single-error-type")  # i-7d2612-888db5: one user-error exception, not four
def _():
    found = [str(p.relative_to(ROOT)) for p in sorted((ROOT / "src").glob("*.ml"))
             if re.search(r"^\s*exception\s+CTError\b", p.read_text(), re.M)]
    return [] if len(found) <= 1 else [f"`exception CTError` is declared in {len(found)} modules: {', '.join(found)}"]


@check("no-global-counter")  # d-7d2612-123e41: labels come from tags, not from a counter
def _():
    problems = []
    for p in sorted((ROOT / "src").glob("*.ml")):
        text = p.read_text()
        for pat in (r"\bgensym\b", r"^let\s+\w+\s*=\s*ref\b"):
            for m in re.finditer(pat, text, re.M):
                line = text.count("\n", 0, m.start()) + 1
                problems.append(f"{p.relative_to(ROOT)}:{line}: `{m.group(0).strip()}` (label names must derive from AST tags)")
    return problems


@check("language-documents-parser")  # LANGUAGE.md must name every keyword the parser accepts
def _():
    keywords = set(re.findall(r'`Atom "([^"]+)"', read("src/parse.ml")))
    doc = read("LANGUAGE.md")
    missing = sorted(k for k in keywords if not re.search(rf"(?<![\w-]){re.escape(k)}(?![\w-])", doc))
    return [f"LANGUAGE.md does not mention `{k}`, which src/parse.ml accepts" for k in missing]


@check("goldens-well-formed")  # docs/architecture.md: the .bbc format
def _():
    problems = []
    for p in sorted((ROOT / "bbctests").rglob("*.bbc")):
        text = p.read_text()
        order = [text.find(f"{f}:") for f in ("NAME", "DESCRIPTION", "SRC", "EXPECTED")]
        if -1 in order or order != sorted(order):
            problems.append(f"{p.relative_to(ROOT)}: needs NAME, DESCRIPTION, SRC, EXPECTED in that order")
        if text.endswith("\n"):
            problems.append(f"{p.relative_to(ROOT)}: ends with a newline the compiler does not emit")
    for p in sorted((ROOT / "bbctests" / "known-bugs").glob("*.bbc")):
        if not re.search(r"i-7d2612-[0-9a-f]{6}", p.read_text().split("\nSRC:", 1)[0]):
            problems.append(f"{p.relative_to(ROOT)}: a known-bug golden names its roadmap item in DESCRIPTION")
    return problems


@check("no-generated-output-tracked")  # emitted redcode and test output never enter git
def _():
    found = tracked("*.s", "*.run", "*.result", "behtests/*.red", "bbctests/*.red", "bbctests/**/*.red")
    return [f"{f} is generated output and is tracked" for f in found]


@check("record-ids-defined")  # every cited d-/i-/s- id is defined in decisions, roadmap or changelog
def _():
    defining = ["docs/decisions.md", "docs/roadmap.md", ".claude/logs/agent-changelog.md"]
    defined = set()
    for doc in defining:
        defined |= set(re.findall(r"^\|\s*([dis]-[0-9a-f]{6}-[0-9a-f]{6})\s*\|", read(doc), re.M))
        defined |= set(re.findall(r"^#{1,6}\s.*·\s*([dis]-[0-9a-f]{6}-[0-9a-f]{6})", read(doc), re.M))
    citing = INSTRUCTION_DOCS + ["docs/references.md", "LANGUAGE.md", "README.md", "REFERENCE.md"]
    citing += [str(p.relative_to(ROOT)) for p in (ROOT / "behtests").glob("*.beh")]
    citing += [str(p.relative_to(ROOT)) for p in (ROOT / "bbctests").rglob("*.bbc")]
    citing += ["tools/behave.py", "tools/audit.py"]
    problems = []
    for doc in citing:
        if not (ROOT / doc).is_file():
            continue
        for rid in sorted(set(re.findall(r"\b[dis]-7d2612-[0-9a-f]{6}\b", read(doc)))):
            if rid not in defined:
                problems.append(f"{doc} cites {rid}, which no decision row, roadmap heading or changelog entry defines")
    return problems


def eol_style(data: bytes) -> str:
    crlf = data.count(b"\r\n")
    lf = data.count(b"\n") - crlf
    return "none" if crlf + lf == 0 else "crlf" if lf == 0 else "lf" if crlf == 0 else "mixed"


@check("eol-preserved")  # d-7d2612-040878: an edit never rewrites a file's line endings
def _():
    problems = []
    for rel in tracked():
        path = ROOT / rel
        if not path.is_file():
            continue
        now = path.read_bytes()
        if b"\0" in now:
            continue  # binary
        head = subprocess.run(["git", "show", f"HEAD:{rel}"], cwd=ROOT, capture_output=True)
        if head.returncode != 0:
            continue  # new file: no style to keep yet
        before, after = eol_style(head.stdout), eol_style(now)
        if "none" not in (before, after) and before != after:
            problems.append(f"{rel}: line endings changed from {before} to {after} (convert back before committing)")
    return problems


@check("label-prefixes-documented")  # docs/architecture.md lists every generated label prefix
def _():
    prefixes = set(re.findall(r'sprintf "([A-Z]+)%d"', read("src/compile.ml")))
    table = read("docs/architecture.md").split("## Generated labels", 1)[-1].split("\n## ", 1)[0]
    return [f"generated label prefix `{p}` (src/compile.ml) is not in docs/architecture.md" for p in sorted(prefixes)
            if f"`{p}`" not in table]


def main() -> int:
    failures: list[str] = []
    advisories: list[str] = []
    results: dict[str, bool] = {}
    for name, fn in CHECKS.items():
        problems = fn()
        results[name] = not problems
        for p in problems:
            (advisories if name in KNOWN_FAILING else failures).append(f"{name}: {p}")
    promoted = [f"{name}: passes but is listed in KNOWN_FAILING as {item}; remove the entry and move the roadmap item"
                for name, item in KNOWN_FAILING.items() if results.get(name)]
    for name, item in KNOWN_FAILING.items():
        if not results.get(name, True):
            print(f"known-failing  {name} ({item})")
    for a in advisories:
        print(f"advisory       {a}")
    for f in failures + promoted:
        print(f"FAIL           {f}")
    print(f"audit: {len(results)} checks, {len(failures) + len(promoted)} failing, "
          f"{sum(1 for n in KNOWN_FAILING if not results.get(n, True))} known-failing")
    return 1 if failures or promoted else 0


if __name__ == "__main__":
    sys.exit(main())
