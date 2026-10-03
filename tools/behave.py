#!/usr/bin/env python3
"""Run the behaviour specs in behtests/: what compiled redcode DOES, not what it looks like.

The golden tests in bbctests/ compare emitted text, and the `execute` suite only asks pMARS to
assemble it (`-A`). Neither notices redcode that assembles, matches its golden and still behaves
wrongly in the core. This runner executes each spec's redcode alone in pMARS's cdb debugger and
checks probes against the behaviour the RED source means (docs/semantics.md).

A spec (`behtests/<name>.beh`), one directive per line; a line starting with `#` is a comment
(only whole lines: `#` also marks immediate operands in a cell's TEXT):

    golden: bbctests/examples/prog8.bbc   the redcode to run: that golden's EXPECTED section
    known-failing: i-7d2612-fffa6c        optional: a recorded bug, by its roadmap id
    alive N                               a process is still running after N executed instructions
    dead N                                no process is left after N executed instructions
    cell N ADDR TEXT                      after N instructions, core cell ADDR disassembles to TEXT
                                          (whitespace-insensitive, e.g. `DAT.F #10, #10`)

The redcode comes from the golden, not from a fresh compile, so this runs without the OCaml
toolchain; the `compare` suite is what ties each golden to the compiler (d-7d2612-b92028).

A single warrior loads at address 0 and the run is deterministic. `skip K` in cdb executes K+1
instructions, which is why a probe at N sends `skip N-1`.

A spec marked known-failing must fail: if it passes, the bug is fixed and the mark (and the roadmap
item's state) must move in the same change, so this exits non-zero (d-7d2612-c5bb3c).

Usage: python3 tools/behave.py [SPEC ...]      default: every behtests/*.beh
pMARS: $PMARS if set, else _build/pmars-host/pmars, built on demand by tools/pmars-host.sh.
"""
from __future__ import annotations

import os
import re
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CONFIG = ROOT / "pmars" / "config" / "94b.opt"  # d-7d2612-6d88cd: the test target is 94b
WORK = ROOT / "_build" / "behave"
CALC_LINE = re.compile(r"^\(cdb\) (\d+)\s*$")


@dataclass
class Spec:
    path: Path
    golden: Path | None = None
    known_failing: str | None = None
    probes: list[tuple[str, list[str]]] = field(default_factory=list)


def parse_spec(path: Path) -> Spec:
    spec = Spec(path)
    for n, raw in enumerate(path.read_text().splitlines(), 1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        if line.startswith("golden:"):
            spec.golden = ROOT / line.split(":", 1)[1].strip()
        elif line.startswith("known-failing:"):
            spec.known_failing = line.split(":", 1)[1].strip()
        else:
            word, *args = line.split(None, 3 if line.startswith("cell") else 1)
            if word not in ("alive", "dead", "cell") or not args or not args[0].isdigit() or int(args[0]) < 1:
                raise SystemExit(f"{path}:{n}: not a probe: {raw!r}")
            spec.probes.append((word, args))
    if spec.golden is None or not spec.golden.is_file():
        raise SystemExit(f"{path}: `golden:` missing or not a file")
    if not spec.probes:
        raise SystemExit(f"{path}: no probes")
    return spec


def expected_redcode(golden: Path) -> str:
    text = golden.read_text()
    if "\nEXPECTED:\n" not in text:
        raise SystemExit(f"{golden}: no EXPECTED section")
    return text.split("\nEXPECTED:\n", 1)[1].split("\nEND", 1)[0] + "\n"


def pmars_binary() -> str:
    if os.environ.get("PMARS"):
        return os.environ["PMARS"]
    built = ROOT / "_build" / "pmars-host" / "pmars"
    if not built.exists():
        subprocess.run([str(ROOT / "tools" / "pmars-host.sh")], check=True, stdout=subprocess.DEVNULL)
    return str(built)


def cdb(pmars: str, warrior: Path, commands: str) -> list[str]:
    run = subprocess.run([pmars, "-@", str(CONFIG), "-e", "-b", str(warrior)],
                         input=commands + "quit\n", capture_output=True, text=True, timeout=60)
    if run.returncode not in (0, 4):  # 4: cdb `quit`; 3 would be an assembly error
        raise RuntimeError(f"pmars exited {run.returncode}: {run.stderr.strip() or run.stdout.strip()}")
    return run.stdout.splitlines()


def squash(text: str) -> str:
    return re.sub(r"\s+", "", text)


def check_probe(pmars: str, warrior: Path, word: str, args: list[str]) -> str | None:
    """None when the probe holds, else what was seen instead."""
    n = int(args[0])
    if word in ("alive", "dead"):
        out = cdb(pmars, warrior, f"skip {n - 1}\ncalc CYCLE\n")
        alive = any(CALC_LINE.match(line) for line in out)
        if alive == (word == "alive"):
            return None
        return f"{'dead' if not alive else 'still running'} after {n} instructions"
    if len(args) < 3:
        return "cell needs N ADDR TEXT"
    addr, want = int(args[1]), args[2]
    out = cdb(pmars, warrior, f"skip {n - 1}\nlist {addr}\n")
    cell = re.compile(rf"^(?:\(cdb\) )?0*{addr}\s+(\S.*)$")
    for line in out:
        m = cell.match(line)
        if m and not CALC_LINE.match(line):
            got = m.group(1).strip()
            return None if squash(got) == squash(want) else f"cell {addr} is `{got}`"
    return f"no listing of cell {addr} (the warrior may be dead after {n} instructions)"


def run_spec(pmars: str, spec: Spec) -> list[str]:
    WORK.mkdir(parents=True, exist_ok=True)
    warrior = WORK / (spec.path.stem + ".red")
    warrior.write_text(expected_redcode(spec.golden))
    failures = []
    for word, args in spec.probes:
        try:
            seen = check_probe(pmars, warrior, word, args)
        except (RuntimeError, subprocess.TimeoutExpired) as err:
            seen = str(err)
        if seen:
            failures.append(f"{word} {' '.join(args)}: {seen}")
    return failures


def main(argv: list[str]) -> int:
    paths = [Path(p) for p in argv] or sorted((ROOT / "behtests").glob("*.beh"))
    if not paths:
        print("behave: no specs found")
        return 1
    pmars = pmars_binary()
    unexpected = 0
    for path in paths:
        spec = parse_spec(path)
        failures = run_spec(pmars, spec)
        name = path.stem
        if spec.known_failing and failures:
            print(f"known-failing  {name}  ({spec.known_failing}): {failures[0]}")
        elif spec.known_failing:
            unexpected += 1
            print(f"FIXED?         {name}: passes but is marked known-failing {spec.known_failing}; "
                  "remove the mark and move the roadmap item in the same change")
        elif failures:
            unexpected += 1
            print(f"FAIL           {name}")
            for f in failures:
                print(f"                 {f}")
        else:
            print(f"ok             {name}  ({len(spec.probes)} probes)")
    print(f"behave: {len(paths)} specs, {unexpected} unexpected")
    return 1 if unexpected else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
