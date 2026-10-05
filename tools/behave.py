#!/usr/bin/env python3
"""Run the behaviour specs in behtests/: what compiled redcode DOES, not what it looks like.

The golden tests in bbctests/ compare emitted text, and the `execute` suite only asks pMARS to
assemble it (`-A`). Neither notices redcode that assembles, matches its golden and still behaves
wrongly in the core. This runner executes each spec's redcode alone in pMARS's cdb debugger and
checks probes against the behaviour the RED source means (docs/semantics.md).

A spec (`behtests/<name>.beh`), one directive per line; a line starting with `#` is a comment
(only whole lines: `#` also marks immediate operands in a cell's TEXT):

    golden: bbctests/examples/prog8.bbc   the redcode to run: that golden's EXPECTED section
    redcode: prog8.red                    or a redcode file, relative to the spec's directory
                                          (what `run_compile.exe --emit-beh` writes); one of the two
    known-failing: i-7d2612-fffa6c        optional: a recorded bug, by its roadmap id
    hill: tiny                            optional: run under pmars/config/<hill>.opt (default 94b),
                                          for a warrior compiled with (hill ...) or --hill
    alive N                               a process is still running after N executed instructions
    dead N                                no process is left after N executed instructions
    cell N ADDR TEXT                      after N instructions, core cell ADDR disassembles to TEXT
                                          (whitespace-insensitive, e.g. `DAT.F #10, #10`; an
                                          empty cell, which cdb lists blank, is `DAT.F $0, $0`)

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
    redcode: Path | None = None
    known_failing: str | None = None
    config: Path = CONFIG
    probes: list[tuple[str, list[str]]] = field(default_factory=list)


def parse_spec(path: Path) -> Spec:
    spec = Spec(path)
    for n, raw in enumerate(path.read_text().splitlines(), 1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        if line.startswith("golden:"):
            spec.golden = ROOT / line.split(":", 1)[1].strip()
        elif line.startswith("redcode:"):
            spec.redcode = path.parent / line.split(":", 1)[1].strip()
        elif line.startswith("known-failing:"):
            spec.known_failing = line.split(":", 1)[1].strip()
        elif line.startswith("hill:"):
            spec.config = ROOT / "pmars" / "config" / (line.split(":", 1)[1].strip() + ".opt")
            if not spec.config.is_file():
                raise SystemExit(f"{path}:{n}: no settings for that hill: {spec.config.relative_to(ROOT)}")
        else:
            word, *args = line.split(None, 3 if line.startswith("cell") else 1)
            if word not in ("alive", "dead", "cell") or not args or not args[0].isdigit() or int(args[0]) < 1:
                raise SystemExit(f"{path}:{n}: not a probe: {raw!r}")
            spec.probes.append((word, args))
    if (spec.golden is None) == (spec.redcode is None):
        raise SystemExit(f"{path}: needs exactly one of `golden:` and `redcode:`")
    source = spec.golden or spec.redcode
    if not source.is_file():
        raise SystemExit(f"{path}: {source} is not a file")
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
    # always: the script returns at once when the build is current, and rebuilds an unpatched one
    subprocess.run([str(ROOT / "tools" / "pmars-host.sh")], check=True, stdout=subprocess.DEVNULL)
    return str(built)


def cdb(pmars: str, warrior: Path, commands: str, config: Path = CONFIG) -> list[str]:
    run = subprocess.run([pmars, "-@", str(config), "-e", "-b", str(warrior)],
                         input=commands + "quit\n", capture_output=True, text=True, timeout=60)
    if run.returncode not in (0, 4):  # 4: cdb `quit`; 3 would be an assembly error
        raise RuntimeError(f"pmars exited {run.returncode}: {run.stderr.strip() or run.stdout.strip()}")
    return run.stdout.splitlines()


# What pMARS fills the core with before loading warriors (pmars.c, INITIALINST).
EMPTY_CORE = "DAT.F $0, $0"


def squash(text: str) -> str:
    return re.sub(r"\s+", "", text)


def check_probe(pmars: str, warrior: Path, word: str, args: list[str], config: Path = CONFIG) -> str | None:
    """None when the probe holds, else what was seen instead."""
    n = int(args[0])
    if word in ("alive", "dead"):
        out = cdb(pmars, warrior, f"skip {n - 1}\ncalc CYCLE\n", config)
        alive = any(CALC_LINE.match(line) for line in out)
        if alive == (word == "alive"):
            return None
        return f"{'dead' if not alive else 'still running'} after {n} instructions"
    if len(args) < 3:
        return "cell needs N ADDR TEXT"
    addr, want = int(args[1]), args[2]
    out = cdb(pmars, warrior, f"skip {n - 1}\ncalc CYCLE\nlist {addr}\n", config)
    # A dead warrior ends cdb before `list` runs, and the only listing left would be the start-up
    # one: the cycle count is printed only while a process is alive.
    calc = next((i for i, line in enumerate(out) if CALC_LINE.match(line)), None)
    if calc is None:
        return f"dead after {n} instructions: cell {addr} was never listed"
    cell = re.compile(rf"^(?:\(cdb\) )?0*{addr}(?:\s+(.*))?$")
    # The last listing of the cell is `list`'s: cdb also prints the instruction at the start (cell 0,
    # before anything runs) and after `skip`, which can be the same address. cdb prints a cell equal
    # to empty core as its address alone (`cellview` in pMARS's disasm.c hides INITIALINST), which
    # looks like `calc`'s output: the listing is whatever matches after the cycle count.
    listed = [(m.group(1) or "").strip() or EMPTY_CORE for line in out[calc + 1:] if (m := cell.match(line))]
    if not listed:
        return f"no listing of cell {addr} (the warrior may be dead after {n} instructions)"
    got = listed[-1]
    return None if squash(got) == squash(want) else f"cell {addr} is `{got}`"


def run_spec(pmars: str, spec: Spec) -> list[str]:
    WORK.mkdir(parents=True, exist_ok=True)
    warrior = WORK / (spec.path.stem + ".red")
    warrior.write_text(expected_redcode(spec.golden) if spec.golden else spec.redcode.read_text())
    failures = []
    for word, args in spec.probes:
        try:
            seen = check_probe(pmars, warrior, word, args, spec.config)
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
