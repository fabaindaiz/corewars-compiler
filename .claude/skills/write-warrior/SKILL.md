---
name: write-warrior
description: Write, compose, tune or measure a Core War warrior in RED with this repository's compiler - pick a strategy from the cookbook or snippets, state its loops with expect, check what it does in pMARS, score it against Wilkies, Koenigstuhl's top 20 and the hill. Use when asked to "write a warrior", "make it stronger", "try a strategy", "tune the constants", "compose a stone and an imp", "how good is it" or to test an idea in redcode.
allowed-tools: Bash, Read, Write, Edit
---

# Write a warrior

The user-facing version of this flow is `docs/manual/05-workflows.md`; this is the agent's
checklist. Every step's cost was measured (2026-10-05): compile and report under 10 ms, a behaviour
spec 0.07 s, `tools/bench.py` (Wilkies and the top 20) about 6 s, `--hill` (the whole Koenigstuhl
hill, 1107 warriors) about 90 s a warrior: run it in the background, never block on it.

## The flow, cheapest check first

1. **Start from a strategy that works**: `docs/manual/03-cookbook.md`, `archetypes/*.src`, or
   `(include "snippets/NAME.src")` (`snippets/README.md` has each template's cost). Each compiles to
   its hand-written twin's code, but for the two scanners' pointer cells; the gap to the top of a
   hill is strategy (`docs/research/2026-10-04-benchmark.md`).
2. **Header**: `(hill 94nop)` for Koenigstuhl (94b is the default and the tests' settings),
   `(name ...)`, constants as `(const ...)` so they stay `EQU`s to tune.
3. **Compile with `--report`** and read every warning: a pointer's partial coverage, dead code
   (a data cell needs a label), a loop with more control than its construct needs.
4. **State each loop** with `(expect (step K))`, `(expect (cycles N))`: free on every compile.
5. **State what it does** with `(expect (alive N))` and `(expect (cell ADDR "TEXT" N))`, export
   with `--emit-beh _build/w.beh`, run `python3 tools/behave.py _build/w.beh`. Make a probe fail
   once (change a number) before trusting it. Trace before writing a probe: hand counts are wrong
   often enough to be a recorded friction (i-7d2612-340f22); cdb prints values above 4000 as
   negative (4396 is `-3604`).
6. **Measure**: `python3 tools/bench.py w.src` (and the hand-written twin if there is one); `--hill`
   in the background before calling it done. Compare against `tools/bench_baseline.json`.
7. **Tune one constant at a time**, measuring each; write the sweep's results into a dated
   `docs/research/` note, not only into the chat.

## Pitfalls that cost a round each (from the log)

- A bare number is a value: `(SPL 1)` is `SPL #1`, which splits onto its own cell. A jump target
  is `(Dir 1)` or a label.
- `step` is a RED word (an expectation): name a parameter `stride`.
- A warrior written from memory of a published one reproduces its data. The user's rule: unlicensed
  sources are ideas only. Choose constants and names afresh, and say in the file whose idea it is.
- A cell kept as data without a label is reported as dead code.
- A step whose bombs reach the warrior's own loop kills it: the step-3 dwarf bombs its `MOV` at
  instruction 8000. The compiler warns ("reaches cell X of its own loop after N iterations"),
  including a pointer kept in the loop's own `JMP` and written every lap; it says nothing when the
  pointer is rewritten before its loop or when the hit would come after a write on the pointer's
  own cell. Prove survival with `(expect (alive N))`, N under 80000. Measure the RED warrior, not
  its hand-written twin: the two scanners' layouts differ (the hand-written SEQ scanner dies alone
  at instruction 2964, the RED one does not).
- `(JN (Ind p))` tests the B-field of the cell `p` points to; compare whole cells (`(NE I ...)`) to
  see every non-empty cell.
- Quote a battle with a fixed seed (`-F 4000`), and measure the warriors the text shows (compile
  the `.src`), not their hand-written twins. The measured circle: the papers here beat the stone and
  every scanner here; the stone beats the scanner
  (`docs/research/2026-10-05-presenting-red.md`).
- `(alive N)` with N of 80000 or more never holds on 94b: the round ends at 80000 cycles.
- `(start label)` when data must precede the code; the metrics then measure from the entry.

## Done means

The warrior compiles without unexplained warnings, its specs pass and were seen to fail, its scores
are in a dated research note beside the archetypes', and, if it joins `archetypes/` or `snippets/`,
it has a golden, a spec, a baseline entry (`tools/bench.py --update`) and a row in the roadmap's
table or `snippets/README.md`.
