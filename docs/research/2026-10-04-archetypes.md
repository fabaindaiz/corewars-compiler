# The classic archetypes in RED, measured against hand-written redcode

Date: 2026-10-04. Roadmap item i-7d2612-34b61d (phase 2), north star d-7d2612-e006c2. Each
archetype exists twice: `archetypes/NAME.red`, written by hand for this study (no third-party
code), and `archetypes/NAME.src`, the same warrior in RED. The RED versions are goldens in
`bbctests/archetypes/` and behaviour specs `behtests/archetype_*.beh`.

## Method

- **Static cost:** `run_compile.exe --report` on the RED version; the hand-written one counted
  from its listing, with the same definitions (`docs/specs/2026-10-03-cost-model-design.md`).
- **Behaviour:** each RED warrior runs in pMARS's cdb with the 94b settings
  (`pmars/config/94b.opt`); the spec states a cell it must have written and that it is alive after
  1000 instructions. Every spec's probe was computed by hand before running, and one was mutated to
  check that it fails.
- **Score:** the Wilkies benchmark (`docs/references.md`, *Hills and community*), twelve warriors
  from the early 1990s, fetched into `_build/bench/` and never committed (it has no licence
  statement). Score = mean over the twelve of (3 × wins + ties) × 100 / rounds, 500 rounds, two runs
  each, with the host pMARS (`tools/pmars-host.sh`). The script, reproducible from a clean clone:

  ```sh
  mkdir -p _build/bench && cd _build/bench
  curl -sfL -o wilkies.zip http://www.koth.org/wilkies/bench.zip || curl -sfL -o wilkies.zip http://www.koth.org/wilkies/wilkies.zip
  unzip -o -q wilkies.zip && cd ../..
  # score WARRIOR: 500 rounds against each, 94b settings
  for b in _build/bench/*.RED; do
    _build/pmars-host/pmars -@ pmars/config/94b.opt -b -k -r 500 WARRIOR "$b" | head -1
  done   # each line: wins ties; average (3*W+T)*100/500 over the files
  ```

  Two runs of the same warrior differ by up to 4 points (dwarf RED: 44 and 48), so a gap under
  about 4 points is noise at 500 rounds.

## Results

| Archetype | RED construct | Cycles/iter hand | Cycles/iter RED | Cells hand | Cells RED | Score hand | Score RED |
|---|---|---|---|---|---|---|---|
| imp | one `MOV` | 1 | 1 | 1 | 2 | 50, 49 | 49, 48 |
| dwarf | `repeat` (`ADD`, `MOV I` through `@`) | 3 | 3 | 4 | 5 | 46, 46 | 44, 48 |
| stone | `SPL 0` before the dwarf, step 3044 | 3 | 3 | 5 | 6 | 77, 80 | 80, 80 |
| core-clear | `repeat` (`MOV I` through `>`) | 2 | 2 | 4 | 5 | 45, 46 | 42, 42 |
| scanner | `repeat` (`ADD`, `if (JN @p)` bomb) | 2 (empty cell) | **3** (empty cell); 2 threaded | 6 | 7 | 56, 57 | **42, 42**; 54, 58 compiled with threading |
| paper | `repeat` (reset, `do-while (JN n)` copy, `SPL @d`, `ADD`) | 6 + 2 per cell | 6 + 2 per cell | 7 | 8 | 78, 80 | 80, 79 |
| SEQ scanner | `repeat` (`ADD F` to two pointers, `if (NE I @a @b)` bomb) | 3 (pair of empty cells) | not expressible; 3 with `(NE I ...)` | 7 | 9 | 36, 37 | 37, 37 |

The SEQ scanner row was measured after conditions gained a modifier the same day (gap 5); its two
extra cells are the pointer cell (the hand-written one keeps its pointers in the `SNE.I` itself)
and the epilogue.

Five of six archetypes compile with **zero cycle overhead**; the sixth, the scanner, loses 14
points to one extra cycle on its hot path (closed the same day by jump threading, gap 1). Every
RED warrior is one cell longer than its hand-written form.

## Gaps found

1. **Scanner: a jump to a jump on the hot path.** `(repeat (seq (ADD 10 p) (if (JN (Ind p)) ...)))`
   compiles the `if`'s false branch to `JMZ.B $_IF9, @p`, and `_IF9` holds only the `repeat`'s
   `JMP $_REP4`. An empty cell costs `ADD`, `JMZ`, `JMP`: 3 cycles, where the hand-written scanner
   jumps straight back (`JMZ.B scan, @ptr`): 2. Closed by jump threading (peephole,
   i-7d2612-ec59a0): retargeting the `JMZ` to `_REP4` makes the RED code the hand-written code, cell
   for cell, except the epilogue. Measured by editing the compiled warrior that way: 55 and 55,
   against 56 and 57 by hand. The extra cycle is the whole 14 points. **Closed** the same day: the
   user pulled threading forward (d-7d2612-3f3f32), and the compiled scanner now emits
   `JMZ.B $_REP4` and scores 54 and 58.
2. **One extra cell, always: the epilogue `DAT`.** `compile_prog` appends `DAT 0, 0` after every
   program. None of the six can fall off its end: each ends in a `repeat` or an imp. **Kept, by the
   user's decision** (d-7d2612-1c1c67): it exists because pMARS discards a label with no
   instruction after it (measured: a trailing `fin` is reported as "Discarding these labels" and a
   `JMP fin` is then "Undefined label"), and it is meant to be indistinguishable from empty core,
   so it costs length and nothing else. One difference remains: pMARS fills empty core with
   `DAT.F $0, $0` (`pmars.c`, lines 165–166, in the vendored `pmars/pmars-0.9.4.zip`), while the epilogue
   is `DAT.F #0, #0`; a `JMZ`/`JMN` scanner cannot tell them apart, an `SEQ.I`/`SNE.I` scanner
   can (i-7d2612-ed9f79). **Closed** the same day: the epilogue is `DAT $0, $0`
   (d-7d2612-f7ae87), and pMARS's cdb lists it as an empty cell.
3. **`unreachable` counts declared data as dead code.** core-clear and scanner hold their bomb as
   `(label bomb) (DAT 0 0)`; `--report` counts it as unreachable code, because only a `let`
   variable's cell counts as data. A phase-4 warning on unreachable cells (i-7d2612-90d6e1) would
   fire on every bomber. Either the metric counts a labelled, never-executed `DAT` as data, or RED
   gets a way to declare data that is not a variable (i-7d2612-cf8fdb). **Closed** the same day,
   the first way: a labelled `DAT` that is never executed counts as data.
4. **A loop's net step is reported per instruction, not per iteration.** In the paper, the outer
   loop moves `d` by `ADD #2365` once and by `<d` seven times (in the inner loop): 2358 cells per
   lap. `--report` predicts two separate steps for the outer loop, 2365 and −1, neither of which is
   what `d` does (i-7d2612-fbe7c8).
5. **SEQ scanner: a comparison of the cells two pointers select is not expressible.** A SEQ
   scanner compares two whole cells (`SEQ.I`) whose offsets live in one cell's two fields, moved
   together by `ADD.F`. In RED, `(if (NE (Ind a) (Ind b)) ...)`, with `a` and `b` in the A and B
   fields of one `DAT`, compiles to `SNE.AB *a, @b`: the A-field of one target against the B-field
   of the other, because the modifier is chosen from the fields the *pointers* live in.
   `docs/semantics.md` §3 says which field an indirect use goes *through*, not which field of the
   target it reads; conditions take no modifier, so the user cannot ask for `.I`
   (i-7d2612-2581ff). Related to i-7d2612-9efd00 (mixed operands) and the operator design
   (i-7d2612-7eadd5). **Half closed** the same day: conditions accept a modifier
   (`(NE I (Ind a) (Ind b))`, d-7d2612-8f9340), and the SEQ scanner is now
   `archetypes/seqscan.src`, as fast as the hand-written one. The default is still open.
6. **A copy through a pointer copies one field by default.** `(MOV b (Ind b))` compiles to
   `MOV.B`: it writes the B-field of the target. Every bomber here needs `(MOV I b (Ind b))`. A
   bomb is a whole instruction, so the default is wrong for the commonest use of `MOV` through a
   pointer; the same question as gap 5, from the other side.
7. **What could not be written at all.** An imp spiral launches processes at `imp + k × 2667`,
   and a quickscan unrolls a score of comparisons at `start + k × step`: both need arithmetic on
   labels or named constants (RED has neither; `EQU` constants are i-7d2612-a3f2b6) and a way to
   repeat a fragment at compile time (snippets, i-7d2612-8e9549; the dev branch's macro layer,
   i-7d2612-ec4d2d). Writing the offsets as numbers by hand works and is the error-prone thing the
   language exists to remove.

Smaller observations:

- The hand-written core-clear keeps its pointer before the code and starts with `END top`. RED
  starts at the first emitted instruction (no `ORG`), so data goes after the code; it costs
  nothing here, but a warrior whose pointer must precede its code (Mice's copy-by-index) has to be
  rearranged.
- `(SPL 0)` compiles to `SPL #0, #0`, equivalent to `SPL $0` under ICWS'94 (an immediate operand
  addresses its own instruction). `--report` lists it as a one-cycle loop of user code, which it
  is for the process scheduler.
- The paper keeps its copy counter in the B-field of its first instruction, `(MOV 7 (store n))`,
  compiled to `MOV.AB #7, #7`: a self-resetting counter, the idiom a hand-written paper uses.

## What this decides, and what it leaves to the user

The cycle gaps the north star cares about come from one place, the jump to a jump (gap 1), which is
phase 3's peephole item. The length gap is the epilogue (gap 2). The expressiveness gaps (5, 6, 7)
are the input to subproject C's operator design: which modifier an indirect use reads, whether
conditions take a modifier, and constants or label arithmetic. They are presented as decisions,
not decided here.
