# 4. Tools

## The compiler

```text
usage: run_compile.exe [--optimize o1,o2] [--report[=json]] [--expect=warn] [--warn=all|none] [--hill KEY] [--emit-beh FILE.beh] <filename>
```

Run it as `dune exec execs/run_compile.exe -- [options] FILE.src`; the redcode goes to standard
output, everything else to standard error.

| Option | What it does |
|---|---|
| `--report` | the warrior's metrics and predictions, on standard error |
| `--report=json` | the same as JSON, on standard output, instead of the redcode |
| `--optimize o1,o2` | the policy: what the compiler optimizes, in order (overrides the header's `optimize`) |
| `--hill KEY` | the hill: `94b` (default), `94nop`, `94`, `94x`, `tiny`, `nano` (overrides the header's `hill`) |
| `--warn=all` / `--warn=none` | every warning whatever the policy, or none |
| `--expect=warn` | a broken expectation warns instead of stopping the compilation |
| `--emit-beh FILE.beh` | write the program's run-time expectations as a behaviour spec, and the redcode beside it |

Exit codes: 0 compiled; 1 an error in the program, printed as `file:line:column: error: ...`; 2 an
internal error of the compiler (please report it, with the program).

```text
e1.src:1:17: error: variable `x` is used but no (store x) places it
```

## The report

```text
length 5/100   code 4  data 1  epilogue 1  unreachable 0   nonzero 3  nonblank 4   boot 0  spl 0
loop 0..2 (_REP4, repeat, node 4)   cycles/iter 3   overhead 1   exit —
  predicted: step 4 → period 2000 iterations, does not visit every cell (6000 cycles per period)
policy: speed > size
optimizations: none
```

- **length**: cells, out of the hill's maximum; `code`, `data` and the final `epilogue` cell;
  `unreachable` code no path reaches; `nonzero` and `nonblank` cells (what a scanner can see).
- **boot**: cycles before the first loop starts; **spl**: how many `SPL` the warrior has.
- **loop**: each loop's cells, the construct that made it, the cycles one iteration costs, and how
  many of those are control instructions the compiler added (`overhead`).
- **predicted**: what the compiler can tell without running: a pointer's step and whether it
  visits the whole core, how long a counter keeps a loop running.
- **optimizations**: which optional transformations the policy chose (below).

## The policy: what "optimized" means

The compiler can lay some constructs out in more than one way: a `while` with its test at the top
or at the bottom, a jump to the next cell kept or removed, an `if` around one instruction as a
skip. It compiles every combination, measures each, and keeps the best for your objectives, in
order:

| Objective | Measures |
|---|---|
| `speed` | cycles per loop iteration |
| `size` | the warrior's length |
| `stealth` | the cells a scanner can see |
| `boot` | the cycles before the first loop starts |

The default is `speed size`. Write another in the header, `(optimize size speed)`, or on the
command line.

## Warnings

The compiler warns where the warrior pays a cost that something could remove, and says what:

| Warning | When |
|---|---|
| a pointer here advances K cells per iteration and visits only N of 8000 cells | always, unless the loop states `(expect (step K))` or `(expect (covers-core))` |
| N cells never executed and holding no data (dead code) | with `size` or `stealth` in the policy |
| a loop spends more control instructions than its construct needs | with `speed` in the policy, when a higher objective declined the faster layout |

Warnings never stop the compilation. A cell you keep as data should carry a label (or hold a
variable), or it counts as dead code.

## Expectations

`(expect e)` states something the warrior must do. In the header it applies to the whole warrior;
inside a loop's body, to that loop. It emits no code.

Checked when compiling (a failure stops the compilation, or warns with `--expect=warn`):

| Expectation | Holds when |
|---|---|
| `(length <= N)`, `(length N)` | the warrior is at most (exactly) N cells, the final cell included |
| `(cycles <= N)`, `(cycles N)` | an iteration of the loop costs at most (exactly) N cycles |
| `(overhead <= N)` | at most N of them are control instructions the compiler added |
| `(boot <= N)` | at most N cycles before the first loop starts |
| `(step K)` | a pointer in the loop advances K cells per iteration |
| `(covers-core)` | that pointer visits every cell of the core |

```text
expect length <= 3: the warrior is 5 cells
```

Checked by running the warrior in pMARS (`--emit-beh`, then `tools/behave.py`):

| Expectation | Holds when |
|---|---|
| `(alive N)` | a process is still running after N executed instructions |
| `(dead N)` | no process is left after N executed instructions |
| `(cell ADDR "TEXT" N)` | after N instructions, cell ADDR holds the instruction TEXT |

## Behaviour specs

A spec is a small text file that runs a warrior alone in pMARS's debugger and checks what the core
holds. `tools/behave.py` runs them; the repository's own are in `behtests/`.

```text
golden: bbctests/archetypes/dwarf.bbc
cell 2 7 DAT.F #0, #4
alive 1000
```

`golden:` takes the redcode from a test's expected output; `redcode: FILE.red` takes it from a file
(what `--emit-beh` writes). `hill: 94nop` runs it under another hill's settings. Make sure a spec
can fail: change one number, watch it go red, put it back.

## Running and debugging in pMARS

```sh
tools/pmars-host.sh                                       # build pMARS for this machine, once
P=_build/pmars-host/pmars
printf 'skip 9\nlist 0,6\nquit\n' | $P -@ pmars/config/94b.opt -e -b w.red   # step it
$P -@ pmars/config/94b.opt -b -k -r 200 -F 4000 w.red other.red              # fight 200 rounds
```

| cdb command | Effect |
|---|---|
| `skip K` | execute K+1 instructions, then show the next one |
| `step` | execute one instruction |
| `list A,B` | show cells A to B (a cell equal to empty core shows only its address) |
| `calc CYCLE` | cycles left; printed only while a process is alive |
| `registers` | the cycle, the processes and their queue |
| `quit` | stop |

## The benchmark

```sh
python3 tools/bench.py                      # the archetypes, RED and hand-written, against the baseline
python3 tools/bench.py w.src other.red      # any warriors, RED or redcode
python3 tools/bench.py --hill w.src         # also the estimated place on Koenigstuhl's 94nop hill
```

It scores each warrior against the Wilkies benchmark (12 warriors, 94b settings, 500 rounds) and
the top 20 of the Koenigstuhl 94nop hill (200 rounds), with a fixed seed. The opponents are
downloaded into `_build/bench/` on first use and never committed. The score is the mean of
(3 × wins + ties) × 100 / rounds.

## When something goes wrong

| Symptom | Likely cause |
|---|---|
| `error: variable x is used but no (store x) places it` | every `let` variable needs one `(store x)` in its body |
| a jump goes to the wrong place | a bare number is a value: write `(Dir 1)` for "the next cell" |
| `` `step` is a RED word `` | a template or parameter took a reserved name; pick another (`stride`) |
| a label at the end of the warrior is undefined in pMARS | it needs an instruction after it; the compiler's final `DAT` gives it one |
| pMARS says "Missing ';assert'" | harmless; `(hill ...)` in the header emits one |
| the loop is slower than you expected | `--report` shows each loop's cycles and overhead; the warnings say what would remove it |

The repository's troubleshooting notes are in `.claude/skills/troubleshoot-redcode/SKILL.md`.
