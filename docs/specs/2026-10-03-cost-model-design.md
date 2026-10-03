# Cost model, ordered IR and expectations — design (subproject A)

Status: approved in conversation on 2026-10-03, section by section; this document is the written
spec for review. Implementation follows a separate plan.

## Why

Before optimizing anything, the compiler needs to know what "better" means and to measure it. This
subproject gives it (1) an ordered, positioned view of the program it emits, (2) a vector of static
metrics computed on that view, (3) a user-configurable policy that orders those metrics, and (4) a
way for the programmer to state expected results — checked at compile time or exported as
simulator probes. It changes **no emitted code**: it measures the compiler as it is, and later
optimizations (subproject C) are measured with it.

The work is split into three subprojects; this spec covers only A.

| | Subproject | Depends on |
|---|---|---|
| **A** | metrics, cost model, ordered IR, predictions, policy, expectations | — |
| **B** | static performance analysis and warnings (possible slowdowns, possible optimizations) | A |
| **C** | language surface and code-generation changes: simple and compound operators, ICWS'94 default modifiers, a reserved label prefix, the `do-while` fix | A (costs), B (the `do-while` warning) |
| — | snippets: a library of named RED fragments with verified metrics and specs | needs a reuse mechanism in the language (C or the dev branch's lambdas) |

## Evidence behind the default

Measured on 2026-10-03 with pMARS 0.9.4 (host build), 94b settings, against the 12-warrior Wilkies
benchmark, 500 rounds, 3 repetitions; run-to-run noise ±2 points. Score = (3·wins + ties) × 100 / rounds,
averaged over the benchmark. Variants of a classic Dwarf:

| Variant | Change | Length | Score |
|---|---|---|---|
| base | 3 instructions per iteration | 4 | 44–47 |
| slow | +1 instruction per iteration (33 % slower) | 5 | 23–27 |
| long0 | +8 `DAT 0,0` cells | 12 | 40–43 |
| long1 | +8 `DAT 1,1` cells (visible to scanners) | 12 | 42 (one run) |

One instruction per iteration cost about 20 points; eight cells about 4. One warrior family, one hill,
one benchmark: it sets a default order (speed before size), not a law.

## Approaches considered

1. **Chosen:** static cost model over a positioned IR, with a lexicographic, user-configurable
   policy. Deterministic, explains each number, and is the base B and C need.
2. A weighted scalar score (`w₁·cycles + w₂·length + …`): more expressive, arbitrary weights. Kept as
   a future `policy` variant over the same metrics.
3. Empirical optimization against a benchmark: what matters on a hill, but ~1 s per evaluation, ±2
   noise, and an unlicensed external corpus. Kept as a future validation layer (`--bench`) to
   calibrate defaults and catch regressions.

## 1. The ordered IR (`src/layout.ml`)

An analysis view built from the `Red.instruction list` that `Compile.compile_expr` produces. Emission
is unchanged: redcode is still printed from the instruction list, with labels.

```ocaml
type field = FA | FB
type role = Code | Epilogue                (* Epilogue: the DAT compile_prog appends *)

type operand = { mode : Red.rmode; value : int; label : string option }
  (* value: the label resolved to a relative offset, normalised to 0..CORESIZE-1 *)

type cell = {
  pos    : int;                            (* 0 = first emitted instruction (ORG 0) *)
  op     : Red.opcode;  md : Red.rmod;
  a      : operand;     b  : operand;
  labels : string list;                    (* labels defined at this cell *)
  vars   : (string * field) list;          (* let variables stored in this cell's fields *)
  origin : Ast.tag option;                 (* RED node that emitted it *)
  role   : role;
}

type edge = Next of int | Jump of int | Skip of int | Dynamic   (* Dynamic: unknown target *)
type loop = { header : int; body : int list; back_edges : (int * int) list }
type program = { cells : cell array; succ : edge list array; loops : loop list;
                 diagnostics : diagnostic list }
```

- **Labels.** An `ILAB` occupies no cell; it attaches to the next instruction. A label at the end
  attaches to the epilogue. Offsets are resolved as pMARS resolves them.
- **Diagnostics** (not failures in A): an undefined label, a label defined twice (pMARS keeps the
  first; i-7d2612-425c66), a line of 256 characters or more (i-7d2612-174acf).
- **Origin.** `compile_expr` returns `(instruction, tag option)` pairs; `pp_instrs` ignores the tag.
  Comments (`ICOM`) are kept out of the cell array.
- **Variables.** A cell's `vars` comes from `penv`/`lenv`: the cell labelled `LETn` holds `x` in the
  field where `(store x)` sits. A cell may both execute and hold a variable (prog1's `MOV`).
- **Successors**, by ICWS'94:

  | Opcode | Successors |
  |---|---|
  | `DAT` | none |
  | `JMP t` | `Jump t` |
  | `SPL t` | `Next (pos+1)`, `Jump t` (two processes) |
  | `JMZ`, `JMN`, `DJN t` | `Jump t`, `Next (pos+1)` |
  | `SEQ`, `SNE`, `SLT`, `CMP` | `Next (pos+1)`, `Skip (pos+2)` |
  | `MOV ADD SUB MUL DIV MOD NOP LDP STP` | `Next (pos+1)` |
  | any jump whose target operand is indirect | `Dynamic` |

  `DIV`/`MOD` by zero also kill the process; the static view keeps `Next`, and the report says so
  only when the divisor is a constant 0.
- **Blocks and loops.** Leaders are the entry, every jump or skip target, and every cell after a jump
  or skip. Loops are natural loops of the back edges found by a depth-first walk from cell 0.
- **Stated limit.** The view describes the static program. Self-modification (the Dwarf's `ADD` that
  moves its pointer) is modelled as data that changes, not as control that changes; indirect jumps
  are `Dynamic`.

## 2. The metrics (`src/metrics.ml`)

A *cycle* is one instruction executed by one process; with *n* processes each advances once every
*n* cycles, so per-process figures scale by *n*.

**Global**

| Metric | Definition |
|---|---|
| `length` | cells emitted, epilogue included; reported against MAXLENGTH |
| `code`, `epilogue` | cells by role |
| `data` | cells never reached from the entry that hold a variable |
| `unreachable` | cells never reached from the entry that hold no variable (dead code) |
| `nonzero` | cells with a non-zero A or B number: what a `JMZ.F` scan sees |
| `nonblank` | cells different from `DAT.F $0, $0` (empty core): what a `SEQ.I` scan sees |
| `boot` | cycles from the entry to the first loop header: min and max over the paths |
| `spl_sites` | number of `SPL` instructions (process counts are not computed statically) |

**Per loop**

| Metric | Definition |
|---|---|
| `cycles/iter` | instructions executed per iteration: min and max over the loop's paths |
| `overhead/iter` | of those, the jumps and skips whose origin is a compiler-generated control node (`repeat`, `if`, `while`, `do-while`, conditions), not a user-written primitive |
| `exit` | cycles from the loop's last header visit to leaving it |

Reference values (the tests check them):

| Program | Values |
|---|---|
| prog1 (`bbctests/examples/prog1.bbc`) | `length 4`, `code 3`, `epilogue 1`, `nonzero 3`, `nonblank 4`, `boot 0`; one loop at cells 0–2: `cycles/iter 3`, `overhead/iter 0` (its `JMP` is user-written) |
| prog7 | `boot 1`; one loop at cells 2–3: `cycles/iter 2`, `overhead/iter 1` (the `DJN`) |
| prog8 | `boot 1`; one loop at cells 2–5: `cycles/iter 3` (`SLT`, `MOV`, `JMP`; the `JMP` to the end is skipped while looping), `overhead/iter 2` |

**Predictions** — heuristic, always labelled as predictions, and absent rather than guessed when no
pattern matches:

- **Fixed-step pointer.** A field that a loop changes by a constant `k` per iteration (`ADD #k` /
  `SUB #k` on it, or its use through `>` `<` `}` `{`) and that is used as the target operand of a
  write (direct or indirect): step `k`; period `CORESIZE / gcd(k, CORESIZE)` iterations; cycles to
  cover the core = period × `cycles/iter`. When `gcd(k, CORESIZE) > 1`, the report says the pointer
  does not visit every cell. prog1: step 4, period 2000, 6000 cycles.
- **Counter.** A `DJN` whose decremented operand has a constant initial value `n` (an immediate on the
  `DJN` itself, or a cell field initialised to `n`): `n` iterations, so the loop runs
  `n × cycles/iter` cycles, and a warrior that ends after it dies after
  `boot + n × cycles/iter + 1` instructions. prog7: `1 + 100 × 2 + 1 = 202`.

## 3. Policy and expectations

**Policy** (in `metrics.ml`):

```ocaml
type objective = Speed | Size | Stealth | Boot   (* cycles/iter · length · nonblank · boot *)
type policy = objective list                     (* lexicographic; default [Speed; Size] *)
val compare : policy -> t -> t -> int
```

`Speed` compares the worst `cycles/iter` over all loops, then the sum. In A the policy changes no
output; it is parsed, validated, reported, and unit-tested, so B and C can use it. Set it with
`run_compile.exe --optimize speed,size prog.src` or in the program header; the command line wins.

**Program header** (optional; a file that is a single expression stays valid):

```lisp
(program
  (optimize speed size)
  (expect (length <= 8))
  (let (x 0) (seq ...)))
```

**Expectations.** A new AST node, `Expect`, which emits no code. In the header it is global; as a
statement inside a loop's body it refers to the innermost enclosing loop (resolved through the
cells' origins).

| Kind | Forms | Checked |
|---|---|---|
| static | `(length <= N)`, `(cycles N)`, `(cycles <= N)`, `(overhead <= N)`, `(boot <= N)`, `(step K)`, `(covers-core)` | at compile time against the metrics. A failure is a compile error stating the measured value — *"expect cycles <= 3: the while of node 5 executes 4 per iteration"*; `--expect=warn` downgrades it to a warning |
| execution | `(alive N)`, `(dead N)`, `(cell ADDR "TEXT" N)` | in pMARS: `--emit-beh FILE` writes them as a behaviour spec, run by `tools/behave.py` |

`tools/behave.py` gains `redcode: <file>` as an alternative to `golden:` so it can run freshly
compiled output. One spec format, one runner.

## 4. Report, files and tests

**Report.** Standard output stays redcode only. `--report` writes a human-readable report to stderr;
`--report=json` writes JSON to stdout instead of the redcode. Example (prog1):

```
length 4/100   code 3  data 0  epilogue 1  unreachable 0   nonzero 3  nonblank 4   boot 0  spl 0
loop 0..2 (LET1, node 1)   cycles/iter 3   overhead 0   exit —
  predicted: step 4 → period 2000 iterations, covers core in 6000 cycles
policy: speed > size
```

**Files.** `src/layout.ml` (IR), `src/metrics.ml` (metrics, predictions, policy), `src/expect.ml`
(static checks, probe export); `src/parse.ml` and `src/ast.ml` accept `program`, `optimize`,
`expect`; `execs/run_compile.ml` gains `--report`, `--optimize`, `--expect`, `--emit-beh`. Dependency
chain: `red` ← `ast` ← `lib` ← `util` ← `analyse` ← `compile` ← `layout` ← `metrics` ← `expect`.

**Tests, written first.**

- Alcotest groups `layout` (offsets, roles, successors, loops, diagnostics), `metrics` (the reference
  values above), `expect` (passing and failing expectations with their exact messages) in
  `execs/run_test.ml`.
- Prediction against execution: the counter prediction for prog7 (202) must equal the `dead 202`
  that `behtests/prog7_dowhile_dn.beh` measures in pMARS.
- The `compare` suite stays green unchanged: proof that A did not alter a byte of output.

## Build order

Each step is its own commit, test first:

1. Origins through `compile_expr` (output identical).
2. `layout.ml`: cells, resolved labels, roles and variables, successors, blocks, loops, diagnostics.
3. `metrics.ml`: global and per-loop metrics.
4. Predictions: fixed-step pointer, counter.
5. Policy and `--optimize`; the `program` header.
6. `expect.ml`: static expectations; then `--emit-beh` and `redcode:` in `behave.py`.
7. Documents: `LANGUAGE.md` (`program`, `optimize`, `expect`), `docs/semantics.md` (metric
   definitions), `docs/architecture.md` (modules, chain), `docs/decisions.md` (A changes no output;
   default policy `[Speed; Size]` with the measurement above; failed expectations are errors by
   default), `docs/roadmap.md` (items for A, B, C, snippets).

## Decisions already taken for subprojects B and C

Recorded here so they are not lost; each is designed in its own spec.

- `do-while` with `GT`/`LT` (i-7d2612-fffa6c): layout `SLT b, a; SNE #0, #1; JMP head` (one more
  cell, same cycles), **plus** a performance warning from B wherever a construct costs extra cells
  or cycles, and a suggestion when a cheaper form exists.
- Label collisions (i-7d2612-425c66): generated labels take a **reserved prefix that user labels may
  not use**, enforced by the parser.
- Default modifiers (i-7d2612-96f7b1): adopt the ICWS'94 table, and consider **simple and compound
  operators** in RED that translate to different modifiers or instruction sequences.

## Verification limits

There is no opam switch on the development machine: the OCaml suites run in CI. Locally, the
scratch `ocamlc` build of `src/` with a `CCSexp` stand-in and `tools/behave.py` cover what they can;
each step's report names which of the two ran.

## Not in this subproject

Any change to emitted code; warnings (B); operators, modifiers, label prefix, `do-while` (C);
snippets; weighted policies; benchmark validation; process-count analysis for `SPL`.
