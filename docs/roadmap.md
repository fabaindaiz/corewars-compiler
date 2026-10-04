# Planned work

Accepted ideas and recorded defects, not yet built. **Not a promise and not a work order**: this is
where each one collides with something, written while it is clear. Every entry has a state —
**Planned**, **Half done**, **Done**, **Closed by measurement**, **Blocked outside** — and no entry
is ever deleted. Finishing an item edits its entry in the same change. New items get an id from
`python3 .agents/tools/bundle.py id i "<idea>"`, written after the `·`.

The core invariant every item is priced against: **a RED program compiles to redcode whose
execution in the core follows the program's meaning** (`docs/semantics.md`).

## The north star

**Every classic archetype — imp, dwarf, stone, scanner, paper, quickscan, core-clear — written in
RED compiles with near-zero overhead against its hand-written redcode** (d-7d2612-e006c2). It orders
everything below: what keeps an archetype from being written comes first, what adds cycles per
iteration second, the rest after. Measured on 2026-10-04 with `run_compile.exe --report`:

| RED construct | Cycles/iter | Control | Hand-written | Gap |
|---|---|---|---|---|
| `repeat` bomber (`SUB`, `MOV @`) | 3 | 1 (`JMP`) | 3 (a Dwarf) | none |
| `while (JN x)` around one instruction | 3 | 2 (`JMZ`, `JMP`) | 2 (`JMN` at the bottom) | +50 % per iteration (i-7d2612-3ca4c2) |
| a variable in its own `DAT` | boot +1 | — | the value in an unused field | +2 cells, +1 cycle (i-7d2612-400784) |

## Where we are

As of 2026-10-04 (s-7d2612-2206a5, s-7d2612-0cb4a2). `main` measures what it compiles: the cost model
(i-7d2612-aeab0f) is merged, with the review's findings fixed. `run_compile.exe --report` shows the
metrics and predictions, `(expect ...)` checks them or exports them to pMARS, and the command line
is `Cored.Driver`, tested in-process. The gate runs locally with an opam switch in `_opam/`:
`dune build`, 74 alcotest cases besides `execute` (Linux x86-64 only), an `--emit-beh` spec run end
to end in pMARS, the audit and seven behaviour specs. **Seven defects are recorded** below, five
with a failing check; phase 1 has since fixed six of them (see below).
`origin/dev` holds a half-done restructure that defines a different language (i-7d2612-ec4d2d).

**Phase 1 is complete** (s-7d2612-2c7e4d): every recorded correctness defect is fixed, `bbctests/known-bugs/`
is empty, errors carry `file:line:col`, and generated labels are reserved. **Next:** phase 2, the
classic archetypes written in RED as the acceptance suite (i-7d2612-34b61d).

## Phase 1 — Correctness, before the output changes

Everything later changes emitted code, so first the code must mean what RED says. Ready items first; the two golden migrations (prefix, defaults) last, one commit each, with their behavioural reason.

### Unary conditions always use the .B modifier · i-7d2612-3744e5
**State.** Done (s-7d2612-2c7e4d). `compile_cond` takes the modifier from the tested operand's
field (`.A` or `.B`, `.B` for a plain number or label); `behtests/cond1_afield.beh` passes and its
golden moved to `bbctests/examples/`. No other golden changed.
In `compile_cond` the label operand of `Cond1` is a reference, so `opmod_to_rmod` falls through to
the `.B` default: a variable stored in an A-field is tested on the B-field of its cell.
**Collides with.** Goldens whose unary condition reads a B-field variable must not change.
**Decide first.** Nothing: the field is known from `penv`.

### An inner let leaks its store placement into an outer variable of the same name · i-7d2612-ce4c3b
**State.** Done (s-7d2612-2c7e4d). Both halves fixed: the placement leak (`analyse_store_expr` stops
at a `let` that rebinds the name; `behtests/let_shadowing.beh`) and the initializer capture,
measured on 2026-10-04 (`y` held `$LET9`, the inner `x`), by `Rename.uniquify` before compilation
(`behtests/let_capture.beh`). Both goldens are in `bbctests/examples/`; no other golden changed.
`Analyse.analyse_store_expr` walks into a nested `ELet` that rebinds the same name, so the inner
`(store x)` sets the outer `x`'s field. Related, not yet measured: `replace_store` resolves an
initializer in the environment of the store site, so `(let (x 1) (let (y x) (let (x 2) … (store y))))`
captures the inner `x`.
**Decide first.** Stop the walk at a shadowing `let`, or uniquify names before analysis.

### do-while with GT or LT loops at equality · i-7d2612-fffa6c
**State.** Done (s-7d2612-2c7e4d). Post-condition `GT`/`LT` emit `SLT; SNE #0, #1; JMP head`: strict,
one more cell, the same two control instructions per iteration (`--report` counts them since the
branch review: a comparison of two immediates has one known successor). `behtests/dowhile_gt_equal.beh` passes; new specs
`dowhile_gt_count.beh` and `dowhile_lt_count.beh` pin the exact instruction count (8) and the final
value; the golden moved to `bbctests/examples/dowhile_gt_equal.bbc`.
Before the fix, `compile_cond2` in post-condition mode emitted `SLT a1, a2; JMP head` for `GT`, which repeats while
`a1 >= a2`; `LT` repeats while `a1 <= a2`. Pre-conditions (`if`, `while`) are correct. Measured: with
`x = y = 3` the compiled loop is still running after 10 instructions; ICWS'94 `SLT` is strict.
**Collides with.** Every golden that contains a `do-while` with `GT`/`LT` (none today).
**Decided** (2026-10-03, user): `SLT b, a; SNE #0, #1; JMP head` — one more cell per `do-while`
with `GT`/`LT`, the same cycles per iteration — plus a performance warning from subproject B where
a construct costs extra. Built in phase 1; the warning waits for phase 4 (i-7d2612-90d6e1).

### Four distinct CTError exceptions, none caught · i-7d2612-888db5
**State.** Done (s-7d2612-2c7e4d). One user error, `Ast.Error of loc option * string`, replaced the
four `CTError`s; the "please report this bug" states are `failwith`, which the driver reports as an
internal error with exit 2. `tools/audit.py` (`single-error-type`) now checks that no other
`*Error` exception appears; its known-failing mark is gone.

### Source locations in compile errors · i-7d2612-1703ff
**State.** Done (s-7d2612-2c7e4d). The user chose a fully located AST: the parser reads
s-expressions with positions (`CCSexp.Make` with `make_loc`) and builds `loc eexpr` directly (the
separate `expr` type is gone), tagging produces `meta eexpr` with `{ tag; loc }`, and
`compile_expr` gives an error raised without a location the innermost node's. Tags are numbered as
before, so no golden changed. Messages of `expect` and `--report` still name nodes by tag: moving
them to line and column is part of the warnings (i-7d2612-90d6e1).
**Why it was in phase 1.** Every warning of phase 4 must say where in the source it applies.

### Lines of 256 characters or more hang pMARS · i-7d2612-174acf
**State.** Done (s-7d2612-2c7e4d). `compile_prog` raises a compile error naming the line and its
length when any emitted line reaches 256 characters (the user's choice: exact, at emission); the
driver prints it and exits 1. `Layout`'s `Long_line` diagnostic uses the same constant.
Measured on pMARS 0.9.4: a 245-character label hung, 200 worked. RED passes user labels through.
**Decided** (2026-10-04, user): a check on emitted line length.

### User labels can collide with generated labels, and store-once is unchecked · i-7d2612-425c66
**State.** Done (s-7d2612-2c7e4d). Generated labels start with `_` (`_LET1`, `_WHI9`, …) and the
parser rejects user names starting with `_` and labels that are pMARS keywords; two stores of one
variable are an error, and a use with no store is an error at the use. Every golden with a generated
label changed once (15 of them), each checked mechanically to differ only by the renamed labels;
`behtests/label_collision.beh` passes and its golden moved to `bbctests/examples/`.
A user `(label LET1)` shares the namespace of generated labels; pMARS keeps the first definition
and only warns. Unchecked as well: a `let` whose variable has no `(store x)` (its `LET` label is
never defined) or two (defined twice), a user label that is a pMARS reserved word (`END`, `MOV`).
**Collides with.** d-7d2612-123e41 (label names are part of every golden).
**Decided** (2026-10-03, user): generated labels take a reserved prefix that the parser forbids in
user labels (pMARS labels are `[A-Za-z_][A-Za-z0-9_]*`, case-sensitive). Every golden changes once.
The user chose `_` (2026-10-04). Built in phase 1.

### A variable next to a plain reference loses its field to the default modifier · i-7d2612-9efd00
**State.** Planned. Known-failing: `behtests/mixed_operand_field.beh`, golden
`bbctests/known-bugs/mixed_operand_field.bbc`. Found by the phase-1 branch review.
`opmod_to_rmod` decides only when both operands are numbers or variables; a variable beside a plain
reference (`(MOV x (Dir -1))`, `(SLT x label)`) falls to the ICWS'94 default, so with `x` in an
A-field `MOV.I` copies the whole cell instead of `x`'s value. Before phase 1 the fallback was `.I`
for every opcode; user-written `JMZ`/`JMN`/`DJN`, where the variable is the only tested operand, were
fixed in the same review (`jump_modifier`, `behtests/user_djn_afield.beh`).
**Collides with.** Every golden with a variable beside a plain reference (none today but this one).
**Decide first.** What a plain reference means beside a variable: its B-field (so `MOV.AB`,
`SLT.AB`, `MOV.BA` …), its whole cell, or a compile error asking for an explicit modifier — part of
the operator design of phase 2 (i-7d2612-7eadd5).

### Fallback modifier .I differs from the ICWS'94 defaults · i-7d2612-96f7b1
**State.** Done (s-7d2612-2c7e4d). `compile_mod` falls back to `Red.default_modifier` (the A.2.1.1
table) instead of `.I`. Two goldens changed, checked line by line (`prog3`, `prog5`: `ADD`/`SUB .I` →
`.AB`); new golden and spec `add_default_modifier` pin the effect in the core. Compound operators
remain for phase 2 (i-7d2612-7eadd5).
Before the fix, when `opmod_to_rmod` could not decide (two references, no variable), the compiler emitted `.I`
(`SLT.I`, `JMZ.I`, `ADD.I #4, #3`). ICWS'94 A.2.1.1 defaults: `SLT`/`JMZ`/`JMN`/`DJN` to `.B`,
`MOV`/`SEQ`/`SNE` to `.I` only when neither operand is immediate, arithmetic with an immediate
A-operand to `.AB`. `SLT.I` requires both `A<A` and `B<B`.
**Collides with.** `prog0.bbc` and every golden with a raw-reference `MOV` (they expect `.I`,
which the default also gives); any golden with `ADD`/`SLT` on two references.
**Decided** (2026-10-03, user): adopt the ICWS'94 table, and consider simple and compound
operators in RED that translate to different modifiers or sequences. Built in subproject C
(i-7d2612-7eadd5); at least `prog3.bbc` and `prog5.bbc` change (`ADD.I #1, #1`, `SUB.I`).

## Phase 2 — Expressiveness: the archetypes as the acceptance suite

Write the classic warriors in RED and measure each against its hand-written form. What they cannot express decides the compound operators and the constants; the ones that work become the snippet catalogue.

### Classic warriors re-expressed in RED as end-to-end tests · i-7d2612-34b61d
**State.** Planned. Imp, Dwarf, Stone, a countdown core-clear, Mice, an imp spiral, a SEQ scanner,
a Silk-style paper: each exercises a different construct (`docs/references.md`, *Corpora*).
**Collides with.** i-7d2612-96f7b1 and i-7d2612-3744e5 for any warrior that needs them.
**Why it is the north star's measure** (d-7d2612-e006c2). Each archetype gets a hand-written counterpart and a row in the gap table above: cycles per iteration, length and benchmark score, compiled against hand-written. What the archetypes cannot express is what the language lacks.

### Operators, default modifiers, reserved label prefix and the do-while layout (subproject C) · i-7d2612-7eadd5
**State.** Planned. Carries the decisions recorded in i-7d2612-fffa6c, i-7d2612-96f7b1 and
i-7d2612-425c66.
**Collides with.** d-7d2612-5b410d ends here: C changes emitted code, so every changed golden needs
its behavioural reason (d-7d2612-6a1527), measured with the cost model.
**Decide first.** The reserved prefix; which compound operators exist and what each emits.

### Constants as named EQU · i-7d2612-a3f2b6
**State.** Planned. Constant optimizers (optiMAX, mopt) tune `EQU` constants; RED inlines them.

### Compile-error tests · i-7d2612-70ea22
**State.** Half done. The command line's error path is tested through `Cored.Driver`
(`test_driver_compile_error_is_clean`, `test_driver_missing_file`). Still no golden uses bbctester's
`STATUS: CT error`.

### Snippets: named RED fragments with verified metrics and specs · i-7d2612-8e9549
**State.** Planned. A catalogue of RED fragments (imp, bomber loop, scanner, ...), each with its
metrics and a behaviour spec. **Blocked on** a reuse mechanism in the language (subproject C or the
dev branch's lambdas, i-7d2612-ec4d2d).

## Phase 3 — The optimizer, under the policy

Transformations that change emitted code to improve the policy's metric, each measured with `--report` before and after. Speed first: one instruction per iteration is worth about five times eight cells.

### Loop rotation: the condition at the end of the loop · i-7d2612-3ca4c2
**State.** Planned (phase 3).
`while` tests at the top and jumps back from the bottom: two control instructions per iteration.
With the test moved to the bottom and one jump into it before the first iteration, a unary
condition costs one (`JMN head, x`). Measured: `while (JN x)` around one instruction runs 3 cycles per
iteration today; rotated, 2 — the difference that cost about 20 benchmark points in the Dwarf
experiment (`docs/specs/2026-10-03-cost-model-design.md`).
**Collides with.** Every golden with a `while` (prog8): each changes once, with this reason.
**What is already in its favour.** `--report` measures the gain; the `do-while` layout already is the
rotated shape.
**Decide first.** Binary conditions keep two instructions either way (`SLT` plus a jump); rotate them
too, for the shorter boot, or only unary ones?

### Variables in fields of existing instructions, not in separate DAT cells · i-7d2612-400784
**State.** Planned (phase 3).
A `let` whose `(store x)` sits in a `DAT` of its own costs a cell and, when the `DAT` is on the path,
a `JMP` around it (prog7, prog8: `JMP $2` then `DAT`): one more cell and one more cycle of boot.
The variable can live in a field the program never reads as code — the epilogue `DAT`, or an
instruction field the opcode ignores.
**Collides with.** d-7d2612-6a1527 (goldens change); i-7d2612-ce4c3b (placement analysis must be right
first).
**Decide first.** Whether the compiler may move a `(store x)` the user wrote, or only suggest it
(a phase-4 warning).

### Peephole cleanup of jumps · i-7d2612-ec59a0
**State.** Planned (phase 3).
Jumps to jumps, a `JMP` to the next cell, an `if` whose body is empty: local rewrites over the
emitted sequence, each kept only when `--report` shows the policy's metric improving.
**Collides with.** Goldens that contain such sequences.
**Decide first.** Nothing beyond the policy.

## Phase 4 — Warnings

Once errors have locations and the optimizer knows what it can do, the warnings can say where a cost is and what would remove it.

### Static performance analysis and warnings (subproject B) · i-7d2612-90d6e1
**State.** Planned. Warnings for possible slowdowns and possible optimizations, from the metrics:
an extra instruction per iteration, compiler overhead above a construct's minimum, a step whose gcd
with CORESIZE leaves cells unvisited, unreachable cells.
**What is already in its favour.** i-7d2612-aeab0f gives every number and the construct that
produced each cell.
**Decide first.** Which warnings are on by default, and whether a policy changes them.

## Phase 5 — The real world: hills and benchmarks

A warrior that can be submitted: other hills than 94b, the header lines KotH expects, and the benchmark as a regression signal.

### Multiple hill targets · i-7d2612-217183
**State.** Planned; the stated direction (d-7d2612-6d88cd). A target parameter (94b, 94nop, tiny,
nano, lp) selecting the header `;redcode-<hill>`, MAXLENGTH, CORESIZE for constants, and whether
`LDP`/`STP` are allowed (94nop has no p-space).
**Collides with.** Every golden's header; the `execute` suite's config path.
**Decide first.** Is the target a CLI flag, a header form in RED, or both?

### Warrior header metadata · i-7d2612-b682d5
**State.** Planned. Emit `;name`, `;author`, `;strategy` and `;assert` (pMARS warns on every
compiled warrior: "Missing ';assert'"; KotH replies the same).

### Benchmark score as a regression signal · i-7d2612-f27a91
**State.** Planned. `pmars -b -r 200 -F 4000 warrior bench/*.red` against the Wilkies set gives a
deterministic score in about a second (measured: prog1 55, a classic Dwarf 49, Imp 48).
**Decide first.** The benchmark has no licence statement: fetch at test time into `_build/`, never
vendor.

### The execute suite only assembles warriors · i-7d2612-05c64d
**State.** Half done. The mechanism is `tools/behave.py` (cdb probes); the content is three
specs for working programs. Missing: specs for `repeat`, `if-else`, `while` with `EQ`/`NE`,
indirection, `SPL`; and folding them into the OCaml suite if wanted.

## Phase 6 — Foundations and research

The compiler-correctness statement made executable, a kind system, and the macro layer.

### A RED reference interpreter for differential testing · i-7d2612-56302d
**State.** Planned. Run RED source and compiled redcode on the same initial core and compare cells
(translation validation, `docs/semantics.md`). Later, QCheck-generated programs.

### A static kind check for RED · i-7d2612-f2f7c5
**State.** Planned. Kinds `Num`, `Lab`, `Place` (`docs/semantics.md`, *Statics*): jump targets are
labels, `#x` is an explicit offset, every let variable is stored exactly once. Subsumes the
store-once half of i-7d2612-425c66.

### The new compiler structure on the dev branch · i-7d2612-ec4d2d
**State.** Planned (phase 6); a half-done start on `origin/dev` (2025-09-09), not merged. Moves to `lib/{common,core,parsing,surface}`
and `bin/`, adds an opam file, disables `execs/`.
**Collides with.** d-7d2612-123e41 (it adds a global `gensym`); RED itself (its surface language is
a simply typed lambda calculus, not RED); every test (none run on the branch). Known defects there:
`List.hd` on an empty `seq` in `typecheck.ml`, unbound names raise `failwith`.
**Decided** (2026-10-04, user; d-7d2612-5de7a6): its typed lambda calculus becomes RED's
compile-time macro layer — functions that are inlined into RED before code generation, terminating
because λ→ is strongly normalising — rebuilt on today's `main` rather than merged. It is the reuse
mechanism snippets wait for (i-7d2612-8e9549). Its global `gensym` is not kept (d-7d2612-123e41).

## Alongside — toolchain and documentation

Cheap items taken when a phase touches them; none blocks a phase.

### dune runtest runs no tests · i-7d2612-47d3ea
**State.** Planned. There is no `(test)` stanza; `make tests` is the only entry. Adding one changes
how CI and editors run the suite.

### Reproducible install of bbctester · i-7d2612-8f3f22
**State.** Planned. The code needs the BBCStepTester fork (public name `bbctester`, commit
2cb3669), which has no `.opam` file; pleiad/BBCTester has an incompatible API under the same name.
CI clones and `dune install`s it into setup-ocaml's local switch (works since s-7d2612-cc9344). An opam file with `pin-depends`, or a
vendored submodule with `(vendored_dirs ...)`, would make it one command.

### The cored profile in dune-workspace is never selected · i-7d2612-ceee87
**State.** Planned. `(env (cored ...))` applies only under `--profile cored`, which nothing passes;
builds use the `dev` profile, where unused opens and values are errors. Decide whether warnings are
errors, then make the file say so.

### RED tutorial · i-7d2612-e8f3f0
**State.** Planned. `TUTORIAL.md` is a title only. Programming games that teach assembly (TIS-100,
EXAPUNKS) teach through small constrained goals with cycle and size counts; a tutorial built from
the classic-warriors item (i-7d2612-34b61d) would reuse those programs.

### pMARS 0.9.5 · i-7d2612-494e75
**State.** Planned. Released 2026-01-03 with overflow and bounds fixes; builds on macOS without the
`round` rename. **Collides with** d-7d2612-3d04ba (the vendored binary and zip are 0.9.4).

### The Docker image cannot run the vendored pmars or build the tests · i-7d2612-202da9
**State.** Planned. `ocaml/opam:ubuntu-20.04` ships glibc 2.31; `pmars/pmars` needs 2.34 and
`libX11.so.6`. The image also installs neither bbctester nor its dependencies, and pins OCaml 5.0.0
while this machine has 5.5.1. Not verified by running Docker.

### Adopt ocamlformat · i-7d2612-2a14f2
**State.** Planned. Needs a pinned `.ocamlformat` (`version=`), one formatting commit with nothing
else in it (d-7d2612-8cdc44), then `dune build @fmt` in the gate.

## Done

### Cost model, ordered IR and expectations (subproject A) · i-7d2612-aeab0f
**State.** Done, merged into `main` (s-7d2612-a654a5, s-7d2612-2206a5). Spec
`docs/specs/2026-10-03-cost-model-design.md`, plan `docs/plans/2026-10-03-cost-model.md`.
`Layout` (cells, successors, loops, label diagnostics), `Metrics` (length, roles, nonzero,
nonblank, boot, per-loop cycles/overhead/exit, step and counter predictions, the policy),
`Expect` (static checks, `--emit-beh` probes); `--report[=json]`, `--optimize`, `--expect=warn`.
The prog7 counter prediction (202) equals what pMARS measures.
**Still missing.** Weighted policies; benchmark validation (`--bench`); process counts for `SPL`.
The review's minor findings were all fixed (s-7d2612-2206a5): expectations keep labels, DIV/MOD by a
zero B-number under `.F`/`.X`/`.I`, `--emit-beh` checks, probe counts from 1, `(optimize)` needs an
objective, `unreachable` qualified when jumps are dynamic, JSON escaping with policy and coresize,
clean compile errors, and the CLI under test (`Cored.Driver`); `Stdlib.Arg` declined
(d-7d2612-7edd7d).

## Process and tooling

Friction goes here once it has been hit twice, with the arithmetic.

### Line endings rewritten by tools · i-7d2612-276a54
**State.** Done (s-7d2612-0cb4a2).
**What happened.** Editing by script through a text API turned CRLF files into LF: five files in the
bootstrap, five more in the cost model, the second time forcing a rewrite of the branch's history.
**Cost.** One whole-file diff per touched CRLF file, unreadable `git blame`, and about half an hour
to detect and repair, × every session that edits a CRLF file by script.
**The fix.** `tools/audit.py` (`eol-preserved`) fails when a tracked file's line-ending style differs
from `HEAD`, so `make check-tools` stops the commit (d-7d2612-040878). Seen to fail on a planted
`Makefile` converted to LF.
**Seen in.** s-7d2612-0a037e, s-7d2612-a654a5.

## Closed by measurement

### Comment injection into pMARS directives · i-7d2612-476e03
**State.** Closed by measurement. The worry: `ICOM` prints `;text`, and a comment starting with
`redcode` or `name` would be read as a pMARS directive. Measured: RED's `com` always prepends a
space (`; redcode ...`), and pMARS 0.9.4 does not read that as a directive. It would reopen only if
another pass emitted `ICOM` without the space.

### Quadratic list and string concatenation · i-7d2612-2a89a3
**State.** Closed by measurement. `@` in `compile_expr` and `^` in `pp_instrs` are quadratic, but a
warrior is at most 100 instructions on 94b: about 5 000 steps. Reopens with a target allowing far
longer warriors.

## What each one costs the invariant

| Item | Does it put the core invariant at risk? |
|---|---|
| i-7d2612-fffa6c, i-7d2612-3744e5, i-7d2612-425c66, i-7d2612-ce4c3b | No — each restores it for a construct that breaks it today |
| i-7d2612-96f7b1 fallback modifier | Yes, until decided: changing defaults changes behaviour of existing programs |
| i-7d2612-888db5, i-7d2612-1703ff, i-7d2612-174acf | No — errors and limits, not code generation |
| i-7d2612-05c64d, i-7d2612-70ea22, i-7d2612-34b61d, i-7d2612-f27a91, i-7d2612-56302d | No — they observe it |
| i-7d2612-47d3ea, i-7d2612-8f3f22, i-7d2612-202da9, i-7d2612-494e75, i-7d2612-2a14f2, i-7d2612-ceee87 | No — tooling; pMARS 0.9.5 needs the behaviour specs re-run on it |
| i-7d2612-217183 hill targets | Yes — the meaning of constants and the length limit change per target |
| i-7d2612-b682d5, i-7d2612-a3f2b6 | Low — header lines and constant spelling, but `EQU` changes every golden |
| i-7d2612-f2f7c5 kind check | No — it rejects programs, never changes output |
| i-7d2612-ec4d2d dev branch | Yes — a different language and a non-deterministic label scheme |
| i-7d2612-e8f3f0 tutorial | No |
| i-7d2612-aeab0f cost model | No — it measures; d-7d2612-5b410d |
| i-7d2612-90d6e1 warnings | No — it reports |
| i-7d2612-7eadd5 subproject C | Yes — it changes emitted code; each change needs a behaviour spec |
| i-7d2612-8e9549 snippets | No |
| i-7d2612-3ca4c2 loop rotation | Yes — it changes every `while`; a behaviour spec per layout |
| i-7d2612-400784 variables in fields | Yes — a moved variable must keep its value and field |
| i-7d2612-ec59a0 peephole | Yes — each rewrite needs the behaviour specs green |
| i-7d2612-276a54 line endings | No |
