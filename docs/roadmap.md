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
| `while (JN x)` around one instruction | 2 (rotated; was 3) | 1 (`JMN`) | 2 (`JMN` at the bottom) | none per iteration, +1 boot cycle (d-7d2612-a773b1) |
| a variable in its own `DAT` | boot +1 | — | the value in an unused field | +1 cell, +1 cycle; in a `repeat`'s `JMP` with `(repeat body (store p))`, none (d-7d2612-d9e5d3) |

The archetypes themselves, RED against hand-written (`docs/research/2026-10-04-archetypes.md`;
Wilkies score, 500 rounds, two runs; noise about 4 points):

| Archetype | Cycles/iter RED / hand | Cells RED / hand | Score RED / hand | Gap |
|---|---|---|---|---|
| imp, dwarf, stone, core-clear, paper | equal | +1 each | within noise | the epilogue `DAT`, kept by decision (d-7d2612-1c1c67) |
| scanner | 2 / 2 per empty cell (was 3) | 6 / 6 (was 7) | 63, 61 / 56, 57 (was 42) | cycles closed by jump threading (d-7d2612-3f3f32); the pointer in the `repeat`'s `JMP` (d-7d2612-d9e5d3); the score is the layout's: a hand-written scanner with it scores 61, 61 |
| SEQ scanner | 3 / 3 per pair of empty cells | 9 / 7 | 37, 37 / 36, 37 | expressible with `(NE I ...)` (d-7d2612-8f9340); +1 cell for the pointers, +1 epilogue |
| imp ring (3 points) | 1 / 1 per process | 9 / 8 | 76, 76 / 76, 77 | none but the epilogue; written with constants and label arithmetic (d-7d2612-d9339f) |
| imp spiral (2 waves) | 1 / 1 per process | 13 / 12 | 70.5 / 70.5 (fixed seed) | none but the epilogue |
| quickscan (16 probes) | 1 / 1 per probe on empty core | 73 / 72 | 33.6 / 33.6 (fixed seed) | none but the epilogue: written with templates and `for` (d-7d2612-c4e274), each probe's `if` fused into a skip (d-7d2612-6222c1) |

## Where we are

As of 2026-10-05, end of s-7d2612-11efe5: **`main` holds phases 1 to 6** (phases 4 to 6 merged by
fast-forward that day) and the phase-6 review's minors (i-7d2612-faa781). The gate: 199 alcotest
cases besides `execute`, 44 behaviour specs, the audit's 12 checks. **Next**, none started: unary
skip fusion (i-7d2612-40b941), a Silk-style paper and Mice (i-7d2612-34b61d), the snippets catalogue
(i-7d2612-8e9549, which needs a decision on how a program uses a catalogue: an include item, a
prelude, or a document to copy from), the host pMARS trap (i-7d2612-0cb9e9).

**Phase 6 is built** (s-7d2612-140ece, branch `feat/phase-6` on `feat/phase-5`, merged 2026-10-05): A-field
modes on numbers, labels and expressions (i-7d2612-e98368), `(start label)` emitted as `ORG`
(i-7d2612-e725ef), the macro layer of typed templates and `for` (i-7d2612-ec4d2d), and skip fusion
under the peephole. The quickscan is the tenth archetype; it and the core-clear now play exactly as
their hand-written twins (`docs/research/2026-10-04-benchmark.md`, phase-6 section). **Next:** the
snippets catalogue (i-7d2612-8e9549), a Silk-style paper with `}` (i-7d2612-34b61d), a quickscan
handing over to a paper or stone, unary skip fusion (i-7d2612-40b941).

**Phase 5 is built** (s-7d2612-9b0d20, branch `feat/phase-5` on `feat/phase-4`, merged 2026-10-05):
hills by header or flag, metadata as written, `make bench` against Wilkies and Koenigstuhl's top
20, behaviour specs for every construct, the Core War documentation checked against its sources.
Measured: RED costs nothing against hand-written archetypes; its best warrior places #899 of 1107
on Koenigstuhl's 94nop hill, the gap being strategy
(`docs/research/2026-10-04-benchmark.md`). **Next:** the macro layer (i-7d2612-ec4d2d), which the
quickscan and the snippets wait for; A-field modes on numbers (i-7d2612-e98368); an entry point
(i-7d2612-e725ef).

**Phase 4 and the rest of phase 2 are built** (s-7d2612-333abd, branch `feat/phase-4` from `main`,
merged 2026-10-05): a cell beside a value is its B-field (i-7d2612-9efd00 fixed), constants as `EQU` with
label arithmetic (the imp ring is the eighth archetype), warnings driven by the policy, the phase-1
review's smaller gaps, compile-error goldens, a pointer's step as its net change per lap. **Next:**
compile-time repetition (the macro layer, i-7d2612-ec4d2d) for the quickscan and the snippets;
phase 5, the hills and the benchmark as a check.

As of 2026-10-04, end of s-7d2612-3f3b23. `main` holds phases 1 to 3: it measures what it compiles
(the cost model, i-7d2612-aeab0f), and the policy picks the optimizations it measures best
(d-7d2612-6b110b). The gate runs locally with an opam switch in `_opam/`: `dune build`, 138 alcotest
cases besides `execute` (Linux x86-64 only), an `--emit-beh` spec run end to end in pMARS, the
audit's 12 checks and the behaviour specs, none known-failing since i-7d2612-9efd00 was fixed. Seven
archetypes are written in RED and match their hand-written cycles.
`origin/dev` holds a half-done restructure that defines a different language (i-7d2612-ec4d2d).

**Phase 3 is built** (s-7d2612-f082c8; reviewed, its findings fixed; merged to `main` with phase 2):
optional transformations are chosen by measuring each under the policy (d-7d2612-6b110b); a unary
`while` is rotated (2 cycles per iteration around one instruction, was 3); `(repeat body (store p))`
keeps a variable in the loop's `JMP` (the scanner archetype is 6 cells, the hand-written count); a
generated jump to the next cell is removed. **Next:** phase 2's operator design (i-7d2612-7eadd5:
the indirect-use default, constants and label arithmetic for the imp spiral and quickscan) and
phase 4's warnings.

**Phase 2 has started** (s-7d2612-14641b): seven archetypes are written in RED and measured against
hand-written forms, and all seven now match them in cycles per iteration. On the user's decisions,
jump threading closed the scanner's gap (d-7d2612-3f3f32), conditions accept a modifier so a SEQ
scanner can be written (d-7d2612-8f9340), a labelled bomb `DAT` counts as data, and the epilogue
`DAT` stays (d-7d2612-1c1c67). The epilogue is now exactly
empty core (d-7d2612-f7ae87). **Still open in phase 2:** the operator design (i-7d2612-7eadd5): the indirect-use default, constants
and label arithmetic for the imp spiral and quickscan.

**Phase 1 is complete** (s-7d2612-2c7e4d): the recorded correctness defects are fixed, errors carry
`file:line:col`, and generated labels are reserved. Its branch review found one more, recorded with a
known-failing spec for phase 2's operator design (i-7d2612-9efd00), and five smaller gaps, listed
under phase 1 below.

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
**State.** Done (s-7d2612-333abd): the user decided that a cell beside a value is its B-field
(d-7d2612-891901); `behtests/mixed_operand_field.beh` passes, unmarked, and its golden moved to
`bbctests/examples/`. Found by the phase-1 branch review.
`opmod_to_rmod` decides only when both operands are numbers or variables; a variable beside a plain
reference (`(MOV x (Dir -1))`, `(SLT x label)`) falls to the ICWS'94 default, so with `x` in an
A-field `MOV.I` copies the whole cell instead of `x`'s value. Before phase 1 the fallback was `.I`
for every opcode; user-written `JMZ`/`JMN`/`DJN`, where the variable is the only tested operand, were
fixed in the same review (`jump_modifier`, `behtests/user_djn_afield.beh`).
**Collides with.** Every golden with a variable beside a plain reference (none today but this one).
**Decide first.** What a plain reference means beside a variable: its B-field (so `MOV.AB`,
`SLT.AB`, `MOV.BA` …), its whole cell, or a compile error asking for an explicit modifier — part of
the operator design of phase 2 (i-7d2612-7eadd5).

### A store inside a condition never emitted its label · i-7d2612-4622d8
**State.** Done (s-7d2612-2c7e4d). Found by the phase-1 branch review: `compile_label` ran only for
primitives, so `(if (EQ (store x) 3) ...)` referenced `_LET1` without defining it (pMARS rejects the
warrior). Conditions now label their stores, and a store on the left of `GT` is placed in the
B-field, since `GT` is emitted as `SLT b, a`. Golden and spec `store_in_condition`.

### Fallback modifier .I differs from the ICWS'94 defaults · i-7d2612-96f7b1
**State.** Done (s-7d2612-2c7e4d). `compile_mod` falls back to `Red.default_modifier` (the A.2.1.2
table) instead of `.I`. Two goldens changed, checked line by line (`prog3`, `prog5`: `ADD`/`SUB .I` →
`.AB`); new golden and spec `add_default_modifier` pin the effect in the core. Compound operators
remain for phase 2 (i-7d2612-7eadd5).
Before the fix, when `opmod_to_rmod` could not decide (two references, no variable), the compiler emitted `.I`
(`SLT.I`, `JMZ.I`, `ADD.I #4, #3`). ICWS'94 A.2.1.2 defaults: `SLT`/`JMZ`/`JMN`/`DJN` to `.B`,
`MOV`/`SEQ`/`SNE` to `.I` only when neither operand is immediate, arithmetic with an immediate
A-operand to `.AB`. `SLT.I` requires both `A<A` and `B<B`.
**Collides with.** `prog0.bbc` and every golden with a raw-reference `MOV` (they expect `.I`,
which the default also gives); any golden with `ADD`/`SLT` on two references.
**Decided** (2026-10-03, user): adopt the ICWS'94 table, and consider simple and compound
operators in RED that translate to different modifiers or sequences. Built in subproject C
(i-7d2612-7eadd5); at least `prog3.bbc` and `prog5.bbc` change (`ADD.I #1, #1`, `SUB.I`).

### Smaller gaps from the phase-1 review · i-7d2612-0568a1
**State.** Done (s-7d2612-333abd), each with a test in the `gaps` group: predefined symbols and
names outside `[A-Za-z][A-Za-z0-9_]*` are rejected as labels (generated ones start with `_` and
pass); a syntax error is `file:line:col:` with a 1-based column; "stored twice" points at the second
store; a condition is compiled before its body, so the first error in the source is reported; the
long-line error points at the node that emits the line; `Parse.locations` holds one parse. Was: Each fails loudly or only in unusual input; none miscompiles silently.
- pMARS's predefined symbols (`CORESIZE`, `MAXLENGTH`, `MAXPROCESSES`, `MAXCYCLES`, `MINDISTANCE`,
  `VERSION`, `WARRIORS`, `ROUNDS`, `PSPACESIZE`, case-sensitive) and `CURLINE` are accepted as labels;
  pMARS then rejects the warrior.
- `LANGUAGE.md` documents labels as `[A-Za-z][A-Za-z0-9_]*`, but nothing enforces it (`a-b`, `9`,
  `x#1` are emitted verbatim).
- Error positions: an s-expression syntax error prints `parse error at L:C` with a 0-based column and
  not as `file:line:col:`; "stored twice" points at the `let`, not the second store; a condition and
  its body are compiled right to left, so an error in both reports the body; the long-line error has
  no location.
- `Parse.locations` is never cleared (harmless at today's sizes; clear it per parse).

## Phase 2 — Expressiveness: the archetypes as the acceptance suite

Write the classic warriors in RED and measure each against its hand-written form. What they cannot express decides the compound operators and the constants; the ones that work become the snippet catalogue.

### Classic warriors re-expressed in RED as end-to-end tests · i-7d2612-34b61d
**State.** Half done (s-7d2612-14641b). Seven archetypes — imp, dwarf, stone, core-clear, a `JMZ`
scanner, a SEQ scanner, a paper — are written by hand and in RED (`archetypes/`), with goldens
(`bbctests/archetypes/`), behaviour specs (`behtests/archetype_*.beh`) and the measurements in
`docs/research/2026-10-04-archetypes.md`; an imp ring since constants (s-7d2612-333abd); an imp spiral with two processes per point (s-7d2612-9b0d20); a quickscan written with templates and `for`, scoring exactly as the hand-written one (s-7d2612-140ece). **Still missing:** Mice's
copy-by-index (expressible since `(start label)`, not attempted), Silk-style paper (expressible since A-field modes on numbers, `(} x)`, not attempted). The benchmark is `make bench`
(i-7d2612-f27a91).
Originally planned: Imp, Dwarf, Stone, a countdown core-clear, Mice, an imp spiral, a SEQ scanner,
a Silk-style paper: each exercises a different construct (`docs/references.md`, *Corpora*).
**Collides with.** i-7d2612-96f7b1 and i-7d2612-3744e5 for any warrior that needs them.
**Why it is the north star's measure** (d-7d2612-e006c2). Each archetype gets a hand-written counterpart and a row in the gap table above: cycles per iteration, length and benchmark score, compiled against hand-written. What the archetypes cannot express is what the language lacks.

### Operators, default modifiers, reserved label prefix and the do-while layout (subproject C) · i-7d2612-7eadd5
**State.** Half done (s-7d2612-333abd). Decided and built: the reserved prefix (`_`), the ICWS'94
defaults, the do-while layout (phase 1); a modifier on conditions (d-7d2612-8f9340); what a cell
means beside a value (d-7d2612-891901); constants and expressions (d-7d2612-d9339f). Not designed:
compound operators. No archetype needs one today: the SEQ scanner uses `(NE I ...)`, bombers write
`MOV I`; the quickscan needed repetition, not an operator (written since the macro layer). Carries the decisions recorded in
i-7d2612-fffa6c, i-7d2612-96f7b1 and i-7d2612-425c66.
**Collides with.** d-7d2612-5b410d ends here: C changes emitted code, so every changed golden needs
its behavioural reason (d-7d2612-6a1527), measured with the cost model.
**Decide first.** The reserved prefix; which compound operators exist and what each emits.

### The modifier an indirect use reads in its target · i-7d2612-2581ff
**State.** Done. Conditions accept a modifier (s-7d2612-14641b, d-7d2612-8f9340), and the default
is decided (s-7d2612-333abd, d-7d2612-891901): a pointer's target is a cell, its B-field beside a
value, whole against another cell, so `(NE (Ind a) (Ind b))` is `SNE.I`. Before: `docs/semantics.md` §3 says which field of a variable an
indirect use goes *through* (`@` or `*`), not which field of the target cell it reads. Today the
modifier follows the pointers' fields: `(NE (Ind a) (Ind b))` with `a`, `b` in one `DAT` compiles
to `SNE.AB *a, @b` (A-field of one target against the B-field of the other), and
`(MOV b (Ind b))` to `MOV.B`, which writes one field of the target. A SEQ scanner needs `SNE.I`, a
bomber needs `MOV.I`; before conditions took a modifier, a SEQ scanner could not be written.
**Collides with.** i-7d2612-9efd00 (the same rule for a variable next to a plain reference); every
golden that moves or compares through a pointer without a modifier.
**Decide first.** Whether an indirect use reads the whole target (`.I`) by default, or a compound
operator for the scan.

### The epilogue DAT differs from empty core in its modes · i-7d2612-ed9f79
**State.** Done (s-7d2612-14641b): the epilogue is `DAT $0, $0` (d-7d2612-f7ae87); every golden's
last line changed for that reason, and `behtests/epilogue_empty_core.beh` checks the cell in the
core. Before: The epilogue is meant to be indistinguishable from empty core
(d-7d2612-1c1c67), but pMARS fills empty core with `DAT.F $0, $0` (`pmars.c` in the vendored zip,
lines 165–166) and the epilogue is `DAT.F #0, #0`. A `JMZ`/`JMN` scanner sees both as zero; an
`SEQ.I`/`SNE.I` scanner comparing against an empty cell sees the epilogue.
**Collides with.** Every golden: each ends in the epilogue (d-7d2612-6a1527 needs the reason).
**Decide first.** Whether to emit `DAT $0, $0` (the user's question, 2026-10-04).

### A number cannot take an A-field mode · i-7d2612-e98368
**State.** Done (s-7d2612-140ece): `(* x)`, `({ x)`, `(} x)` and `AInd`, `ADec`, `AInc` (d-7d2612-0e831c). Was: RED's modes pick
the A-field variant (`*`, `{`, `}`) only through a variable stored in an A-field; a number or label
always gets the B variant (`@`, `<`, `>`). The fast Silk-style paper copies through `}`.
**Decide first.** A spelling for the A-field modes (`(AInd x)`, `(ADec x)`, `(AInc x)`), or a mode
written on the variable.

### An entry point other than the first cell · i-7d2612-e725ef
**State.** Done (s-7d2612-140ece): `(start label)` (d-7d2612-3bce15); the core-clear archetype uses it. Was: Execution starts at the first emitted instruction (no `ORG`);
a warrior that keeps a pointer before its code (the hand-written core-clear, Mice's copy-by-index)
is rearranged in RED, which costs the core-clear 4 points against Wilkies.
**Decide first.** A header item `(start label)` emitting `ORG label`, and what Layout and the
metrics take as the entry.

### Constants as named EQU · i-7d2612-a3f2b6
**State.** Done (s-7d2612-333abd): `(const name value)` in the header is an `EQU` line, and operand
expressions `(op a b)` over numbers, constants and labels are emitted for pMARS (d-7d2612-d9339f);
`Layout` evaluates them for the metrics. Constant optimizers (optiMAX, mopt) tune `EQU` constants,
which RED used to inline.

### Compile-error tests · i-7d2612-70ea22
**State.** Done (s-7d2612-333abd): goldens with `STATUS: CT error` in `bbctests/errors/` (three
today). Before: The command line's error path is tested through `Cored.Driver`
(`test_driver_compile_error_is_clean`, `test_driver_missing_file`). Still no golden uses bbctester's
`STATUS: CT error`.

### Snippets: named RED fragments with verified metrics and specs · i-7d2612-8e9549
**State.** Unblocked (s-7d2612-140ece): templates are the reuse mechanism. Still planned: the catalogue itself. A catalogue of RED fragments (imp, bomber loop, scanner, ...), each with its
metrics and a behaviour spec. **Blocked on** a reuse mechanism in the language (subproject C or the
dev branch's lambdas, i-7d2612-ec4d2d).

### Smaller gaps from the phase-6 review · i-7d2612-faa781
**State.** Done (s-7d2612-11efe5, d-7d2612-22c819): all six, each with a `test_phase7_*` case. Was:
planned (s-7d2612-140ece). Found by the branch review, each with a reproducer there;
none miscompiles silently except the first, which pMARS then rejects:
- a template's let binder is renamed throughout its body, scope ignored: `(seq (JMP top) (let (top
  1) ...))` in a template with a global `top` emits `JMP $_X1_top`, an undefined label in pMARS;
- `(for k lo hi ...)` with bounds near the integer limits overflows `hi - lo + 1` and ends in an
  internal error (exit 2) instead of the 1000 limit;
- nested `for`s multiply: `(for i 1 1000 (for j 1 1000 (NOP)))` runs for seconds before the length
  check; a bound on the expanded size would stop it at once;
- the header words (`start`, `hill`, `name`, `author`, `strategy`, `optimize`, `const`) are not RED
  words, so a template may take their name and its call at the top of the body is read as a header
  item;
- the later-template check counts the head of a let binding (`(let (b 3) ...)` reads as a call of a
  template `b`);
- the A-field mode error on a let variable prints the internal name (`_x#1`, `_X1_v`).

### A let in a template named like a parameter loses its binder · i-7d2612-672fff
**State.** Known bug (s-7d2612-11efe5, found by the phase-7 review; on `main` since the macro
layer). `(define (t (x Num)) (let (x 1) (seq (ADD 1 x) ...)))` called as `(t 5)` emits `ADD.AB #1,
#5`: parameters are left out of the per-expansion renaming, then the arguments replace every atom
of their name, the let's binder and its uses included. A silent miscompile. Characterized in
`bbctests/known-bugs/template_let_param.bbc`, `behtests/template_let_param.beh` (known-failing).
**Decide first.** Whether such a let shadows the parameter (rename it like any let binder) or is an
error (a template's let may not take a parameter's name). Shadowing matches RED's let elsewhere.

### Smaller gaps from the phase-7 review · i-7d2612-9aa299
**State.** Planned (s-7d2612-11efe5). Two more error messages print the internal name of a
template's let variable: `Consts` ("variable `_X1_v` cannot be part of an expression") and the `Num`
kind check ("`_X1_v` is a let variable (pass it as a Var)"); `Rename.original` would print `v`.
Inside a template, a let named like an earlier template turns that template's call into "Not a valid
expr: (_X1_b)", where at the top level the call goes to the template.

## Phase 3 — The optimizer, under the policy

Transformations that change emitted code to improve the policy's metric, each measured with `--report` before and after. Speed first: one instruction per iteration is worth about five times eight cells.

### Loop rotation: the condition at the end of the loop · i-7d2612-3ca4c2
**State.** Done (s-7d2612-f082c8). The user decided: unary always, binary when the policy picks it
(d-7d2612-a773b1), chosen by measuring each variant (d-7d2612-6b110b,
`docs/specs/2026-10-04-optimizer-design.md`). A unary `while` around one instruction now runs 2
cycles per iteration instead of 3 (`behtests/while_jn_rotated.beh`); no existing golden changed
(prog8's `LT` is binary, never better rotated). Before:
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
**State.** Half done (s-7d2612-f082c8): the user chose the explicit form, `(repeat body (store p))`
puts `p` in the `repeat`'s `JMP` (d-7d2612-d9e5d3); the scanner archetype is 6 cells, the
hand-written count. Still possible later: the same for `while`'s back jump and other ignored
fields. Before:
A `let` whose `(store x)` sits in a `DAT` of its own costs a cell and, when the `DAT` is on the path,
a `JMP` around it (prog7, prog8: `JMP $2` then `DAT`): one more cell and one more cycle of boot.
The variable can live in a field the program never reads as code — an instruction field the
opcode ignores, such as the B-field of a generated `JMP` — but not in the epilogue, which stays
empty core (d-7d2612-f7ae87).
Not the epilogue `DAT` itself: every archetype is one cell longer than its hand-written form for
it, and the user decided to keep it (d-7d2612-1c1c67).
**Collides with.** d-7d2612-6a1527 (goldens change); i-7d2612-ce4c3b (placement analysis must be right
first).
**Decided** (2026-10-04, user): it may not move one; the user writes where it goes.

### Peephole cleanup of jumps · i-7d2612-ec59a0
**State.** Done. Jumps to jumps are threaded, pulled forward into phase 2 by the user
(s-7d2612-14641b, d-7d2612-3f3f32, `Compile.thread_jumps`); a generated jump to the next cell (an
empty `if`, `else` or rotated `while`) is removed under the policy (s-7d2612-f082c8,
d-7d2612-b9a097, `Compile.peephole`). No existing golden had one.
Jumps to jumps, a `JMP` to the next cell, an `if` whose body is empty: local rewrites over the
emitted sequence, each kept only when `--report` shows the policy's metric improving.
**Measured** (s-7d2612-14641b): the RED scanner's `if` inside a `repeat` jumps to the `repeat`'s
`JMP` on every empty cell, 3 cycles against 2 by hand; threading that one jump by hand took its
benchmark score from 42 to 55 (hand-written: 56). The largest gap the archetypes show; closed by
threading in the compiler, which scores 54 and 58.
**Collides with.** Goldens that contain such sequences.
**Decide first.** Nothing beyond the policy.
**Since** (s-7d2612-140ece, d-7d2612-6222c1): an `EQ`/`NE` `if` around one instruction is fused into
the inverted skip, and a label plus a number counts cells from its label, modulo the core.

### Unary skip fusion · i-7d2612-40b941
**State.** Planned (s-7d2612-140ece). A `JZ`/`JN` `if` around one instruction (`JMN fin, x; X;
fin:`) could be `SNE #0, x; X` (or `SEQ`), as skip fusion does for `EQ`/`NE`: one cell and one
cycle less. **Collides with.** The field the comparison reads: `SNE.AB #0, x` compares with x's
B-field, `SNE.A #0, x` with its A-field, and the modifier rules (d-7d2612-891901) must pick the one
the `JMZ` tested. A `JMZ.F` (both fields zero) has no single-skip form. **Decide first.** Whether the policy may trade a `JMZ.F` for
two skips.

## Phase 4 — Warnings

Once errors have locations and the optimizer knows what it can do, the warnings can say where a cost is and what would remove it.

### Static performance analysis and warnings (subproject B) · i-7d2612-90d6e1
**State.** Done (s-7d2612-333abd): warnings driven by the policy (d-7d2612-4d7c73), `--warn=all|none`.
Not built: a warning for compiler overhead above a construct's minimum *outside* the loop's own
construct (an `if` inside a loop costs its test, which the user wrote). Before: Warnings for possible slowdowns and possible optimizations, from the metrics:
an extra instruction per iteration, compiler overhead above a construct's minimum, a step whose gcd
with CORESIZE leaves cells unvisited, unreachable cells.
**What is already in its favour.** i-7d2612-aeab0f gives every number and the construct that
produced each cell.
**Decide first.** Which warnings are on by default, and whether a policy changes them.

### Unreachable counts a labelled data DAT as dead code · i-7d2612-cf8fdb
**State.** Done (s-7d2612-14641b), the user choosing the first option: a never-executed `DAT` with a
user's label counts as data (`test_phase2_labelled_dat_is_data`); an unlabelled one is still dead code.
Widened by the phase-4 review (s-7d2612-333abd): any never-executed cell an executed instruction
reads or writes is data (an `SPL` bomb, a cell written through `(Dir -1)` or a pointer).
Before: A bomb written `(label bomb) (DAT 0 0)` is never executed,
by design, and `--report` counts it as unreachable code: only a `let` variable's cell counts as
data. An unreachable-cell warning would fire on every bomber (core-clear and scanner archetypes).
**Decide first.** Whether a labelled `DAT` the program never reaches is data, or RED gets a way to
declare data that is not a variable.

### A loop's pointer step is predicted per instruction, not per iteration · i-7d2612-fbe7c8
**State.** Half done (s-7d2612-333abd): a pointer's step is now the sum of its changes over one lap
(`test_net_step`), and nothing is predicted when a change is on some laps only or inside an inner
loop; the paper's outer loop predicts nothing for `d` instead of two wrong steps. Not pursued
(s-7d2612-9b0d20): summing through an inner loop whose trip count is known. The count is known
statically only when nothing resets the counter each lap, which no archetype does (the paper resets
its own); summing through a reset would need data flow (what a `MOV #k, c` leaves each lap). Was: The paper archetype's outer loop moves `d` by `ADD #2365` and
by `<d` seven times in its inner loop: 2358 cells per lap. `--report` predicts two steps for the
outer loop, 2365 and −1, neither of which is what `d` does. The prediction should sum a pointer's
changes over one iteration, inner loops included when their trip count is known.

## Phase 5 — The real world: hills and benchmarks

A warrior that can be submitted: other hills than 94b, the header lines KotH expects, and the benchmark as a regression signal.

### Multiple hill targets · i-7d2612-217183
**State.** Done (s-7d2612-9b0d20): `(hill key)` and `--hill key` (d-7d2612-65fa08) for 94b, 94nop, 94, 94x, tiny and nano.
`--emit-beh` names the hill in the spec it writes, and `tools/behave.py` runs a spec under
`pmars/config/<hill>.opt` (`behtests/hill_tiny_imp.beh`). Not done: the `execute` suite still
assembles everything under 94b. Was: A target parameter (94b, 94nop, tiny,
nano, lp) selecting the header `;redcode-<hill>`, MAXLENGTH, CORESIZE for constants, and whether
`LDP`/`STP` are allowed (94nop has no p-space).
**Collides with.** Every golden's header; the `execute` suite's config path.
**Decide first.** Is the target a CLI flag, a header form in RED, or both?

### Warrior header metadata · i-7d2612-b682d5
**State.** Done (s-7d2612-9b0d20): `(name ...)`, `(author ...)`, `(strategy ...)` as written, `;assert` with a
named hill (d-7d2612-56cfae). Was: Emit `;name`, `;author`, `;strategy` and `;assert` (pMARS warns on every
compiled warrior: "Missing ';assert'"; KotH replies the same).

### Benchmark score as a regression signal · i-7d2612-f27a91
**State.** Done (s-7d2612-9b0d20): `make bench` (d-7d2612-1d4491), baseline in
`tools/bench_baseline.json`. Was: `pmars -b -r 200 -F 4000 warrior bench/*.red` against the Wilkies set gives a
deterministic score in about a second (measured: prog1 55, a classic Dwarf 49, Imp 48).
**Decide first.** The benchmark has no licence statement: fetch at test time into `_build/`, never
vendor.
**Seen in** s-7d2612-14641b: the archetype study scored every warrior this way, with a script that
lived only in `_build/bench/`; its procedure is written out in
`docs/research/2026-10-04-archetypes.md`, so another machine can repeat it.

### The execute suite only assembles warriors · i-7d2612-05c64d
**State.** Done (s-7d2612-9b0d20), as the user chose: behaviour specs for `repeat`, `if-else` (both
branches), `while` with `NE` and `EQ`, indirection through `@`, `<`, `>`, and `SPL`
(`behtests/repeat_counts.beh` ... `spl_two_processes.beh`), each with one probe mutated to see it
fail, and, after the branch review, against the mutants it named (a `while` that never leaves, an
`SPL` removed): the `while` programs now mark their exit; the `execute` suite stays an assembly check. Was: The mechanism is `tools/behave.py` (cdb probes); the content is three
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
**State.** Done as a macro layer (s-7d2612-140ece, d-7d2612-c4e274): typed templates and `for`, rebuilt on `main` as decided;
the dev branch itself is not merged. Was: planned (phase 6); a half-done start on `origin/dev` (2025-09-09), not merged. Moves to `lib/{common,core,parsing,surface}`
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

### Host pMARS traps on some battles · i-7d2612-0cb9e9
**State.** Done (s-7d2612-11efe5, d-7d2612-b153c1). The cause, found with an ASan build in a scratch
copy: `sim.c` formats "Warrior %d: %s terminated - End of round %d" into a 60-byte buffer when an
opponent's `;break` armed cdb and our warrior dies before that breakpoint first runs; a name of about
19 characters overflows it, and macOS's fortified `sprintf` traps (exit 133). Seed-dependent because
the order of the death and the breakpoint is. `tools/pmars-host.sh` patches the extracted copy;
`make check-tools` runs a self-written reproducer (`tools/pmars-trap/`). The same ASan run found two
memory errors that do not trap here, left as they are: `clparse.c:543` writes one past `options[21]`
(`OPTNUM` 21 for 22 options with PERMUTATE, RWLIMIT and PSPACE) and `asm.c:2183` reads `buf[-1]` on
an empty line. The vendored Linux binary has the same overflow, unpatched (assumption: silent there,
no fortify). Was: planned, cause unknown.

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

### Hand counts in tests are wrong before they run · i-7d2612-340f22
**State.** Planned (s-7d2612-3f3b23). Tests first means expected values computed by hand, and they
were wrong before the code ran: columns of error positions (three in phase 1), a plan's value, the
rotated `while`'s death (9 instructions, counted as 8: the entry `JMP` goes to the test).
**Cost.** About two minutes each to rerun and recount, × about 5 a session.
**Proposal.** When a probe fails, read the trace (`run-warrior` skill) before changing the probe or
the code, and say in the changelog which of the two was wrong. No tool needed.
**Seen in.** s-7d2612-2c7e4d, s-7d2612-14641b, s-7d2612-f082c8, s-7d2612-333abd, s-7d2612-140ece
(counted in the log at s-7d2612-11efe5: five sessions; the last, a cell index read from a numbered
listing that counted a blank line).

### Compound commands refused whole by a permission rule · i-7d2612-51fe9d
**State.** Planned (s-7d2612-3f3b23). A command chaining an edit or a check with a denied verb
(`git stash list`, `git checkout -- FILE`) is refused whole, and nothing in it runs; four times in
these sessions.
**Cost.** One retry each (about a minute), and once a mutation check silently not run.
**Proposal.** Restore files with `git show HEAD:FILE > FILE`, park work as a patch in the job's
scratch directory, and keep denied verbs out of chains (the global rule already says so).
**Seen in.** s-7d2612-2c7e4d, s-7d2612-14641b, s-7d2612-f082c8, s-7d2612-9b0d20 (counted in the log at
s-7d2612-11efe5).

### One-concern commits split by hand-staged blobs · i-7d2612-f5490f
**State.** Planned (s-7d2612-3f3b23). Four times this session several fixes landed in the same files
before committing, and each commit's version had to be rebuilt from `HEAD` and staged with
`git hash-object` and `git update-index`.
**Cost.** About ten minutes each, × 4, plus the risk of a commit that never compiled alone (each was
checked in a temporary worktree).
**Proposal.** Commit each concern as soon as its tests pass, before starting the next; a review's
findings one at a time. No tool needed.
**Seen in.** s-7d2612-14641b, s-7d2612-f082c8.

### Mutation scripts that do not apply · i-7d2612-138e9e
**State.** Planned (s-7d2612-3f3b23). A mutation that silently did not apply reports a passing test
as if the guard were untested: Perl regex parentheses, a split on `|` (OCaml or-patterns contain
it), and an unused variable the `dev` profile rejected.
**Cost.** About three minutes each, × 3, and a false conclusion if unnoticed.
**Proposal.** A literal-replacement helper in `tools/` that fails unless the text occurs exactly
once, builds, runs one test group and restores the file. A tooling change: its own commit, when
scheduled.
**Seen in.** s-7d2612-14641b, s-7d2612-f082c8.

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
