# Planned work

Accepted ideas and recorded defects, not yet built. **Not a promise and not a work order**: this is
where each one collides with something, written while it is clear. Every entry has a state —
**Planned**, **Half done**, **Done**, **Closed by measurement**, **Blocked outside** — and no entry
is ever deleted. Finishing an item edits its entry in the same change. New items get an id from
`python3 .agents/tools/bundle.py id i "<idea>"`, written after the `·`.

The core invariant every item is priced against: **a RED program compiles to redcode whose
execution in the core follows the program's meaning** (`docs/semantics.md`).

## Where we are

As of 2026-10-03 (s-7d2612-0a037e, s-7d2612-cc9344). `main` compiles the RED constructs in
`LANGUAGE.md`, and the whole gate passes in CI: `dune build` and 29 tests (1 parse, 14 compare,
14 execute) on OCaml 5.5.1, plus `make check-tools`. **Seven defects are recorded** below, five with
a failing check (four behaviour specs, one audit check), and the `execute` suite only proves that
pMARS assembles the output. A behaviour harness exists now (`tools/behave.py`, 3 passing specs,
4 known-failing). On this machine only `make check-tools` runs (no opam switch).
`origin/dev` holds a half-done restructure that defines a different language (i-7d2612-ec4d2d).
Nothing is in motion beyond that branch.

**Next, by cost to the invariant and verifiability:** the four correctness fixes with known-failing
specs (each is local, test-first, and its spec already exists), then i-7d2612-47d3ea and
i-7d2612-8f3f22 so the OCaml gate runs anywhere.

## Correctness — recorded defects

### do-while with GT or LT loops at equality · i-7d2612-fffa6c
**State.** Planned. Known-failing: `behtests/dowhile_gt_equal.beh`, golden
`bbctests/known-bugs/dowhile_gt_equal.bbc`.
`compile_cond2` in post-condition mode emits `SLT a1, a2; JMP head` for `GT`, which repeats while
`a1 >= a2`; `LT` repeats while `a1 <= a2`. Pre-conditions (`if`, `while`) are correct. Measured: with
`x = y = 3` the compiled loop is still running after 10 instructions; ICWS'94 `SLT` is strict.
**Collides with.** Every golden that contains a `do-while` with `GT`/`LT` (none today).
**Decide first.** The layout: invert with an extra skip (one more instruction per iteration) or
reuse the pre-condition layout at the loop's end.

### Unary conditions always use the .B modifier · i-7d2612-3744e5
**State.** Planned. Known-failing: `behtests/cond1_afield.beh`.
In `compile_cond` the label operand of `Cond1` is a reference, so `opmod_to_rmod` falls through to
the `.B` default: a variable stored in an A-field is tested on the B-field of its cell.
**Collides with.** Goldens whose unary condition reads a B-field variable must not change.
**Decide first.** Nothing: the field is known from `penv`.

### Fallback modifier .I differs from the ICWS'94 defaults · i-7d2612-96f7b1
**State.** Planned. No spec yet.
When `opmod_to_rmod` cannot decide (two references, no variable), the compiler emits `.I`
(`SLT.I`, `JMZ.I`, `ADD.I #4, #3`). ICWS'94 A.2.1.1 defaults: `SLT`/`JMZ`/`JMN`/`DJN` to `.B`,
`MOV`/`SEQ`/`SNE` to `.I` only when neither operand is immediate, arithmetic with an immediate
A-operand to `.AB`. `SLT.I` requires both `A<A` and `B<B`.
**Collides with.** `prog0.bbc` and every golden with a raw-reference `MOV` (they expect `.I`,
which the default also gives); any golden with `ADD`/`SLT` on two references.
**Decide first.** Is `.I` a deliberate RED rule? If yes, document it in `LANGUAGE.md` and
`docs/semantics.md`; if no, adopt the ICWS'94 table and write a spec per opcode family.

### User labels can collide with generated labels, and store-once is unchecked · i-7d2612-425c66
**State.** Planned. Known-failing: `behtests/label_collision.beh`.
A user `(label LET1)` shares the namespace of generated labels; pMARS keeps the first definition
and only warns. Unchecked as well: a `let` whose variable has no `(store x)` (its `LET` label is
never defined) or two (defined twice), a user label that is a pMARS reserved word (`END`, `MOV`).
**Collides with.** d-7d2612-123e41 (label names are part of every golden).
**Decide first.** Reject colliding user labels in the parser, or give generated labels a prefix
users cannot write (pMARS labels are `[A-Za-z_][A-Za-z0-9_]*`, case-sensitive).

### An inner let leaks its store placement into an outer variable of the same name · i-7d2612-ce4c3b
**State.** Planned. Known-failing: `behtests/let_shadowing.beh`.
`Analyse.analyse_store_expr` walks into a nested `ELet` that rebinds the same name, so the inner
`(store x)` sets the outer `x`'s field. Related, not yet measured: `replace_store` resolves an
initializer in the environment of the store site, so `(let (x 1) (let (y x) (let (x 2) … (store y))))`
captures the inner `x`.
**Decide first.** Stop the walk at a shadowing `let`, or uniquify names before analysis.

### Four distinct CTError exceptions, none caught · i-7d2612-888db5
**State.** Planned. Known-failing check: `tools/audit.py` `single-error-type`.
`lib`, `util`, `parse` and `compile` each declare `exception CTError of string`; they are four
exceptions, `run_compile` catches none, and a user error ends as an uncaught exception (exit 2).
`util.ml` also raises "please report this bug" through the same type.
**Decide first.** One user-error exception (with a location, i-7d2612-1703ff) plus `failwith`-style
internal errors, converted to a message and exit code in the driver.

### Lines of 256 characters or more hang pMARS · i-7d2612-174acf
**State.** Planned. No spec (a probe would hang the gate).
Measured on pMARS 0.9.4: a 245-character label hung, 200 worked. RED passes user labels through.
**Decide first.** A maximum label length in the parser, or a check on emitted line length.

## Testing

### The execute suite only assembles warriors · i-7d2612-05c64d
**State.** Half done. The mechanism is `tools/behave.py` (cdb probes); the content is three
specs for working programs. Missing: specs for `repeat`, `if-else`, `while` with `EQ`/`NE`,
indirection, `SPL`; and folding them into the OCaml suite if wanted.

### Compile-error tests · i-7d2612-70ea22
**State.** Planned. bbctester supports `STATUS: CT error` with EXPECTED as a pattern; no golden
uses it. Blocked in practice by i-7d2612-888db5 (errors escape as exceptions).

### Classic warriors re-expressed in RED as end-to-end tests · i-7d2612-34b61d
**State.** Planned. Imp, Dwarf, Stone, a countdown core-clear, Mice, an imp spiral, a SEQ scanner,
a Silk-style paper: each exercises a different construct (`docs/references.md`, *Corpora*).
**Collides with.** i-7d2612-96f7b1 and i-7d2612-3744e5 for any warrior that needs them.

### Benchmark score as a regression signal · i-7d2612-f27a91
**State.** Planned. `pmars -b -r 200 -F 4000 warrior bench/*.red` against the Wilkies set gives a
deterministic score in about a second (measured: prog1 55, a classic Dwarf 49, Imp 48).
**Decide first.** The benchmark has no licence statement: fetch at test time into `_build/`, never
vendor.

### A RED reference interpreter for differential testing · i-7d2612-56302d
**State.** Planned. Run RED source and compiled redcode on the same initial core and compare cells
(translation validation, `docs/semantics.md`). Later, QCheck-generated programs.

## Toolchain

### dune runtest runs no tests · i-7d2612-47d3ea
**State.** Planned. There is no `(test)` stanza; `make tests` is the only entry. Adding one changes
how CI and editors run the suite.

### Reproducible install of bbctester · i-7d2612-8f3f22
**State.** Planned. The code needs the BBCStepTester fork (public name `bbctester`, commit
2cb3669), which has no `.opam` file; pleiad/BBCTester has an incompatible API under the same name.
CI clones and `dune install`s it into setup-ocaml's local switch (works since s-7d2612-cc9344). An opam file with `pin-depends`, or a
vendored submodule with `(vendored_dirs ...)`, would make it one command.

### The Docker image cannot run the vendored pmars or build the tests · i-7d2612-202da9
**State.** Planned. `ocaml/opam:ubuntu-20.04` ships glibc 2.31; `pmars/pmars` needs 2.34 and
`libX11.so.6`. The image also installs neither bbctester nor its dependencies, and pins OCaml 5.0.0
while this machine has 5.5.1. Not verified by running Docker.

### pMARS 0.9.5 · i-7d2612-494e75
**State.** Planned. Released 2026-01-03 with overflow and bounds fixes; builds on macOS without the
`round` rename. **Collides with** d-7d2612-3d04ba (the vendored binary and zip are 0.9.4).

### Adopt ocamlformat · i-7d2612-2a14f2
**State.** Planned. Needs a pinned `.ocamlformat` (`version=`), one formatting commit with nothing
else in it (d-7d2612-8cdc44), then `dune build @fmt` in the gate.

### The cored profile in dune-workspace is never selected · i-7d2612-ceee87
**State.** Planned. `(env (cored ...))` applies only under `--profile cored`, which nothing passes;
builds use the `dev` profile, where unused opens and values are errors. Decide whether warnings are
errors, then make the file say so.

## Language and output

### Multiple hill targets · i-7d2612-217183
**State.** Planned; the stated direction (d-7d2612-6d88cd). A target parameter (94b, 94nop, tiny,
nano, lp) selecting the header `;redcode-<hill>`, MAXLENGTH, CORESIZE for constants, and whether
`LDP`/`STP` are allowed (94nop has no p-space).
**Collides with.** Every golden's header; the `execute` suite's config path.
**Decide first.** Is the target a CLI flag, a header form in RED, or both?

### Warrior header metadata · i-7d2612-b682d5
**State.** Planned. Emit `;name`, `;author`, `;strategy` and `;assert` (pMARS warns on every
compiled warrior: "Missing ';assert'"; KotH replies the same).

### A static kind check for RED · i-7d2612-f2f7c5
**State.** Planned. Kinds `Num`, `Lab`, `Place` (`docs/semantics.md`, *Statics*): jump targets are
labels, `#x` is an explicit offset, every let variable is stored exactly once. Subsumes the
store-once half of i-7d2612-425c66.

### Constants as named EQU · i-7d2612-a3f2b6
**State.** Planned. Constant optimizers (optiMAX, mopt) tune `EQU` constants; RED inlines them.

### Source locations in compile errors · i-7d2612-1703ff
**State.** Planned. Needs a position-aware s-expression reader and one annotated AST type.

### The new compiler structure on the dev branch · i-7d2612-ec4d2d
**State.** Half done, on `origin/dev` (2025-09-09), not merged. Moves to `lib/{common,core,parsing,surface}`
and `bin/`, adds an opam file, disables `execs/`.
**Collides with.** d-7d2612-123e41 (it adds a global `gensym`); RED itself (its surface language is
a simply typed lambda calculus, not RED); every test (none run on the branch). Known defects there:
`List.hd` on an empty `seq` in `typecheck.ml`, unbound names raise `failwith`.
**Decide first.** Is the lambda-calculus surface a new front end that lowers to RED, or a
replacement? That answer decides whether this branch is merged, rebased, or restarted.

## Documentation

### RED tutorial · i-7d2612-e8f3f0
**State.** Planned. `TUTORIAL.md` is a title only. Programming games that teach assembly (TIS-100,
EXAPUNKS) teach through small constrained goals with cycle and size counts; a tutorial built from
the classic-warriors item (i-7d2612-34b61d) would reuse those programs.

## Process and tooling

Friction goes here once it has been hit twice, with the arithmetic. Nothing yet.

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
