# AGENTS.md

corewars-compiler compiles **RED**, a small s-expression language with `let`, `store` and
structured control flow (`repeat`, `if`, `while`, `do-while`), into **ICWS'94 Redcode** for Core
War, played in pMARS and on KotH hills. It is an OCaml/dune project of about 800 lines, started from
a university compilers course template. What is unusual: the target has no separation between code
and data — every RED variable is a field of an instruction that can be executed or overwritten — so
a compiler bug rarely crashes anything; it produces a warrior that assembles and quietly does the
wrong thing. This file is the single source for every assistant; `CLAUDE.md` only imports it.

## Non-negotiable constraints

Each rule: what breaks, and what catches it. `—` means nothing catches it yet. Decisions are in
`docs/decisions.md`; the semantics they protect are in `docs/semantics.md`.

**Code generation** (the domain invariants; `docs/semantics.md` states each formally)
- **Generated labels derive from AST tags (`Ast.tag_expr`), never from a global counter**
  (d-7d2612-123e41). A counter makes the output depend on how many programs the process compiled
  before, and bbctester compiles every golden in one process. Enforced: `tools/audit.py`
  (`no-global-counter`), the `compare` suite.
- **Every comparison keeps its strict meaning in every layout** (`if`, `while`, `do-while`),
  unsigned modulo CORESIZE (d-7d2612-2a4435). `SLT` is strict `<`; a post-condition that loops at
  equality is a different program. Enforced: `behtests/` (`dowhile_*.beh`, `prog8_while_lt.beh`).
- **A variable is read and written through the modifier that selects its field** (A or B, decided
  by where its `(store x)` sits). The wrong modifier reads the other field of the same cell.
  Beside it, a cell (a plain reference, or a pointer's target) is its B-field; two cells are read
  whole (d-7d2612-891901). Enforced: `behtests/` (`cond1_afield.beh`, `user_djn_afield.beh`,
  `let_shadowing.beh`, `mixed_operand_field.beh`, `prog5_pointer_target.beh`), the `cells` group.
- **With no modifier given, the variables' fields decide; where they do not, the ICWS'94 default**
  (`Red.default_modifier`, d-7d2612-7aad26), so the compiled code does what the same redcode written
  by hand does in pMARS. Enforced: `test_phase1_icws_default_modifiers`,
  `behtests/add_default_modifier.beh`.
- **Generated labels never collide with user labels or pMARS reserved words.** pMARS only warns on
  a redefinition and keeps the first one. Generated labels start with `_`, which user names may not
  (d-7d2612-bd5def). Enforced: the parser (`test_phase1_reserved_prefix_rejected`),
  `behtests/label_collision.beh`.
- **A warrior fits the target's MAXLENGTH (100 on 94b), epilogue included, and every line is under
  256 characters** (longer lines hang pMARS). Enforced: the `execute` suite for length;
  `Compile.compile_prog` rejects a long line (`test_phase1_long_line_is_an_error`).
- **Execution starts at the first emitted instruction** (no `ORG`/`END` is emitted). Enforced: —.
- **A rewrite after emission (jump threading, the peephole) never passes or removes a cell that
  does more than jump**: a user's `JMP` or label, a variable, an operand that moves a pointer, a
  cell a skip or a numeric offset counts (d-7d2612-3f3f32, d-7d2612-b9a097). Both phase reviews
  found a miscompile here. Enforced: the `review2`, `review3` and `phase3` groups, each guard
  mutated.

**Tests**
- **A golden is what `run_compile.exe` prints with the default policy**: `Optimize.choose` picks the
  optional transformations by measuring them (d-7d2612-6b110b), and the `compare` suite compiles
  through it. `Compile.compile_body` alone is the unoptimized baseline. Enforced:
  `test_emit_text_unchanged`, `test_phase3_driver_uses_the_choice`.
- **Goldens compare byte for byte, padding included** (d-7d2612-5f2a0b), **and an EXPECTED section
  changes only with a stated behavioural reason** (d-7d2612-6a1527): a regenerated golden certifies
  whatever the compiler now does. Enforced: the `compare` suite; the reason: — (say it in the
  changelog entry).
- **Known bugs are recorded, not fixed in passing** (d-7d2612-c5bb3c): a characterization golden in
  `bbctests/known-bugs/`, a `known-failing` spec in `behtests/`, a roadmap item. When a fix makes the
  spec pass, `tools/behave.py` fails until the mark and the roadmap item move in the same change.
- **The target settings are 94b** (`pmars/config/94b.opt`; d-7d2612-6d88cd). Other hills are a
  roadmap item (i-7d2612-217183), not a flag to flip in one test.

## Guardrails that are NOT relaxed

- **The class of bug this repo cannot see is redcode that assembles, matches its golden and behaves
  wrongly in the core.** The `execute` suite runs pMARS with `-A`, which only assembles
  (i-7d2612-05c64d). Any change to code generation runs `python3 tools/behave.py` and adds a spec
  for the construct it touched; a report that claims a construct works names the spec that ran.
- **`pmars/pmars` is a Linux x86-64 binary on purpose** (d-7d2612-3d04ba): it serves the `execute`
  suite on Linux. On macOS it cannot run (exit 126); observe warriors with `tools/pmars-host.sh`,
  which builds `_build/pmars-host/pmars` from the vendored zip, and the `run-warrior` skill.
- **`docs/semantics.md` is the specification; the compiler is not.** When code, golden and comment
  disagree, none of them is the spec: go back to the rule there, or to ICWS'94.

## Files you should not hand-edit

- `pmars/**` — third-party pMARS, binary and source zip (d-7d2612-472634). `permissions.deny`.
- `.agents/**` except `.agents/carrier.toml` and new files in `.agents/proposals/` — the bundle's
  release; `bundle.py verify` fails on any change. `permissions.deny`.
- `_build/**` — dune's and the tools' output.

## Commands

From `Makefile` unless noted. The OCaml half needs an opam switch with `dune`, `containers`,
`alcotest` and `bbctester` (the BBCStepTester fork; `REFERENCE.md`).

```sh
make check                      # THE GATE: check-tools, then check-ocaml
make check-tools                # audit + behaviour specs + bundle checks; needs only Python 3.11+ and cc
make check-ocaml                # dune build + the test suites (execute only on Linux x86-64)
make tests F=compare            # dune exec execs/run_test.exe -- test '<F>'  (ctests: compact)
make compile src=examples/prog1.src   # print the redcode for one RED file
dune exec execs/run_compile.exe -- --report examples/prog1.src   # + metrics and predictions on stderr
dune exec execs/run_compile.exe -- --emit-beh _build/p.beh examples/prog7_expect.src   # (expect ...) as a .beh
python3 tools/behave.py behtests/prog8_while_lt.beh   # one behaviour spec
tools/pmars-host.sh             # build pMARS for this machine into _build/pmars-host/
```

## Verification

- `make check` is the floor. Without an opam switch, run `make check-tools` and **say in the report
  that the OCaml half did not run**; CI (`.github/workflows/ci.yml`) runs both.
- After touching code generation: `make tests F=compare`, `python3 tools/behave.py`, and look at the
  emitted redcode for the construct (`make compile`), stepping it in cdb when the claim is about
  behaviour (`.claude/skills/run-warrior/`).
- A claim needs a measurement. "Fixed" means the known-failing spec now passes and its mark is gone.

## Committing

Nothing is committed unless the user asks. One concern per commit; a subject that needs an "and" is
two commits. Every commit passes `make check-tools`, and `make check` wherever a switch exists. A
process or tooling change never rides in a feature commit. Commit messages follow the existing
history: `feat:`, `fix:`, `docs:`, `test:`, `chore:`.

## Engineering standards

- **Tests first.** The expected behaviour exists before the code: a golden for the text, a `.beh`
  spec for what it does, an alcotest case for a function. Watch it fail for the reason you expect. A
  test written after a fix is checked by mutation: break the fix, see that test go red, restore.
- **Characterization is labelled.** A golden recording current, wrong output lives in
  `bbctests/known-bugs/` and names its roadmap item; it is never mistaken for a specification.
- **Types.** dune's `dev` profile is the strictness level: unused opens and values and
  non-exhaustive matches are errors (the `cored` profile in `dune-workspace` is never selected,
  i-7d2612-ceee87). Never silence a warning or add a `| _ ->` to make a build pass: a wildcard
  hides every constructor added later, as the `| _, _ -> rmod` fallback in `opmod_to_rmod` showed
  (it silently produced `.I`; i-7d2612-96f7b1).
- **OCaml 5.5 reserves `effect`** (effect handlers): it cannot name a value.
- **Errors.** A user's mistake is `Ast.Error` (raise it with `Ast.error msg`; `compile_expr` adds the
  node's location), printed `file:line:col: error: ...` with exit 1; an impossible state is
  `failwith`, printed as an internal error with exit 2 (d-7d2612-8bba52). No other `*Error`
  exception: `tools/audit.py` (`single-error-type`).
- **Comments say why**, in the density of the file you are in; a number carries the measurement
  that produced it. No ticket ids or "previously" in code comments: that belongs in the changelog.
- **Domain non-negotiables:** every emitted instruction costs a cycle and one of MAXLENGTH cells; a
  construct's cost in instructions is documented in `LANGUAGE.md` and changing it is a decision.

## What changed → what must move, in the same change

| You changed | Also update |
|---|---|
| a construct's emitted code or its cost | `LANGUAGE.md`, `docs/semantics.md` §5, the goldens (with the reason), a `.beh` spec |
| a rule, or a settled question | `docs/decisions.md`: a new row with a `d-` id and its enforcer |
| a recorded bug's status | its `known-failing:` mark or `KNOWN_FAILING` entry, and its roadmap entry's state |
| a pass, an IR, a module, a label prefix | `docs/architecture.md` (the audit checks the prefixes) |
| a command, a Makefile target, a flag | every document that quotes it: this file, `REFERENCE.md`, the skills |
| something the roadmap planned | that entry's state, and what is still missing |
| anything | `.claude/logs/agent-changelog.md` |

## Logging obligation

Every session that changes the repository adds an entry, newest first, to
`.claude/logs/agent-changelog.md`, with an id from `python3 .agents/tools/bundle.py id s "<title>"`.
Parallel sessions cannot see each other; the log is how one warns the next. Write what went wrong on
the way and what was left undone, not only what worked.

## Working style

- Explain trade-offs in this repo's units: instructions emitted, cycles per loop, warrior length,
  goldens touched. Ask before structural changes; extend a module before adding one.
- Write the test first (a golden, a behaviour spec, an alcotest case) and watch it fail.
- Documents in this repository are in English (d-7d2612-cbb430); the conversation follows the user.
- New decision rows, roadmap items and changelog entries get ids from `bundle.py id d|i|s TEXT`,
  never the next number.

## The documents, and which one answers what

| Question | Document |
|---|---|
| What does a RED construct mean? What must the compiler preserve? | `docs/semantics.md` |
| What is the syntax, for a user writing RED? | `LANGUAGE.md` |
| Where is each pass, what does each IR hold, where does a new file go? | `docs/architecture.md` |
| Why is it done this way? Was this already decided? | `docs/decisions.md` |
| What is planned, what is broken, what collides with what? | `docs/roadmap.md` |
| What does the outside world (ICWS'94, pMARS, hills, literature) say? | `docs/references.md` |
| How close is RED to hand-written redcode? What was measured, and how? | `docs/research/` (dated notes; the archetypes: `docs/research/2026-10-04-archetypes.md`) |
| How do I install the toolchain and run the tests? | `REFERENCE.md` |
| How do I run a warrior and watch it execute? | `.claude/skills/run-warrior/SKILL.md` |
| A warrior misbehaves or pMARS rejects it — what is known? | `.claude/skills/troubleshoot-redcode/SKILL.md` |
| What changed recently, and what did it leave undone? | `.claude/logs/agent-changelog.md` |
| A change touches state, a contract, data, security or verification | look it up in `.agents/knowledge/INDEX.md` before a design decision and open only the cards it links; decide each from its claim and where it stops applying, run its check before claiming done, open a full note only when its boundary is unclear here. Where this file states an invariant that contradicts a note, this file wins and the report says so. Only when the user asks for a review in a fresh context or names the reviewer, give the diff to the `knowledge-reviewer` subagent (`.claude/agents/`) and wait for it |
| A procedure in `.agents/method/` says otherwise | this repository's own procedure wins (its tracker, logs, gate, commit rules); `.agents/carrier.toml` `adapted` records the mapping |
| Privacy | nothing written into `.agents/` or into any file that leaves this repository may identify, directly or by reconstruction, a private repository, its people or its users; `python3 .agents/tools/bundle.py privacy .agents` checks it |
