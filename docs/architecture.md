# Architecture

How the compiler is laid out, what each pass hands the next, and where a new file goes. The
meaning each pass must preserve is in `docs/semantics.md`; settled choices are in
`docs/decisions.md`.

## The tree

| Path | What it holds |
|---|---|
| `src/` | the library `cored` (`src/dune`): every compiler pass |
| `execs/run_compile.ml` | the CLI: `run_compile.exe <file.src>` prints redcode to stdout |
| `execs/run_test.ml` | the test runner: alcotest unit tests, then the bbctester suites `compare` and `execute` |
| `bbctests/examples/*.bbc` | goldens for working programs |
| `bbctests/known-bugs/*.bbc` | characterization goldens: the current output of a program that triggers a recorded bug |
| `bbctests/errors/*.bbc` | goldens for compile errors (`STATUS: CT error`; EXPECTED is a regular expression searched in the error) |
| `bbctests/archetypes/*.bbc` | goldens for the classic archetypes written in RED (phase 2's acceptance suite) |
| `behtests/*.beh` | behaviour specs, run by `tools/behave.py` |
| `examples/*.src` | RED programs to compile by hand (`make compile src=...`) |
| `archetypes/` | each classic archetype twice: `NAME.red` written by hand, `NAME.src` in RED; measured in `docs/research/2026-10-04-archetypes.md` |
| `docs/research/` | dated research notes: what was measured, how, and what it found |
| `pmars/` | third-party pMARS: the Linux binary, its configs (`config/94b.opt`) and the source zip |
| `tools/` | the gate's own scripts: `audit.py`, `behave.py`, `pmars-host.sh` |
| `docs/` | the documents in the map of `AGENTS.md` |

## The pipeline

```
RED text ──CCSexp──▶ sexp ──Parse.parse_source──▶ Ast.source ──Ast.tag_expr──▶ tag eexpr
   ──Compile.compile_expr (Analyse, Lib, Util)──▶ Compile.emitted list ──Red.pp_instrs──▶ redcode text
     (once per set of Compile.options; Optimize.choose keeps the variant the policy prefers)
                                                       └──Layout.build──▶ Layout.program ──Metrics.measure──▶ Metrics.t
                                                                                          └──Expect.check──▶ pass / fail
```

The analysis half (`Layout`, `Metrics`, `Expect`) never changes the redcode (d-7d2612-5b410d).

| Module | Role | Representation it produces |
|---|---|---|
| `src/parse.ml` | s-expression → AST; the optional `(program ...)` header; rejects unknown forms with `Ast.Error` at the form's line and column (the located reader records each node's position) | `Ast.source`, `Ast.expr` (= `loc eexpr`) |
| `src/ast.ml` | the annotated AST `'a eexpr`, `loc`, `meta = { tag; loc }`, the one user error `Error`; `tag_expr` numbers every node in pre-order from 1 | `meta eexpr` (tags feed label names) |
| `src/consts.ml` | `resolve`: a constant used as an operand becomes an immediate expression, an expression without a mode gets one (immediate without labels, direct with), and a `let` or label named after a constant, or a variable inside an expression, is an `Ast.Error` | `Ast.expr` |
| `src/rename.ml` | `uniquify`: every `let`-bound variable gets a name no other binder uses (`x`, then `_x#1`, … in the reserved `_` space, so no user name can equal one; messages show the original), so initializers resolve where they are bound; the tree keeps its shape, so tags are unchanged | `Ast.expr` |
| `src/analyse.ml` | per `let`: finds the field (`PA`/`PB`) where `(store x)` sits | extends `penv` |
| `src/lib.ml` | environments `aenv` (name → initializer), `penv` (name → field), `lenv` (name → label); `jump_label` | `env` |
| `src/util.ml` | operand lowering through three small IRs: `darg` (number or label) → `carg` (constant, label, variable or pointer) → `Red.rarg`; and modifier choice `opmod` → `Red.rmod` | `Red.rarg`, `Red.rmod` |
| `src/compile.ml` | control flow and conditions to labels and jumps, each instruction annotated with its origin tag, generating construct and stored variables; `thread_jumps` aims a generated jump that lands on a generated `JMP` at that `JMP`'s target; with `Compile.options`, rotates a `while` and removes generated jumps to the next cell (`peephole`); `compile_prog` adds the header and the epilogue `DAT` | `Compile.emitted list`, then text |
| `src/layout.ml` | positions: labels resolved to offsets, cells with roles and variables, successors, loops (each described by the construct owning the loop-head label it closes on), label and line-length diagnostics | `Layout.program` |
| `src/optimize.ml` | measure and choose (d-7d2612-6b110b): compiles the program once per combination of `Compile.options`, measures each and keeps the one the policy prefers, ties to the fewest transformations; the driver and the `compare` suite compile through it | `Compile.options`, the chosen `emitted list` and its `Metrics.t` |
| `src/metrics.ml` | static metrics, step and counter predictions, the optimization policy, text and JSON reports | `Metrics.t` |
| `src/expect.ml` | collects `(expect ...)` with their enclosing loop, checks the static ones, writes the execution ones as a behaviour spec | `Expect.outcome`, `.beh` text |
| `src/warnings.ml` | the cost warnings (d-7d2612-4d7c73): from the chosen variant's metrics, whether a faster variant exists, and the expectations, each warning at the location of the node that emitted the cost | `Warnings.warning list` |
| `src/driver.ml` | the command line as a function: arguments in; standard output, standard error, files to write and exit code out; an `Ast.Error` becomes `file:line:col: error: ...` and exit 1, a `Failure` an internal error and exit 2 | `Driver.output` |
| `src/red.ml` | the Redcode target: opcodes, modes, modifiers, and the pretty-printer that fixes the column padding | text |

**Dependency direction:** `red` ← `ast` ← `consts` ← `rename` ← `lib` ← `util` ← `analyse` ← `compile` ← `layout` ← `metrics`
← `optimize` ← `expect` ← `warnings` ← `driver`; `parse` depends only on `ast`. `execs/run_compile.ml` only performs what
`Driver.run` returns. dune rejects cycles, so the direction cannot invert silently; a new module states where it
sits in this chain.

## Generated labels

Every label the compiler invents is a prefix plus the tag of the node that produced it
(d-7d2612-123e41). The prefixes in use:

| Prefix | Produced by |
|---|---|
| `_LET` | `let`: labels the cell that holds the variable (where `(store x)` sits) |
| `_REP` | `repeat`: loop head |
| `_IF` | `if` without else: end label |
| `_IFM`, `_IFF` | `if` with else: else-branch label, end label |
| `_WHI`, `_WHF` | `while`: loop head, end label |
| `_WHC` | rotated `while`: the test after the body (d-7d2612-a773b1) |
| `_DWH` | `do-while`: loop head |

User names may not start with `_`, so no user label can take one of these (d-7d2612-bd5def).
Changing the tag numbering or a prefix changes every golden that contains one: that is a change to
the output contract, not a refactor.

## Tests and where a new one goes

| You want to show | Add | Run |
|---|---|---|
| the exact redcode a program compiles to | a `.bbc` in `bbctests/examples/` (copy `bbctests/examples/prog2.bbc`) | `make tests F=compare` |
| what the compiled warrior does in the core | a `.beh` in `behtests/` pointing at a golden (copy `behtests/prog8_while_lt.beh`), or `(expect (alive N))`-style expectations in the RED source exported with `run_compile.exe --emit-beh` (a spec with `redcode:` instead of `golden:`) | `python3 tools/behave.py` |
| that a program is rejected, and with which message | a `.bbc` in `bbctests/errors/` with `STATUS: CT error` (copy `bbctests/errors/stored_twice.bbc`) | `make tests F=compare` |
| a bug, before fixing it | a `.bbc` in `bbctests/known-bugs/` with the current output, a `.beh` marked `known-failing: <roadmap id>`, and the roadmap item | both of the above |
| a function's result | an alcotest case in `execs/run_test.ml` (`ocaml_tests`; groups `parse`, `emit`, `layout`, `metrics`, `policy`, `expect`, `review`, `minor`, `driver`) | `make tests F=metrics` |
| a metric, prediction or expectation message | an alcotest case with the exact value (copy `test_metrics_prog8`) | `make tests F=metrics` |
| what the command line prints, writes or exits with | a `driver` case through `Driver.run` with an in-memory `read` (copy `test_driver_expectation_fails`) | `make tests F=driver` |

The `.bbc` format (BBCStepTester): `NAME:`, `DESCRIPTION:`, optional `PARAMS:` and `STATUS:`
(`CT error`, `RT error`), `SRC:`, `EXPECTED:`, optional `END`. No trailing newline after the last
EXPECTED line. bbctester splits on those words anywhere in the file, so no golden may contain
the text `END` elsewhere (a `(label END)` breaks the parse). With `STATUS: CT error`, EXPECTED is a
regular expression searched in the compile error (`bbctests/errors/`). bbctester finds `*.bbc` recursively under `bbctests/`, so every golden runs in both
`compare` and `execute`.

## Exemplary files

- A pass: `src/compile.ml` — one function per construct, exhaustive `match`, errors via `Ast.error`.
- A golden: `bbctests/examples/prog2.bbc`.
- A behaviour spec: `behtests/prog8_while_lt.beh`.

## Deliberate deviations from the OCaml and compiler-course defaults

- No `.mli` files: every module's whole surface is visible to the others. Not decided; recorded so
  that adding them is seen as a structural change.
