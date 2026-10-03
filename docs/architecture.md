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
| `behtests/*.beh` | behaviour specs, run by `tools/behave.py` |
| `examples/*.src` | RED programs to compile by hand (`make compile src=...`) |
| `pmars/` | third-party pMARS: the Linux binary, its configs (`config/94b.opt`) and the source zip |
| `tools/` | the gate's own scripts: `audit.py`, `behave.py`, `pmars-host.sh` |
| `docs/` | the documents in the map of `AGENTS.md` |

## The pipeline

```
RED text ──CCSexp──▶ sexp ──Parse.parse_exp──▶ Ast.expr ──Ast.tag_expr──▶ tag eexpr
   ──Compile.compile_expr (Analyse, Lib, Util)──▶ Red.instruction list ──Red.pp_instrs──▶ redcode text
```

| Module | Role | Representation it produces |
|---|---|---|
| `src/parse.ml` | s-expression → AST; rejects unknown forms with `CTError` | `Ast.expr` |
| `src/ast.ml` | AST types; `tag_expr` numbers every node in pre-order from 1 | `tag eexpr` (tags feed label names) |
| `src/analyse.ml` | per `let`: finds the field (`PA`/`PB`) where `(store x)` sits | extends `penv` |
| `src/lib.ml` | environments `aenv` (name → initializer), `penv` (name → field), `lenv` (name → label); `jump_label` | `env` |
| `src/util.ml` | operand lowering through three small IRs: `darg` (number or label) → `carg` (constant, label, variable or pointer) → `Red.rarg`; and modifier choice `opmod` → `Red.rmod` | `Red.rarg`, `Red.rmod` |
| `src/compile.ml` | control flow and conditions to labels and jumps; `compile_prog` adds the header and the epilogue `DAT` | `Red.instruction list`, then text |
| `src/red.ml` | the Redcode target: opcodes, modes, modifiers, and the pretty-printer that fixes the column padding | text |

**Dependency direction:** `red` ← `ast` ← `lib` ← `util` ← `analyse` ← `compile`; `parse` depends only
on `ast`. dune rejects cycles, so the direction cannot invert silently; a new module states where it
sits in this chain.

## Generated labels

Every label the compiler invents is a prefix plus the tag of the node that produced it
(d-7d2612-123e41). The prefixes in use:

| Prefix | Produced by |
|---|---|
| `LET` | `let`: labels the cell that holds the variable (where `(store x)` sits) |
| `REP` | `repeat`: loop head |
| `IF` | `if` without else: end label |
| `IFM`, `IFF` | `if` with else: else-branch label, end label |
| `WHI`, `WHF` | `while`: loop head, end label |
| `DWH` | `do-while`: loop head |

User labels share this namespace today (i-7d2612-425c66). Changing the tag numbering or a prefix
changes every golden that contains one: that is a change to the output contract, not a refactor.

## Tests and where a new one goes

| You want to show | Add | Run |
|---|---|---|
| the exact redcode a program compiles to | a `.bbc` in `bbctests/examples/` (copy `bbctests/examples/prog2.bbc`) | `make tests F=compare` |
| what the compiled warrior does in the core | a `.beh` in `behtests/` pointing at a golden (copy `behtests/prog8_while_lt.beh`) | `python3 tools/behave.py` |
| a bug, before fixing it | a `.bbc` in `bbctests/known-bugs/` with the current output, a `.beh` marked `known-failing: <roadmap id>`, and the roadmap item | both of the above |
| a function's result | an alcotest case in `execs/run_test.ml` (`ocaml_tests`) | `make tests F=parse` |

The `.bbc` format (BBCStepTester): `NAME:`, `DESCRIPTION:`, optional `PARAMS:` and `STATUS:`
(`CT error`, `RT error`), `SRC:`, `EXPECTED:`, optional `END`. No trailing newline after the last
EXPECTED line. bbctester finds `*.bbc` recursively under `bbctests/`, so every golden runs in both
`compare` and `execute`.

## Exemplary files

- A pass: `src/compile.ml` — one function per construct, exhaustive `match`, errors via `CTError`.
- A golden: `bbctests/examples/prog2.bbc`.
- A behaviour spec: `behtests/prog8_while_lt.beh`.

## Deliberate deviations from the OCaml and compiler-course defaults

- Two parallel AST types (`expr` and `'a eexpr`) instead of one annotated `'a expr`: inherited; no
  source locations exist yet (i-7d2612-1703ff).
- No `.mli` files: every module's whole surface is visible to the others. Not decided; recorded so
  that adding them is seen as a structural change.
