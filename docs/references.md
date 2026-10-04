# External information worth knowing

Not a link list: every entry says what it contributes **to this project**, and when it contradicts
something already decided, it says so. An entry is here because it changed or confirmed a
decision. Claims marked *(measured)* were reproduced on 2026-10-02 with pMARS 0.9.4 built from
`pmars/pmars-0.9.4.zip`; claims marked *(read)* come from the source named; the rest say what they
rest on. URLs were checked on 2026-10-02.

## The target: ICWS'94 and pMARS

- **[ICWS'94 draft standard](https://corewar.co.uk/standards/icws94.txt)** — the only normative
  definition of Redcode semantics, with a reference simulator in C.

  **What it confirms:** pre-condition layouts (`JMN` for `JZ`, `DJN` for `DZ`, `SLT a2, a1` for
  `GT`); the default `ORG 0` start, so emitting no `ORG`/`END` is correct; core values are stored in
  `0..CORESIZE-1`, so comparisons are unsigned.

  **What we do differently, on purpose:** nothing yet. *Not on purpose:* the `.I` fallback modifier
  (the A.2.1.1 default table says `.B` for `SLT`/`JMZ`/`JMN`/`DJN`), and the post-condition `SLT`
  layout, which is not strict at equality *(measured)*.

  **Not applied yet:** `DIV`/`MOD` by zero kills the process; `SLT.I` behaves as `SLT.F`.

  Produced: d-7d2612-2a4435, i-7d2612-fffa6c, i-7d2612-3744e5, i-7d2612-96f7b1.

- **pMARS 0.9.4 documentation and source** (`pmars/pmars-0.9.4.zip`: `doc/pmars.txt`,
  `doc/redcode.ref`, `src/asm.c`, `src/pmars.c`) *(read, measured)*.

  **What it confirms:** labels are `[A-Za-z_][A-Za-z0-9_]*` and case-sensitive. Opcode and
  pseudo-opcode names are case-insensitive and cannot be labels (`END`, `ORG`, `EQU`, `FOR`, `ROF`,
  `PIN`). A single warrior loads at address 0. Exit codes: 0 even with warnings, 3 on an assembly
  error, 4 on cdb `quit`. `-A` assembles without running a battle.

  **What we do differently:** the `execute` suite uses `-A`, so it never runs a warrior
  (i-7d2612-05c64d).

  **Not applied yet:** a redefined label is only a warning, and the first definition wins
  (i-7d2612-425c66). Lines of 256 characters or more hang the assembler (i-7d2612-174acf). Every
  compiled warrior warns "Missing ';assert'" (i-7d2612-b682d5).

  Produced: d-7d2612-3d04ba, `tools/behave.py`.

- **[pMARS at koth.org](http://www.koth.org/pmars/)** — the maintained home. 0.9.5 (2026-01-03) adds
  overflow and bounds fixes and builds on macOS unpatched. The vendored zip's SHA-256 matches
  koth.org's 0.9.4. Debian/Ubuntu 24.04 package 0.9.4; Ubuntu 20.04/22.04 package 0.9.2, which has no
  `-A`. Produced: i-7d2612-494e75, i-7d2612-202da9.

- **pMARS cdb debugger** (`doc/primer.cdb`, `doc/pmars.txt`) *(measured)*. Commands piped to
  `pmars -e -b` give a deterministic trace: `skip K` executes K+1 instructions, `calc CYCLE` prints
  the remaining cycles, `list A` disassembles a cell, `pqueue` lists processes. `list` paginates
  after about 40 lines and swallows the commands after it. **What we do with it:** the behaviour
  specs (d-7d2612-b92028); the `run-warrior` skill.

## Hills and community

- **[koth.org](http://www.koth.org/koth.html)** (http only) — the 94nop hill is the most active
  (battles on 2026-10-02); hills 88, 94, 94nop, 94x, icws, 94m, 94xm; submission by mail with a
  `;redcode-94nop` header; IRC replaced by a Slack. **Confirms** 94nop as the natural second target;
  **not applied:** multi-target output (i-7d2612-217183).
- **[SAL hills](https://sal.discontinuity.info)** — 94b (beginners: core 8000, length 100,
  p-space 500), tiny (800/20), nano (80/5), lp, mp. The `;redcode-<key>` header selects the hill.
  **Confirms** d-7d2612-6d88cd; small-core hills are the hardest test of compiled-code size.
- **[corewar.co.uk](https://corewar.co.uk)** — news, guides, the evolvers list, tournament results
  (bot-gated; Wayback snapshots used). Chat: Discord, Libera `#corewars`.
- **[Koenigstuhl](https://asdflkj.net/COREWAR/koenigstuhl.html)** — archive hill of published
  warriors with source (updated 2026-07). A corpus, not fixtures: authors keep their rights.
- **Digital Red Queen** ([arXiv 2601.03335](https://arxiv.org/abs/2601.03335),
  [SakanaAI/drq](https://github.com/SakanaAI/drq)) — LLM-evolved Redcode; ships a Python 3 MARS with
  a step API. **Not applied:** a second simulator for differential tests (i-7d2612-56302d).
- **[rmars](https://github.com/clrsrc/rmars)** — Rust reimplementation of pMARS 0.9.6-dev claiming
  identical results, with Python and wasm bindings. A candidate second oracle.
- **Avoid:** corewar.io (the domain redirects elsewhere; docs live at corewar-docs.readthedocs.io),
  vyznev.net (redirect loop; the beginners' guide is mirrored at corewar.co.uk/karonen/guide.htm),
  halite.io (gone).

## Corpora and observation

- **[Wilkies benchmark](http://www.koth.org/wilkies/wilkies.html)** — 12 classic warriors (paper,
  stone, scissors) *(measured: 12×200 rounds in about 1 s; `-F N` is deterministic, `-f` reseeds
  when the code changes)*. No licence statement: fetch, never vendor. Produced: i-7d2612-f27a91.
- **[n1LS/redcode-warriors](https://github.com/n1LS/redcode-warriors)** — 567 warriors, 565 assemble
  under 94b *(measured)*; no licence. A corpus for parser and assembler sweeps only.
- **pMARS's own `warriors/`** (in the zip; GPL-2+) — `validate.red` is an ICWS compliance test and
  may be used as a fixture.
- **A lone warrior always "survives" in battle mode** *(measured)*: pMARS's score cannot tell a dead
  warrior from a live one with one warrior loaded. Battle against a `JMP 0` "duck", or use cdb.
- **Prior art:** no maintained compiler from a higher-level language to Redcode was found besides
  this one. samurai/redcomp (Python 2, 2012) is a toy; evolvers (CCAI, µGP, YabEvolver, DRQ) generate
  Redcode text; optiMAX and mopt tune constants (i-7d2612-a3f2b6). 42-school "corewar" is a different
  VM and bytecode.

## Compiler construction

- **[CC5116 notes, conditionals and binary operators](https://users.dcc.uchile.cl/~etanter/CC5116/lec_cond-binops_notes.html)**
  — the course this repository started from. **Confirms** labels from AST tags, "completely
  determined by its input… easier to work with in the context of testing" (d-7d2612-123e41).
  **Differs, not on purpose:** the course uses one annotated `'a expr` that also carries source
  locations; this repository keeps two AST types and no locations (i-7d2612-1703ff).
- **[Siek, *Essentials of Compilation*](https://jeapostrophe.github.io/courses/2021/spring/406/notes/book.pdf)**
  — test each pass's output on an interpreter for its language; a *uniquify* pass removes shadowing
  before analysis. **Applied:** uniquify, as `src/rename.ml` (i-7d2612-ce4c3b). **Not applied:** the
  interpreters (i-7d2612-56302d).
- **[Keep & Dybvig, nanopass](https://andykeep.com/pubs/np-preprint.pdf)** — many single-task passes
  over defined intermediate languages, each output checkable for well-formedness. **Confirms** the
  pass structure; **not applied:** a validator over `Red.instruction` (labels unique and defined).
- **[Leroy, CompCert (CACM)](https://xavierleroy.org/publi/compcert-CACM.pdf)** — semantic
  preservation by simulation; *translation validation* as the practical alternative to proof.
  **Applied as:** behaviour specs are a weak translation validation; the correctness statement in
  `docs/semantics.md` follows its shape.
- **[Real World OCaml: compiler frontend](https://dev.realworldocaml.org/compiler-frontend.html),
  [error handling](https://dev.realworldocaml.org/error-handling.html)** and the OCaml compiler's own
  `Location` / `Misc.fatal_error` — locations on the AST, one user-error type, internal errors kept
  apart, an `.mli` per module. **Not applied:** all four (i-7d2612-888db5, i-7d2612-1703ff).
- **[Csmith](https://users.cs.utah.edu/~regehr/papers/pldi11-preprint.pdf), [QCheck](https://github.com/c-cube/qcheck)**
  — random programs plus differential comparison and shrinking. Later, after a reference
  interpreter exists.
- **CS3110's interpreter chapters use a global `gensym`** — fine for an interpreter, **contradicts**
  d-7d2612-123e41 for a compiler with goldens. The dev branch follows CS3110 here.

## OCaml and dune

- **dune: [profiles](https://dune.readthedocs.io/en/latest/reference/dune-workspace/profile.html)
  and its `ocaml_flags.ml`** — the default profile is `dev`; an `(env (<name> ...))` stanza applies
  only under that profile name. For `lang dune 3.10` the dev flags make unused opens (33), unused
  values (32) and non-exhaustive matches (8) errors. **Contradicts** the `cored` env stanza, which
  never applies (i-7d2612-ceee87).
- **dune: [`@check`](https://dune.readthedocs.io/en/stable/reference/aliases/check.html),
  [tests and cram](https://dune.readthedocs.io/en/stable/tests.html),
  [formatting](https://dune.readthedocs.io/en/stable/howto/formatting.html)** — `@check` (`make init`)
  type-checks without linking executables, so the gate uses `dune build`. Cram and expect tests
  promote output on request; `dune fmt` rewrites files (never on a dirty tree), `dune build @fmt`
  only prints a diff. **Not applied:** a `(test)` stanza (i-7d2612-47d3ea), ocamlformat
  (i-7d2612-2a14f2).
- **[BBCStepTester](https://github.com/fabaindaiz/BBCStepTester)** (commit 2cb3669) — the
  `bbctester` library the tests link. No `.opam` file; install by clone and `dune install`.
  `compare_status` checks only the exit status; `tests_from_dir` finds `*.bbc` recursively;
  `unix_command` runs `/bin/sh -c` with `%s` replaced by the emitted file. **Contradicts** the README,
  which credits pleiad/BBCTester, whose API differs (i-7d2612-8f3f22).
- **[opam manual](https://opam.ocaml.org/doc/Manual.html)** — `pin-depends` needs the pinned package
  in `depends` and an opam file in the pinned repository; `opam lock` freezes versions.
- **[setup-ocaml](https://github.com/ocaml/setup-ocaml)** v3 — the CI action; caches the opam switch.

## Programming-language theory

The definitions and the reading list for contributors are in `docs/semantics.md`. The sources that
shaped it:

- **Nielson & Nielson, *Semantics with Applications*** ([PDF](http://www.cs.ru.nl/~herman/onderwijs/semantics2019/wiley.pdf))
  — small-step and big-step semantics of While, environments versus stores, and a provably correct
  compilation of While to an abstract machine with labels and jumps: the closest model of what RED
  does. **Applied:** the structure of `docs/semantics.md`.
- **[Software Foundations vol. 2, *Programming Language Foundations*](https://softwarefoundations.cis.upenn.edu/plf-current/toc.html)**
  — small-step semantics, program equivalence, type soundness as progress plus preservation.
- **Barendregt, the lambda cube** ([overview](https://en.wikipedia.org/wiki/Lambda_cube); *Lambda
  calculi with types*, Handbook of Logic in Computer Science vol. 2) — eight typed lambda calculi on
  three axes. **Applied:** placing RED and the dev branch's surface language on it.
- **Morrisett et al., [From System F to Typed Assembly Language](https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf)**
  — a type system for assembly separating integers from code labels. **Not applied:** the kind check
  (i-7d2612-f2f7c5); it can be a lint, not a soundness guarantee, because Redcode mixes code and data.
- **Harper, [*Practical Foundations for Programming Languages*](https://www.cs.cmu.edu/~rwh/pfpl/)**
  (statics versus dynamics), **Pierce, [*TAPL*](https://www.cis.upenn.edu/~bcpierce/tapl/main.html)**
  (lambda calculus, simple types), **[PLAI](https://plai.org/)** (interpreters and desugaring),
  **[PLT Redex](https://redex.racket-lang.org/)** (executable reduction semantics).
- **No formal semantics of Core War or Redcode was found** beyond ICWS'94's reference simulator.

## What to read first

| If you are about to touch… | Read | And watch out for |
|---|---|---|
| `compile_cond`, `compile_cond2`, any loop layout | ICWS'94 `SLT`/`JMZ`/`DJN`; `docs/semantics.md` *Dynamics* | strict `<`, unsigned values; i-7d2612-fffa6c |
| `opmod_to_rmod`, modifiers | ICWS'94 A.2.1.1 | the `.I` fallback (i-7d2612-96f7b1); `.I` changes `SLT` to "both fields" |
| label generation, `tag_expr` | CC5116 notes; `docs/architecture.md` *Generated labels* | every golden contains the numbering; user-label collisions |
| `analyse.ml`, `lib.ml`, `rename.ml` environments | Siek (uniquify); `docs/semantics.md` *Statics* | names are unique after `Rename.uniquify`; analysis may rely on it |
| errors | RWO error handling | four `CTError`s (i-7d2612-888db5) |
| tests or goldens | BBCStepTester README; `docs/architecture.md` | `execute` proves assembly only; d-7d2612-6a1527 |
| pMARS, its flags, its binary | `doc/pmars.txt` in the zip | the vendored ELF is Linux-only; `-A` runs nothing |
| hill targets | koth.org, SAL | p-space absent on 94nop; header selects the hill |
