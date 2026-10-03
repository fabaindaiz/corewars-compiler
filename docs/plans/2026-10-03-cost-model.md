# Cost model, ordered IR and expectations — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task by task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** give the compiler a positioned view of the redcode it emits, static metrics and
predictions over it, a configurable policy, and checkable expectations — without changing a byte
of emitted code.

**Architecture:** `compile_expr` returns annotated instructions (origin tag, generating construct,
stored variables); `Layout` resolves them into a cell array with successors and loops; `Metrics`
measures it and holds the policy; `Expect` checks programmer expectations against the metrics or
exports them as behaviour probes. `compile_prog` keeps printing the same text.

**Tech stack:** OCaml 5 / dune 3.10, alcotest, bbctester (BBCStepTester fork), Python 3.11+ for
`tools/behave.py`.

**Spec:** `docs/specs/2026-10-03-cost-model-design.md` — read it first; this plan argues from it.

## Global constraints

- Emitted redcode is unchanged: `make tests F=compare` stays green with no golden edited.
- Dependency chain: `red` ← `ast` ← `lib` ← `util` ← `analyse` ← `compile` ← `layout` ← `metrics` ←
  `expect`; `parse` depends only on `ast`. Add each new module to `(modules ...)` in `src/dune`.
- `CORESIZE` defaults to 8000 (94b); every function that needs it takes `?coresize:int`.
- Offsets and field values are normalised to `0..coresize-1`.
- dune `dev` profile: no unused opens or values, no non-exhaustive match, **no new `| _ ->` over
  `Red.opcode`, `Ast.expr` or `Ast.eexpr`** (AGENTS.md, *Engineering standards*).
- Errors stay `CTError` raised from the module that detects them (unifying them is
  i-7d2612-888db5, not this plan). `run_compile` catches what it needs to exit cleanly.
- Every parser keyword added must be documented in `LANGUAGE.md` in the same commit:
  `tools/audit.py` (`language-documents-parser`) fails otherwise.
- Work on branch `feat/cost-model`. Commit only as `make check && git commit ...` (or
  `make check-tools && ...` where no opam switch exists, saying so). Commit messages carry **no
  `Co-Authored-By` or any assistant attribution**; before any push,
  `git log --format=%B main..HEAD | grep -ci co-authored` prints `0`. No merge to `main` or push
  unless the user asks.
- Each task adds or extends its alcotest group in `execs/run_test.ml` and appends it to the list
  passed to `run`.

## Review focus

1. **A program with no loops** (prog0, the imp `MOV 0, 1`): `loops = []`, `boot = None`, no
   predictions, the report prints `loops: none`. The static view cannot see that the imp copies
   itself forward; it must not invent a loop. → Task 3.
2. **A `DJN` counter that starts at 0** runs `coresize` iterations (0 − 1 wraps to 7999), not 0.
   → Task 4.
3. **Jumps through indirect operands** (prog4 has four: `JMP @LET4`, `JMP *LET1`, `JMP @LET2`,
   `JMP @LET3`): `Dynamic` successors, counted in the report (`dynamic jumps: 4`); reachability and
   loops ignore them; nothing crashes. → Tasks 2, 3.
4. **A label no instruction defines** (`(JMP nowhere)`, which pMARS rejects): `Undefined_label`
   diagnostic, the operand keeps `label = Some l` with `value = 0`, a jump to it is `Dynamic`. (A
   `let` with no `(store x)` never reaches the IR today: `compile` raises `CTError` from `penv`,
   i-7d2612-425c66.) → Task 2.
5. **A loop-scoped expectation with no enclosing loop**, or a global `cycles` expectation in a
   program with no loops: fails with `no loop encloses this expectation` /
   `the program has no loop`, never passes vacuously. → Task 6.

---

### Task 1: Annotated emission in `compile_expr`

**Files:**
- Modify: `src/compile.ml` (`compile_expr`, `compile_cond`, `compile_label`, `compile_prog`)
- Test: `execs/run_test.ml` (new group `emit`)

**Interfaces:**
- Produces, in `src/compile.ml`:
  ```ocaml
  type emitted = {
    instr     : Red.instruction;
    origin    : Ast.tag option;                (* None for comments and the epilogue *)
    construct : string option;                 (* "repeat" | "if" | "if-else" | "while" | "do-while"
                                                  for instructions a control construct generates *)
    stores    : (string * Ast.place) list;     (* (store x) operands of this instruction *)
  }
  val compile_expr : tag eexpr -> env -> emitted list
  val compile_body : Ast.expr -> emitted list     (* tag_expr + compile_expr empty_env; no prelude, no epilogue *)
  val epilogue : Red.instruction list            (* unchanged *)
  val compile_prog : Ast.expr -> string          (* unchanged output *)
  ```
  `ILAB`s emitted by a construct carry its tag and `construct`; `ILAB`s from `compile_label` carry
  the `EPrim2`'s tag and `construct = None`; condition instructions carry the construct's tag.

- [ ] **Step 1: Write the failing tests** in group `emit`, using the SRC of
  `bbctests/examples/prog8.bbc` parsed with `Parse.sexp_from_string`:
  - `test_emit_prog8_back_jump_is_while`: the `INSTR (IJMP, _, RLab (_, "WHI…"), _)` element has
    `construct = Some "while"` and the tag of the `while` node.
  - `test_emit_prog8_mov_is_user`: the `IMOV` element has `construct = None`.
  - `test_emit_prog1_stores`: in prog1, the `IMOV` element has `stores = [("x", PB)]`.
  - `test_emit_text_unchanged`: for every `bbctests/**/*.bbc`, `pp_instrs (List.map (fun e -> e.instr) (compile_body src))`
    equals what `compile_prog` printed before (compare with the golden's EXPECTED minus the
    `;redcode-94b` header and epilogue line).
- [ ] **Step 2: Run** `make tests F=emit` — expect a build error (`compile_body` unbound).
- [ ] **Step 3: Implement** the record and thread it through `compile_expr`; `compile_prog` maps
  `fun e -> e.instr` before `pp_instrs`. `stores` is filled where an `EPrim2` or a condition has
  `AStore` operands (`PA` for the first operand, `PB` for the second).
- [ ] **Step 4: Run** `make tests` — `emit` passes and `compare` is green with no golden changed.
- [ ] **Step 5: Commit** `feat: annotate emitted instructions with origin, construct and stores`.

### Task 2: The positioned IR (`src/layout.ml`)

**Files:**
- Create: `src/layout.ml`; modify `src/dune` (add `layout`)
- Test: `execs/run_test.ml` (group `layout`)

**Interfaces:**
- Consumes: `Compile.emitted`, `Compile.compile_body`, `Compile.epilogue`.
- Produces (types exactly as in spec §1, plus):
  ```ocaml
  type diagnostic = Undefined_label of string | Duplicate_label of string * int * int
                  | Long_line of int                          (* cell whose printed line is >= 256 chars *)
  type edge = Next of int | Jump of int | Skip of int | Dynamic
  type loop = { header : int; body : int list; back_edges : (int * int) list }   (* body sorted *)
  type program = { cells : cell array; succ : edge list array; loops : loop list;
                   diagnostics : diagnostic list; coresize : int }
  val build : ?coresize:int -> Compile.emitted list -> program   (* appends Compile.epilogue as role Epilogue *)
  val reachable : program -> bool array
  val of_expr : ?coresize:int -> Ast.expr -> program            (* build (compile_body e) *)
  ```
  A cell's `construct` is copied from its instruction (add `construct : string option` to `cell`).
  `vars`: the `stores` of the instruction, mapped `PA -> FA`, `PB -> FB`. A label that resolves
  twice keeps the first definition (as pMARS) and records `Duplicate_label (l, first, second)`.

- [ ] **Step 1: Write the failing tests** (`layout`), each on a golden's SRC:
  - `test_layout_prog8_cells`: 7 cells; `cells.(1).labels = ["LET1"; "LET2"]`;
    `cells.(1).vars = [("x", FA); ("y", FB)]`; `cells.(6).role = Epilogue`.
  - `test_layout_prog8_succ`: `succ.(2) = [Next 3; Skip 4]`, `succ.(3) = [Jump 6]`,
    `succ.(5) = [Jump 2]`, `succ.(6) = []`.
  - `test_layout_prog8_loop`: `loops = [{ header = 2; body = [2; 4; 5]; back_edges = [(5, 2)] }]`.
  - `test_layout_offsets_normalised`: prog1 cell 2 (`JMP $LET1`) has `a.value = 7998` and
    `a.label = Some "LET1"`.
  - `test_layout_prog4_dynamic`: each of prog4's four `JMP` cells has successors `[Dynamic]`.
  - `test_layout_undefined_label`: SRC `(JMP nowhere)` gives `Undefined_label "nowhere"` in
    `diagnostics`, and the jump's successors are `[Dynamic]`.
  - `test_layout_long_line`: SRC `(seq (label <a label of 250 letters>) (JMP <same label>))` gives
    `Long_line 0` (the `JMP` line printed by `Red.pp_instruction` is 256+ characters).
  - `test_layout_duplicate_label`: `bbctests/known-bugs/label_collision.bbc` SRC gives
    `Duplicate_label ("LET1", 1, 2)`.
- [ ] **Step 2: Run** `make tests F=layout` — fails to build (`Layout` unbound).
- [ ] **Step 3: Implement** `build`: one pass assigns positions (ILAB attaches to the next INSTR or
  to the epilogue; ICOM skipped), a second resolves operands, a third computes successors per the
  spec's table (any jump-class opcode whose target operand mode is indirect → `Dynamic`). Loops:
  iterative DFS from 0 over non-`Dynamic` edges; an edge to a node on the stack is a back edge; the
  natural loop body is the header plus every node that reaches the back edge's source without
  passing the header. Loops sharing a header merge.
- [ ] **Step 4: Run** `make tests` — all green.
- [ ] **Step 5: Commit** `feat: positioned IR with successors, loops and label diagnostics`.

### Task 3: Global and per-loop metrics (`src/metrics.ml`)

**Files:**
- Create: `src/metrics.ml`; modify `src/dune`
- Test: `execs/run_test.ml` (group `metrics`)

**Interfaces:**
- Consumes: `Layout.program`, `Layout.reachable`, `Layout.of_expr`.
- Produces:
  ```ocaml
  type range = { min : int; max : int }
  type loop_metrics = { loop : Layout.loop; node : Ast.tag option; construct : string option;
                        cycles : range; overhead : range; exit : int option }
  type t = { length : int; code : int; data : int; epilogue : int; unreachable : int;
             nonzero : int; nonblank : int; boot : range option; spl_sites : int;
             dynamic_jumps : int; div_by_zero : int list;   (* cells: DIV/MOD by a constant 0 *)
             loops : loop_metrics list; predictions : prediction list }
  val measure : Layout.program -> t        (* predictions = [] until Task 4 *)
  val to_text : maxlength:int -> t -> string
  val to_json : t -> string
  ```
  `node` / `construct`: those of the back-edge source cell. `cycles`: over the simple paths from
  the header back to it inside the body (each cell at most once; an inner loop counts one pass).
  `overhead`: cells on such a path with `construct <> None` and a jump or skip opcode (`JMP JMZ JMN
  DJN SEQ SNE SLT CMP`). `exit`: shortest path from the header to a successor outside the body;
  `None` when there is none. `boot`: over simple paths from 0 to the first header met; `None` with
  no loops. `nonblank`: a cell is blank only if `DAT` with modifier `RN` or `RF` and both operands
  `$0`. `to_text` prints the spec's report layout; with no loops the loop lines are
  `loops: none`.

- [ ] **Step 1: Write the failing tests** with the spec's reference values:
  - `test_metrics_prog1`: `length 4, code 3, epilogue 1, data 0, nonzero 3, nonblank 4`,
    `boot = Some {min=0;max=0}`, one loop with `cycles {3;3}`, `overhead {0;0}`, `exit = None`.
  - `test_metrics_prog7`: `boot {1;1}`; loop cells `[2;3]`, `cycles {2;2}`, `overhead {1;1}`,
    `construct = Some "do-while"`.
  - `test_metrics_prog8`: `boot {1;1}`; loop `cycles {3;3}`, `overhead {2;2}`, `exit = Some 2`.
  - `test_metrics_prog0_no_loops`: `loops = []`, `boot = None`; `to_text` contains `loops: none`.
  - `test_metrics_prog4_dynamic`: `dynamic_jumps = 4`; `measure` returns without exception.
  - `test_metrics_div_by_zero`: SRC `(seq (DIV 0 (Dir 1)) (DAT 1 1))` gives `div_by_zero = [0]` and
    `to_text` contains `kills: cell 0 divides by 0`.
  - `test_metrics_json_keys`: `to_json` of prog1 contains `"length":4` and `"cycles":{"min":3,"max":3}`.
- [ ] **Step 2: Run** `make tests F=metrics` — fails to build.
- [ ] **Step 3: Implement** `measure`, `to_text`, `to_json` (hand-written JSON, no new dependency).
- [ ] **Step 4: Run** `make tests` — all green.
- [ ] **Step 5: Commit** `feat: static metrics over the positioned IR`.

### Task 4: Predictions

**Files:**
- Modify: `src/metrics.ml`
- Test: `execs/run_test.ml` (group `metrics`)

**Interfaces:**
- Produces, in `Metrics`:
  ```ocaml
  type prediction =
    | Step of { loop : int; cell : int; field : Layout.field; k : int;
                period : int; cover_cycles : int; full : bool }
    | Counter of { loop : int; n : int; loop_cycles : int; dies_after : int option }
  ```
  `loop` is the header. **Step**: inside a loop, either an `ADD`/`SUB` with an immediate A operand
  `k` whose modifier writes one field (`.A`/`.BA` → A, `.B`/`.AB` → B; other modifiers: no
  prediction) of cell `T`, executed on every path of the loop, where `T`'s field is the destination
  of a write in the loop (directly: `T` itself writes through its B operand and the field is B; or
  indirectly: a write's B operand is `@`/`*` through that field of `T`); or a write whose B operand
  uses `>`/`}` (k = 1) or `<`/`{` (k = −1) through a field of `T`. `k` normalised mod coresize;
  `period = coresize / gcd(k, coresize)`; `cover_cycles = period × cycles.max`; `full = (gcd = 1)`.
  **Counter**: the back-edge source is `DJN` targeting the header; `n` is the initial value of the
  decremented field (immediate B → its own B number; direct B → that cell's field selected by the
  modifier); `n = 0` means `coresize`; `loop_cycles = n × cycles.max`; `dies_after = Some (boot.max
  + loop_cycles + 1)` when the cell after the `DJN` is a `DAT`, else `None`.

- [ ] **Step 1: Write the failing tests:**
  - `test_predict_prog1_step`: `Step { k = 4; period = 2000; cover_cycles = 6000; full = true }`.
  - `test_predict_prog7_counter`: `Counter { n = 100; loop_cycles = 200; dies_after = Some 202 }`.
  - `test_predict_prog7_step`: `Step { k = 1; period = 8000; cover_cycles = 16000; full = true }`
    (the `>LET1` destination).
  - `test_predict_counter_zero`: SRC `(do-while (DN 0) (NOP))` gives `n = 8000`.
  - `test_predict_matches_behaviour`: read `behtests/prog7_dowhile_dn.beh`, take its `dead N`; it
    equals prog7's `dies_after`.
  - `test_predict_partial_cover`: SRC
    `(let (p 0) (seq (JMP (Dir 2)) (DAT (store p) 0) (repeat (seq (ADD 2 p) (MOV 0 (Ind p))))))`
    (`ADD.A #2` on the A-field of `p`'s cell, `MOV #0, *LET1` through it) gives `k = 2`,
    `period = 4000` and `full = false`.
- [ ] **Step 2: Run** `make tests F=metrics` — the new tests fail (`predictions = []`).
- [ ] **Step 3: Implement** the two recognisers; `to_text` prints `predicted: ...` lines as in the
  spec, with `does not visit every cell` when `full = false`.
- [ ] **Step 4: Run** `make tests` — all green; mutate `period` (e.g. drop the gcd) and watch
  `test_predict_partial_cover` fail, then restore.
- [ ] **Step 5: Commit** `feat: fixed-step and counter predictions`.

### Task 5: Policy, program header and CLI

**Files:**
- Modify: `src/ast.ml`, `src/parse.ml`, `src/metrics.ml`, `execs/run_compile.ml`, `LANGUAGE.md`
- Test: `execs/run_test.ml` (group `policy`)

**Interfaces:**
- Produces:
  ```ocaml
  (* ast.ml *)
  type cmp = Eq | Le
  type expectation = XLength of cmp * int | XCycles of cmp * int | XOverhead of cmp * int
                   | XBoot of cmp * int | XStep of int | XCoversCore
                   | XAlive of int | XDead of int | XCell of int * string * int  (* addr, text, after N *)
  type source = { optimize : string list option; expects : expectation list; body : expr }
  (* parse.ml *)
  val parse_source : CCSexp.sexp -> Ast.source      (* a plain expression → {None; []; body} *)
  val parse_expectation : CCSexp.sexp -> Ast.expectation
  (* metrics.ml *)
  type objective = Speed | Size | Stealth | Boot
  type policy = objective list
  val default_policy : policy                       (* [Speed; Size] *)
  val objective_of_string : string -> objective option   (* speed size stealth boot *)
  val compare : policy -> t -> t -> int
  ```
  Header form: `(program (optimize o ...) (expect e) ... body)`, options in any order before the
  single body. All four measured forms accept `(m N)` and `(m <= N)`. `compare` per objective:
  `Speed` — worst `cycles.max`, then the sum of `cycles.max`; `Size` — `length`; `Stealth` —
  `nonblank`; `Boot` — `boot.max` (no loops sorts last). `run_compile`: `--optimize o1,o2`
  (overrides the header), `--report` (text to stderr) and `--report=json` (JSON to stdout instead of
  redcode), using `Stdlib.Arg`; an unknown objective exits 1 with
  `unknown objective `fast`: one of speed, size, stealth, boot`.

- [ ] **Step 1: Write the failing tests:** `test_policy_default` (`[Speed; Size]`),
  `test_policy_compare_speed_first` (a 3-cycle 8-cell program beats a 4-cycle 4-cell one under the
  default; the reverse under `[Size; Speed]`), `test_parse_source_plain` (prog1 SRC →
  `optimize = None`, `expects = []`), `test_parse_source_header` (`(program (optimize size) (expect (length <= 8)) (MOV 0 1))`
  → `Some ["size"]`, `[XLength (Le, 8)]`), `test_objective_unknown` (`objective_of_string "fast" = None`).
- [ ] **Step 2: Run** `make tests F=policy` — fails to build.
- [ ] **Step 3: Implement**; add `EExpect`/`Expect` constructors to `Ast.expr`/`Ast.eexpr`
  (`tag_expr` gives them a tag; `compile_expr` emits `[]`; `parse_exp` accepts `(expect e)` as a
  statement), and document `program`, `optimize`, `expect` and every new keyword in
  `LANGUAGE.md`.
- [ ] **Step 4: Run** `make check` — all green, including the audit; then by hand:
  `dune exec execs/run_compile.exe -- --report examples/prog1.src` prints the prog1 report on
  stderr and the unchanged redcode on stdout.
- [ ] **Step 5: Commit** `feat: optimization policy, program header and --report/--optimize`.

### Task 6: Expectations (`src/expect.ml`) and probe export

**Files:**
- Create: `src/expect.ml`; modify `src/dune`, `execs/run_compile.ml`, `tools/behave.py`,
  `docs/architecture.md` (spec format line)
- Test: `execs/run_test.ml` (group `expect`); a new `examples/prog7_expect.src`

**Interfaces:**
- Consumes: `Metrics.measure`, `Layout.of_expr`, `Ast.source`.
- Produces:
  ```ocaml
  val collect : Ast.tag Ast.eexpr -> (Ast.expectation * Ast.tag option) list
     (* the tag of the innermost enclosing repeat / while / do-while; None outside any *)
  type outcome = Pass | Fail of string
  val check : Metrics.t -> Ast.expectation * Ast.tag option -> outcome option   (* None: execution kind *)
  val to_beh : redcode:string -> Ast.expectation list -> string
  ```
  Messages, exactly (`%s` is the construct, `%d` values; `<= ` is omitted for `Eq`):
  - `expect length <= N: the warrior is L cells`
  - `expect cycles <= N: the C of node T executes M per iteration` (global: the loop with the
    largest `cycles.max`, phrased the same)
  - `expect overhead <= N: the C of node T spends M per iteration on control`
  - `expect boot <= N: the first loop starts after M cycles`
  - `expect step K: no fixed-step pointer found` / `expect step K: the pointer steps by M`
  - `expect covers-core: the pointer steps by K and visits P of CORESIZE cells` (the number)
  - `... : no loop encloses this expectation` / `... : the program has no loop`
  `run_compile`: a `Fail` prints the message to stderr and exits 1; `--expect=warn` prints
  `warning: ` + message and exits 0. `--emit-beh FILE` writes `FILE` and the redcode beside it as
  `<stem>.red`; the spec maps `(alive N)`→`alive N`, `(dead N)`→`dead N`,
  `(cell A "T" N)`→`cell N A T`, with `redcode: <stem>.red`. `tools/behave.py` accepts
  `redcode: PATH` (relative to the spec's directory) as an alternative to `golden:`; exactly one of
  the two is required.

- [ ] **Step 1: Write the failing tests:** `test_expect_length_pass`, `test_expect_length_fail`
  (exact message), `test_expect_cycles_in_loop_fail` (prog8 body with `(expect (cycles <= 2))`
  inside the `while` → `expect cycles <= 2: the while of node T executes 3 per iteration`, with
  prog8's while tag), `test_expect_no_enclosing_loop`, `test_expect_global_no_loop` (prog0 with
  `(expect (cycles <= 3))`), `test_expect_step_prog1`, `test_expect_to_beh` (exact text for
  `[XAlive 201; XDead 202]`).
- [ ] **Step 2: Run** `make tests F=expect` — fails to build.
- [ ] **Step 3: Implement** `expect.ml`, the CLI flags and `redcode:` in `behave.py`. Write
  `examples/prog7_expect.src`: prog7's program inside `(program (expect (length <= 5)) (expect (alive 201)) (expect (dead 202)) ...)`.
- [ ] **Step 4: Run** `make check`; then
  `dune exec execs/run_compile.exe -- --emit-beh _build/prog7.beh examples/prog7_expect.src && python3 tools/behave.py _build/prog7.beh`
  → `ok  prog7  (2 probes)`; and with `(expect (length <= 4))` the compile exits 1 with
  `expect length <= 4: the warrior is 5 cells`.
- [ ] **Step 5: Commit** `feat: expectations checked at compile time or exported as behaviour probes`.

### Task 7: Documents put back to true

**Files:**
- Modify: `docs/semantics.md` (a §6a *Metrics* with the spec's definitions, pointing at the spec),
  `docs/architecture.md` (pipeline, module table, dependency chain, `layout`/`metrics`/`expect`),
  `docs/decisions.md`, `docs/roadmap.md`, `AGENTS.md` (commands: the new flags), `REFERENCE.md`,
  `.claude/skills/run-warrior/SKILL.md` (`--emit-beh`), `.claude/logs/agent-changelog.md`

- [ ] **Step 1:** Mint ids (`python3 .agents/tools/bundle.py id d|i|s "<text>"`) and add:
  decisions — *the cost model changes no emitted code*; *the default policy is speed then size*
  (with the measurement table's numbers); *a failed static expectation is a compile error unless
  `--expect=warn`*. Roadmap — subproject A as **Done** (what it became, what is missing: weighted
  policies, benchmark validation, SPL process counts), and new **Planned** items for B (performance
  warnings), C (operators, ICWS'94 defaults, reserved label prefix, the `do-while` layout) and
  snippets, each with its collisions and its recorded decision.
- [ ] **Step 2:** Changelog entry for the work, including what went wrong on the way.
- [ ] **Step 3: Run** `make check` — the audit checks the new paths and ids.
- [ ] **Step 4: Commit** `docs: record the cost model in the semantics, architecture, decisions and roadmap`.
