# Semantics of RED

What a RED program means, what the compiler must preserve, and the vocabulary to talk about both.
This document is the specification: when the compiler, a golden and a comment disagree, the rule
here (or ICWS'94, for the target) decides. `LANGUAGE.md` is the user's syntax reference;
`docs/architecture.md` says where each rule is implemented.

The structure follows the standard presentation of a small imperative language — syntax, statics,
dynamics, then a correctness statement for its compiler (Nielson & Nielson, *Semantics with
Applications*; *Programming Language Foundations*). Sources are in `docs/references.md`.

## 1. The target machine: ICWS'94

RED compiles to Redcode, the assembly of a MARS (Memory Array Redcode Simulator). What the
compiler relies on:

- **The core** is a circular array of `M` cells (`CORESIZE`, 8000 on 94b). Each cell holds a whole
  instruction: opcode, modifier, and two operands (A and B), each a mode and a number. **There is no
  separation between code and data**: a "variable" is a field of an instruction, and executing it
  or overwriting it is always possible.
- **Numbers** are stored in `0..M-1`; arithmetic is modulo `M`; `-1` is `M-1`. So every comparison
  is **unsigned** (d-7d2612-2a4435).
- **Processes**: each warrior has a FIFO queue of program counters. Each cycle executes one
  instruction of the process at the head; `DAT` (and a division by zero) removes the process; `SPL`
  queues `PC+1`, then its target. A warrior with an empty queue is dead.
- **Addresses are relative** to the executing cell. Modes: `#` immediate, `$` direct, `@`/`*`
  B/A-indirect, `<`/`{` predecrement, `>`/`}` postincrement.
- **Modifiers** (`.A .B .AB .BA .F .X .I`) select which fields an instruction reads and writes.
  ICWS'94 A.2.1.1 gives the default when none is written.
- **Loading**: execution starts at the first instruction (`ORG 0` by default).

## 2. Syntax

Abstract syntax, as `src/ast.ml` represents it (`LANGUAGE.md` has the concrete forms):

```
e ::= (label l) | (com …) | (OP [mod] a a)                          primitives
    | (repeat e) | (if c e) | (if c e e) | (while c e) | (do-while c e)   control flow
    | (let (x a) e) | (seq e …)                                       binding, sequence
c ::= (JZ a) | (JN a) | (DZ a) | (DN a)                               unary conditions
    | (EQ a a) | (NE a a) | (GT a a) | (LT a a)                       binary conditions
a ::= n | id | (mode n) | (mode id) | (store x) | none                operands
```

`Ast.tag_expr` numbers every node in pre-order; the number `n` of a node names the labels it
generates (`_LETn`, `_REPn`, `_IFn`, …), which makes the output a function of the program alone
(d-7d2612-123e41).

## 3. Statics: what a well-formed program is

- **Scope.** `(let (x a) e)` binds `x` in `e`. An inner `let` of the same name shadows the outer one.
  An identifier that is not a bound variable is a label (resolved by pMARS).
- **Places.** A variable does not denote a value but a **place**: `ℓ(x) = (_LETn, f)`, the cell
  labelled `_LETn` (n is the `let`'s tag) and the field `f ∈ {A, B}` in which `(store x)` occurs. The
  instruction containing `(store x)` is emitted with `a` in that field, so at load time the place
  holds `a`.
- **Store once.** In `(let (x a) e)`, `e` contains **exactly one** `(store x)` that is not under a
  `let` that shadows `x`. Checked: two stores are an error (`Rename.check_single_stores`), and a
  use with no store is an error at the use (`Lib.translate_penv`). Shadowing is resolved before compilation: `Rename.uniquify` gives every binder
  a unique name, so an initializer means the variable visible where its `let` binds it.
- **Stores in conditions.** A `(store x)` inside a condition makes the condition's instruction x's
  place; in `GT`, emitted as `SLT b, a`, the left operand lands in the B-field.
- **Uses.** A bare or `$` use of `x` reads or writes the field `f` of `_LETn`, through the modifier
  that selects `f`. An indirect use (`@`, `<`, `>`) goes through that field (`*`, `{`, `}` when
  `f = A`). `#x` is the offset to `_LETn`, not its value.
- **Condition placement.** `DZ` is only valid as a pre-condition (`if`, `while`); `DN` only as a
  post-condition (`do-while`). Checked: `compile_cond1` rejects the others.
- **Labels.** Generated labels and user labels must be distinct, and no label may be a pMARS
  keyword. Checked in the parser: user names may not start with `_` (reserved for generated labels)
  and a label may not be a pMARS keyword (i-7d2612-425c66).
- **Kinds (proposed, i-7d2612-f2f7c5).** Three sorts of operand — `Num` (a number), `Lab` (a code
  address), `Place = Lab × {A, B}` — with jump targets of sort `Lab`, arithmetic on `Num` or
  `Place`, and `#x` the explicit coercion `Place → Num`. This mirrors Typed Assembly Language's
  separation of integers and code labels. Because Redcode cannot keep an opponent from overwriting
  code, such a check is a lint against our own mistakes, not a soundness guarantee.

## 4. Dynamics: what a program does

**The structured fragment.** The rules below define programs whose primitives do not transfer
control (no `JMP`, `SPL`, `JMZ`, `JMN`, `DJN`, `SEQ`, `SNE`, `SLT` written by hand), do not
overwrite their own code, and run as one process. Outside that fragment RED is an assembler with
structured sugar: its meaning is the compiled redcode under ICWS'94, and nothing more is promised.

A configuration is `⟨e, σ⟩`: the remaining program and the core. A condition is evaluated with its
effect: `⟦c⟧σ = (b, σ′)`, where `DZ`/`DN` decrement their operand first and every comparison is
unsigned modulo `M`.

```
⟨(repeat e), σ⟩        →  ⟨(seq e (repeat e)), σ⟩
⟨(if c e), σ⟩          →  ⟨e, σ′⟩       if ⟦c⟧σ = (true, σ′);   ⟨skip, σ′⟩ if (false, σ′)
⟨(if c e₁ e₂), σ⟩      →  ⟨e₁, σ′⟩      if ⟦c⟧σ = (true, σ′);   ⟨e₂, σ′⟩   if (false, σ′)
⟨(while c e), σ⟩       →  ⟨(seq e (while c e)), σ′⟩  if ⟦c⟧σ = (true, σ′);  ⟨skip, σ′⟩ if (false, σ′)
⟨(do-while c e), σ⟩    →  ⟨(seq e (if c (do-while c e))), σ⟩
⟨(let (x a) e), σ⟩     →  ⟨e, σ⟩        (binding is static: x already denotes ℓ(x))
⟨(seq skip e …), σ⟩    →  ⟨(seq e …), σ⟩,   ⟨(seq), σ⟩ → ⟨skip, σ⟩,   and seq steps its first element
⟨(OP m a₁ a₂), σ⟩      →  ⟨skip, σ′⟩    where σ′ is one ICWS'94 execution of the compiled instruction
```

**Falling off the end.** A program that reaches `skip` executes the epilogue `DAT` that
`compile_prog` appends, and the process dies. So "terminates" in RED means "the warrior dies";
most useful warriors never reach `skip`.

## 5. The compilation scheme

Where each construct goes (`compile_expr`, `compile_cond` in `src/compile.ml`). `⟦c⟧pre→t` jumps
to `t` when `c` is **false**; `⟦c⟧post→t` jumps to `t` when `c` is **true**.

| Construct | Emitted |
|---|---|
| `(repeat e)` | `_REPn: ⟦e⟧; JMP _REPn` |
| `(if c e)` | `⟦c⟧pre→_IFn; ⟦e⟧; _IFn:` |
| `(if c e₁ e₂)` | `⟦c⟧pre→_IFMn; ⟦e₁⟧; JMP _IFFn; _IFMn: ⟦e₂⟧; _IFFn:` |
| `(while c e)` | `_WHIn: ⟦c⟧pre→_WHFn; ⟦e⟧; JMP _WHIn; _WHFn:` |
| `(do-while c e)` | `_DWHn: ⟦e⟧; ⟦c⟧post→_DWHn` |

| Condition | pre (jump when false) | post (jump when true) |
|---|---|---|
| `JZ a` | `JMN t, a` | `JMZ t, a` |
| `JN a` | `JMZ t, a` | `JMN t, a` |
| `DZ a` | `DJN t, a` | rejected |
| `DN a` | rejected | `DJN t, a` |
| `EQ a b` | `SEQ a, b; JMP t` | `SNE a, b; JMP t` |
| `NE a b` | `SNE a, b; JMP t` | `SEQ a, b; JMP t` |
| `GT a b` | `SLT b, a; JMP t` | `SLT b, a; SNE #0, #1; JMP t` |
| `LT a b` | `SLT a, b; JMP t` | `SLT a, b; SNE #0, #1; JMP t` |

`SEQ`/`SNE`/`SLT` skip the next instruction when their test holds; `SLT` is strict `<`. A post-condition
`GT`/`LT` cannot skip on false with `SLT`, so `SNE #0, #1` (always skips) sits between the test and the
jump: true skips it and jumps back, false runs it and skips the jump (i-7d2612-fffa6c).

## 6. Correctness, and how it is checked

**The statement**, in the shape CompCert uses: for every program `P` in the structured fragment
that the compiler accepts, run as one process with no other warrior writing into it, there is a
relation `~` between RED configurations and core states such that the initial states are related,
`σ(x) = core[_LETn].f` for every variable, and every RED step `⟨e, σ⟩ → ⟨e′, σ′⟩` is matched by one
or more ICWS'94 steps that end in a related state. Reaching `skip` is matched by the process dying
on the epilogue `DAT`. Because MARS is deterministic (FIFO queues, fixed `SPL` order), a forward
simulation suffices.

**What checks it today** — none of it is a proof:

| Check | Shows | Runs |
|---|---|---|
| `compare` suite | the emitted text is the recorded one | `make tests F=compare` |
| `execute` suite | pMARS assembles it under 94b (`-A`; nothing executes) | `make tests F=execute`, Linux x86-64 |
| behaviour specs | instances of the statement: alive/dead after N instructions, a cell's content | `python3 tools/behave.py` |
| reference interpreter (planned) | `P` and its compiled code agree cell by cell (translation validation) | i-7d2612-56302d |

## 6a. Metrics

What the compiler measures on its own output (`src/metrics.ml`; definitions and reference values in
`docs/specs/2026-10-03-cost-model-design.md`). A *cycle* is one instruction executed by one process.

| Metric | Meaning |
|---|---|
| `length` | cells emitted, epilogue included |
| `nonzero` / `nonblank` | cells a `JMZ.F` / `SEQ.I` scanner can tell from empty core |
| `boot` | cycles before the first loop starts |
| `cycles/iter` | instructions per loop iteration |
| `overhead/iter` | of those, the jumps and skips a control construct added |
| step prediction | a pointer that advances `k` per iteration visits `CORESIZE / gcd(k, CORESIZE)` cells |
| counter prediction | a `DJN` from `n` runs `n` iterations (`0` runs `CORESIZE`) |

These measure the static program: self-modifying code and indirect jumps are not followed, and a
prediction is absent rather than guessed. They are claims about the redcode, checked against pMARS
where a behaviour spec exists (prog7's counter: 202 instructions, both predicted and measured).

## 7. Vocabulary

- **Syntax and semantics.** Syntax is which s-expressions parse (`src/parse.ml`); semantics is what
  they do to the core.
- **Abstract syntax tree.** The parsed tree (`Ast.expr`), and the same tree with a tag per node
  (`tag eexpr`).
- **Binding, scope, shadowing.** Where a name is introduced and where it is visible; an inner binding
  of the same name hides the outer one.
- **α-equivalence.** Renaming every bound variable consistently gives the same program; here, the
  same redcode up to label names.
- **Capture-avoiding substitution, hygiene.** Substituting a term must not let its free names be
  captured by a binder at the destination. Hygiene is the same demand on names a tool generates:
  `_IF3` or `_LET1` must not capture, or be captured by, a user's label (Kohlbecker et al., 1986);
  here the `_` prefix, which user names may not use, guarantees it.
- **Environment and store.** The environment maps names to places (`aenv`, `penv`, `lenv` in
  `src/lib.ml`); the store maps places to values (the core).
- **Small-step and big-step semantics.** A relation between successive configurations (structural
  operational semantics, Plotkin) versus a relation from a program to its final state (natural
  semantics). RED needs small-step: warriors usually never finish.
- **Denotational semantics.** Each phrase is mapped compositionally to a mathematical object, such
  as a store transformer. It fits RED's structured constructs and not self-modifying code.
- **Type soundness.** *Progress* (a well-typed program is finished or can take a step) plus
  *preservation* (a step keeps it well typed).
- **Call-by-value, call-by-name.** Evaluate an argument before substituting it, or substitute it
  unevaluated. A RED `let` initializer is pasted unevaluated into the store site: call-by-name over
  syntax.
- **Paradigms.** Functional programming descends from the lambda calculus (Church), imperative
  programming from the Turing and von Neumann machines. Redcode is von Neumann in its purest form —
  code and data share one store and programs rewrite themselves. RED is a structured imperative
  language over that store; it has no functions.
- **Curry–Howard.** Types correspond to propositions and programs to proofs; the reason type
  systems and logics share their structure.

### The lambda cube

Barendregt's lambda cube orders eight typed lambda calculi by three independent ways a term or a
type may depend on another:

```
          λω ──────── λC
         ╱ │         ╱ │          up:            terms depending on types   (polymorphism)
       λ2 ──────── λP2 │          back:          types depending on types   (type operators)
        │  λω_ ─────│── λPω_      right:         types depending on terms   (dependent types)
        │ ╱         │ ╱
       λ→ ──────── λP
```

- **λ→**: the simply typed lambda calculus, the origin.
- **λ2**: System F (polymorphism); **λω_** ("weak λω"): type operators; **λP**: dependent types
  (the logical framework LF).
- **λω** = λ2 + type operators (System Fω); **λP2**, **λPω_**: the other combinations of two axes.
- **λC**: all three, the Calculus of Constructions, the core of proof assistants such as Coq/Rocq.

Where this repository sits on it:

- **RED today is not a lambda calculus**: it has no functions and no types; it is a first-order
  imperative language whose names denote places.
- **The dev branch's surface language** (i-7d2612-ec4d2d) is **λ→** with base types `Unit`, `Bool`,
  `Int`: the origin of the cube. Its strong normalization is what would justify inlining every
  lambda away before code generation.
- **The proposed kinds** (`Num`, `Lab`, `Place`, §3) are not on the cube: they are a first-order
  sort discipline, the kind Typed Assembly Language uses to keep integers apart from code labels.
- Moving up the cube (polymorphism, type operators, dependent types) has no use in sight here; the
  constraints that matter are field placement, length and cycles.

## 8. Reading list

In the order a contributor would use them; links in `docs/references.md`.

1. ICWS'94 draft — the target's semantics, and the only executable one (its reference simulator).
2. Nielson & Nielson, *Semantics with Applications* — While, its operational semantics, and a
   provably correct compiler to an abstract machine with jumps.
3. Pierce et al., *Programming Language Foundations* (Software Foundations vol. 2).
4. Leroy, *Formal verification of a realistic compiler* — the correctness statement and simulation.
5. Morrisett et al., *From System F to Typed Assembly Language* — types for assembly.
6. Pierce, *Types and Programming Languages*; Harper, *Practical Foundations for Programming
   Languages*; Barendregt, *Lambda calculi with types* — for the lambda calculus and the cube.
