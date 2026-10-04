# The macro layer: typed templates and `for` — design

Date: 2026-10-04 (s-7d2612-140ece). Roadmap item i-7d2612-ec4d2d, which the snippets
(i-7d2612-8e9549) and the quickscan wait for. Replaces the plan to merge the dev branch's λ→, which
has no type for RED code and no repetition (d-7d2612-5de7a6 chose the direction; this is its form).

## The user's decisions (2026-10-04)

1. **Typed templates plus `for`**, a first-order λ→: templates take typed parameters and are
   expanded before compilation; `for` repeats a fragment over a range of constants.
2. **Labels fresh per expansion**: a label a template defines is renamed at each expansion, unless it
   arrives as a parameter.

## Syntax

```
(program
  (const step 400)
  (define (probe (k Num) (miss Lab))
    (if (EQ I (Dir (+ start (* k step))) (Dir (+ start (* k step) 4))) (JMP miss)))
  (seq (label start) (for k 1 8 (probe k next)) (label next) ...))
```

- `(define (name (param Kind) ...) body)` in the header; `body` is one RED expression.
- A call `(name arg ...)` stands where an expression (a statement) stands.
- `(for k lo hi body)` stands where an expression stands: `body` once for each integer `k` from `lo`
  to `hi`, in order, as a `seq`.

Kinds, checked at each call:

| Kind | Argument | Stands for, in the body |
|---|---|---|
| `Num` | a number, a constant, a label or an expression `(op a b)` | an operand or a term of an expression |
| `Lab` | a label name | a label: in `(label ...)`, as a jump target, in an expression |
| `Var` | a `let` variable in scope at the call | that variable (its cell) |
| `Code` | a RED expression | a statement |

## Expansion

At the s-expression level, in the parser, before anything else: a call is replaced by its body with
each parameter's atom replaced by the argument, then expanded again (its own calls and `for`s). A
`for` evaluates `lo` and `hi` when compiling (numbers, constants, enclosing `for` variables and `Num`
parameters bound to numbers) and expands `body` with `k` replaced by each number.

**Hygiene.** At each expansion, every label the template body defines (`(label l)`) and every `let`
binder it introduces is renamed `_X<n>_<name>`, `n` counting the program's expansions in source
order (per compilation, never a global counter: d-7d2612-123e41), and every reference inside the body
follows. Arguments are substituted after the renaming, so a name passed in is never renamed and never
captured. User names may not start with `_`, so a renamed name never meets one.

**Termination.** A template may call only templates defined before it, so a call never reaches
itself; a `for` runs a known number of times (at most 1000). Every expansion therefore ends, and
the warrior's length limit bounds what it produces.

**Locations.** An expanded node carries the location of the call (or the `for`) that produced it, so
an error inside an expansion says where the call is.

## What it costs

A template emits exactly what its body emits once per call: no code of its own, no runtime cost. The
cost model measures the expanded program as any other.

## Tests

Alcotest cases for substitution of each kind, kind errors, arity, hygiene (two expansions defining
one label), no capture of a `Var` argument by a template's own `let`, calling a later template
(error), `for` bounds (constant, unknown, too many), locations; goldens for an expansion; the
quickscan archetype, written with a template and `for`, with its behaviour spec and benchmark.
