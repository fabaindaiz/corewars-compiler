# Phase 3: the optimizer under the policy — design

Date: 2026-10-04. Roadmap items i-7d2612-3ca4c2 (loop rotation), i-7d2612-400784 (variables in
fields of existing instructions), i-7d2612-ec59a0 (peephole, the part left after threading). The
cost model (`docs/specs/2026-10-03-cost-model-design.md`) measures every choice below.

## The user's decisions (2026-10-04)

1. **Rotation:** a `while` with a unary condition (`JZ`, `JN`) is rotated; a binary one (`EQ`, `NE`,
   `GT`, `LT`) only when the policy picks it. `DZ` is never rotated: no single instruction
   decrements and loops while zero.
2. **Variables in fields:** explicit. `(repeat body arg)` puts `arg` in the B-field of the
   `repeat`'s own `JMP`, which `JMP` ignores; `(repeat body (store p))` makes that field `p`'s
   place. The compiler never moves a `(store x)` the user wrote.
3. **Policy:** measure and choose. The compiler compiles the whole program with each combination
   of optional transformations, measures each with the cost model, and keeps the one the policy
   prefers.

## Measure and choose

`Compile.options` holds one switch per optional transformation: `rotate_unary`, `rotate_binary`,
`peephole`. `Compile.compile_body ~opts` honours them; with `Compile.no_opts` it emits what the
compiler emitted before phase 3. `Optimize.choose policy e` enumerates the 2^k option sets in a
fixed order, fewest transformations first, measures each (`Layout.build`, `Metrics.measure`), and
keeps the least under `Metrics.compare policy`; a tie keeps the earlier set, so a transformation is
applied only when it improves the first objective that tells the variants apart. The driver
compiles, reports and checks `(expect ...)` against the chosen variant, and the `compare` suite
compiles goldens the same way with the default policy, so a golden is what `run_compile.exe`
prints. Jump threading (d-7d2612-3f3f32) stays unconditional: it never adds a cell or a cycle.

With k = 3 a program is compiled eight times; each compile is linear in its length (a warrior is
at most 100 cells), so the cost is negligible next to running pMARS once.

## Rotated `while`

```
(while c e), unrotated:  _WHIn: ⟦c⟧pre→_WHFn; ⟦e⟧; JMP _WHIn; _WHFn:
(while c e), rotated:    JMP _WHCn; _WHIn: ⟦e⟧; _WHCn: ⟦c⟧post→_WHIn; _WHFn:
```

`while c e` ≡ `if c (do-while c e)`: the entry jump goes to the test, the test loops back while the
condition holds. Per iteration, a unary condition costs one control instruction instead of two
(`JMN _WHIn, x` against `JMZ _WHFn, x` plus `JMP _WHIn`); boot costs one more (the entry `JMP`);
cells are equal. A binary condition costs two control instructions either way, one more cycle of
boot, and `GT`/`LT` one more cell (the always-skipping `SNE`): under every objective of today's
policy it is never better, so the default policy never picks it; it is there for a policy that
would. `_WHC` is a new generated-label prefix.

## Peephole

With `peephole`, a generated `JMP`, `JMZ` or `JMN` whose target is the next instruction is removed,
unless the instruction before it can skip (`SEQ`, `SNE`, `SLT`, `CMP`: removing the cell changes
what is skipped) or it holds a variable. `DJN` is kept: it decrements. Cases: an `if` whose body is
empty, an `if`-`else` whose `else` is empty, a rotated `while` whose body is empty.

## `(repeat body arg)`

`arg` is the B operand of the `repeat`'s `JMP`, `#0` when absent. A `(store p)` there places `p` in
that B-field (`_LETn` labels the `JMP` cell). The `JMP` is executed every iteration, so the cell is
code, and a pointer through it (`@p`) reads its B-field as any variable's. The scanner archetype
goes from 7 cells to 6, the hand-written count.

## Tests

Alcotest cases for each layout and for the choice under two policies; a golden and a behaviour spec
for a rotated unary `while` (it dies three instructions sooner than the unrotated one, which the
spec measures); the scanner archetype's golden changes to the `repeat` data form, with its spec.
