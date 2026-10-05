# Snippets

Named RED fragments, each a template (`define`) with its cost measured and its behaviour specified.
A program uses one with `(include "snippets/NAME.src")` in its header, the path relative to the
program's own file (d-7d2612-c2df7c). Each has a demo in `bbctests/snippets/` that compiles to the
code of its archetype (`archetypes/`, labels renamed) and a behaviour spec in
`behtests/snippet_*.beh` with that archetype's probes.

Costs from `run_compile.exe --report` on the demo, epilogue excluded; the paper's per copy is counted
by hand from its loops (`--report` counts an inner loop's body once). `test_phase8_snippet_demos_are_their_archetypes`
enforces that each demo compiles to its archetype's code.

| Snippet | Templates and parameters | Cells | Cycles | Demo, spec |
|---|---|---|---|---|
| `imp.src` | `(imp)` | 1 | 1 a step | `imp.bbc`, `snippet_imp.beh` |
| `bomber.src` | `(bomber (stride Num))` | 4 | 3 a bomb | `dwarf.bbc` (stride 4), `stone.bbc` (behind `(SPL 0)`, stride 3044) |
| `scanner.src` | `(scanner (stride Num) (first Num))` | 5 | 2 an empty cell, 4 a bombed one | `scanner.bbc` |
| `clear.src` | `(clear (entry Lab) (first Num))`, with `(start entry)` | 4 | 2 a cell | `clear.bbc` |
| `paper.src` | `(paper (dist Num) (stride Num))` | 7 | 18 a copy: 4, and 2 for each of its 7 cells | `paper.bbc` |
| `quickscan.src` | `(probes (first Lab) (hits Lab) (stride Num) (gap Num))`, `(stubs (first Lab) (ptr Lab) (found Lab) (stride Num))` | 32 and 32 | 1 a probe on equal cells | `quickscan.bbc` |

`step` is a RED word (an expectation), so the parameters are named `stride`. A template's labels and
lets are its own at each call; the names passed in are used as they are (`LANGUAGE.md`, *Templates*).

Adding one: write the template in `snippets/NAME.src` with a comment saying what it does and what
it costs (measured), a demo golden in `bbctests/snippets/`, and a spec in `behtests/`; add its row
here.
