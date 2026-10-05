# 5. Workflows: what to use for what

The tools of chapter 4 are cheap to run (measured on a laptop: compiling a warrior with its report
takes under 10 ms, a behaviour spec 0.07 s, the benchmark against Wilkies and the top 20 about
6 s; the placement on the whole hill of 1107 warriors about 90 s), so the best workflow runs them early and often,
cheapest first. This chapter says which ones each kind of work needs.

## Which tool answers which question

| You want to know | Use | Cost |
|---|---|---|
| does it compile, what redcode does it give | `run_compile.exe w.src` | instant |
| what does it cost: cells, cycles per loop, boot | `--report` | instant |
| does a loop still do what I designed | `(expect (cycles 3))`, `(expect (step 3044))` in the loop | instant, on every compile |
| what does it do in the core | `(expect (alive N))`, `(expect (cell ...))`, then `--emit-beh` and `tools/behave.py` | under a second |
| why does it misbehave | cdb: `skip`, `list`, `registers` (chapter 4) | minutes, by hand |
| how strong is it | `tools/bench.py w.src` | seconds |
| where would it place on a hill | `tools/bench.py --hill w.src` | about 90 s a warrior |

The order matters: an expectation the compiler checks costs nothing and catches a broken loop the
moment you break it; a benchmark tells you only that something got worse.

## Use case: learning Core War

You want to see how warriors work. Read [chapter 1](01-getting-started.md) and the
[tutorial](02-tutorial.md) in order. For each warrior:

1. Compile it and read the redcode beside the RED: the compiler writes the modifiers and modes you
   would otherwise have to learn first.
2. Step it in cdb (`skip`, `list`) and watch the cells change.
3. Fight it against the dwarf or the imp, 200 rounds, and change one number.

## Use case: writing a warrior for a hill

You want a competitive warrior. The loop:

1. **Start from a strategy that works.** Take a recipe from the [cookbook](03-cookbook.md) or
   include a snippet; every one of them compiles to the same code as its hand-written twin, so the
   compiler costs you nothing to start from.
2. **Name the hill** in the header, `(hill 94nop)`, so the warrior is measured and asserted on the
   core it will fight on, and give it a `(name ...)`.
3. **Make the constants constants.** `(const step 3044)` keeps them as `EQU`s you can tune without
   touching the code.
4. **State what each loop does** with `expect`: its step, its cycles. From now on the compiler
   tells you the moment an edit breaks it.
5. **State what the warrior does** with `alive` and `cell`, export the spec, run it.
6. **Measure**: `tools/bench.py` after each change of strategy; `--hill` before you call it done.
7. **Tune**: change one constant at a time and measure again. The archetypes' measurements in the
   repository found that a constant sweep moves a paper by a few points; the strategy moves it by
   tens.

Composing two strategies is one `SPL`: the second starts beside the first.

```red
(program
  (hill 94nop)
  (name stone and imp)
  (include "snippets/bomber.src")
  (include "snippets/imp.src")
  (seq
    (SPL impgo)
    (SPL 0)
    (bomber 3044)
    (label impgo)
    (imp)))
```

```redcode
;redcode-94nop
;name stone and imp
;assert CORESIZE==8000 && MAXLENGTH==100

  SPL    $impgo , #0
  SPL    #0     , #0
_REP10
  ADD.AB #3044  , $_LET7
  MOV.I  $_LET7 , @_LET7
  JMP    $_REP10, #0
_LET7
  DAT    #0     , #0
impgo
  MOV.I  $0     , $1
  DAT    $0     , $0
```

## Use case: testing an idea quickly

You want to know whether a trick works, not to finish a warrior. Write the smallest program that
has the trick, then:

1. `--report` for its cost.
2. One `expect (cell ...)` that would be true if the trick works, exported and run: the answer in
   under a second, and it stays as a test if the idea survives.
3. If the cell is wrong, cdb shows why (`skip N`, `list A,B`).

## Use case: teaching

The examples in this manual are compiled by the test suite, so what students see is what the
compiler does today. Show a construct in RED and its redcode side by side (the
single-page version of this manual, built by `tools/manual_page.py`, does exactly that), then
`--report` to explain what it costs. A student's
mistake prints `file:line:column: error:` with what is wrong; the table at the end of
[chapter 4](04-tools.md) covers the usual ones.

## Use case: a warrior misbehaves

1. `--report`: its loops, their cycles, what the compiler predicts; a warning may already say it.
2. `--warn=all`: every warning, whatever the policy.
3. cdb at the instruction where it goes wrong.
4. Turn what you found into a `cell` expectation, so it cannot come back.
