# 2. Tutorial: a bomber, step by step

This chapter builds a warrior that bombs the core, then teaches it to look before it bombs, and on
the way shows each part of RED. Save each step in a file of its own (the commands below call the
dwarf `dwarf.src`) and compile it with `dune exec execs/run_compile.exe -- FILE.src`.

## 2.1 Variables are cells

Core War has no memory apart from the core: a number a warrior keeps lives in a field of one of its
own instructions. A RED variable says so. `let` introduces it with its first value, and
`(store b)` says where in the code its cell is:

```red
(let (b 0)
  (seq
    (ADD 4 b)
    (DAT 0 (store b))))
```

```redcode
;redcode-94b

  ADD.AB #4     , $_LET1
_LET1
  DAT    #0     , #0
  DAT    $0     , $0
```

- The `DAT` holding `b` got a label, `_LET1`, and `b` is its B-field because `(store b)` sits in the
  B operand. `ADD 4 b` became `ADD.AB #4, $_LET1`: add the immediate 4 to that B-field. You never
  write the modifier: where you store a variable decides which field every use reads.
- `seq` runs its expressions in order.
- A bare number is a value (`#4`); a bare name is a variable, or a label if no variable has that
  name.

Put the store in the A operand and every use follows it to the A-field:

```red
(let (b 0)
  (seq
    (ADD 4 b)
    (DAT (store b) 0)))
```

```redcode
;redcode-94b

  ADD.A  #4     , $_LET1
_LET1
  DAT    #0     , #0
  DAT    $0     , $0
```

Every `let` variable is stored exactly once; a second `(store b)` or a use with no store is an error.

## 2.2 A loop: the dwarf

`repeat` runs its body for ever. `(Ind b)` is the cell `b` points to (the `@` mode), and
`(MOV I b (Ind b))` copies the whole cell holding `b` there: a bomb. Here, written with the
modifier `I` because a bomb is a whole instruction:

```red
(let (b 0)
  (seq
    (repeat
      (seq
        (ADD 4 b)
        (MOV I b (Ind b))))
    (DAT 0 (store b))))
```

```redcode
;redcode-94b

_REP4
  ADD.AB #4     , $_LET1
  MOV.I  $_LET1 , @_LET1
  JMP    $_REP4 , #0
_LET1
  DAT    #0     , #0
  DAT    $0     , $0
```

This is Dewdney's dwarf, the same three instructions and the same three cycles a bomb as written
by hand. The `repeat` cost one `JMP`.

**Why 4.** The pointer is cell 3 and every bomb lands 4 cells after the last one, on cells 7, 11, 15,
...: all of them 3 more than a multiple of 4. The code sits in cells 0, 1 and 2, which no bomb ever
reaches, so the dwarf never hits itself. Change the step to 3 and the bombs visit every cell of the
core, its own code included:

```red
(program
  (expect (dead 8000))
  (let (b 0)
    (seq
      (repeat
        (seq
          (ADD 3 b)
          (MOV I b (Ind b))))
      (DAT 0 (store b)))))
```

This warrior drops a bomb on its own `MOV` (cell 1) and dies at its 8000th instruction (`(expect (dead 8000))` holds,
`(expect (alive 7999))` too; see 2.7 for how to run them). The compiler sees it coming and says
so, with the iteration and the cycle:

```text
dwarf3.src:5:7: warning: a pointer here reaches cell 1 of its own loop after 2666 iterations (7998 cycles): a write through it there hits the warrior's running code
```

## 2.3 What it costs

Ask the compiler:

```sh
dune exec execs/run_compile.exe -- --report dwarf.src
```

It prints the redcode, then on standard error the warrior's length, the cycles each loop spends per
iteration, how many of them are control the compiler added, and predictions, without running
anything. It also warns:

```text
dwarf.src:3:5: warning: a pointer here advances 4 cells per iteration and visits only 2000 of 8000 cells; write (expect (step 4)) if that is meant
```

A step of 4 bombs a quarter of the core, which is the dwarf's design. Say so, and the compiler
checks it instead of warning: an `(expect ...)` inside the loop states what the loop does.

```red
(let (b 0)
  (seq
    (repeat
      (seq
        (expect (step 4))
        (expect (cycles 3))
        (ADD 4 b)
        (MOV I b (Ind b))))
    (DAT 0 (store b))))
```

If the loop ever stops doing what it states, the compilation fails and says which expectation
broke. [Chapter 4](04-tools.md#expectations) lists them all.

## 2.4 Looking before bombing: conditions

A scanner looks at cells and bombs only the ones that are not empty. `if` takes a condition:
`(JN x)` holds when `x` is not zero, and `(JN (Ind p))` when the cell `p` points to is not zero.

```red
(let (p 20)
  (seq
    (repeat
      (seq
        (ADD 10 p)
        (if (JN (Ind p))
          (MOV I bomb (Ind p))))
      (store p))
    (label bomb)
    (DAT 0 0)))
```

```redcode
;redcode-94b

_REP4
  ADD.AB #10    , $_LET1
  JMZ.B  $_REP4 , @_LET1
  MOV.I  $bomb  , @_LET1
_IF9
_LET1
  JMP    $_REP4 , #20
bomb
  DAT    #0     , #0
  DAT    $0     , $0
```

Three things to notice:

- `(repeat body (store p))` keeps `p` in the B-field of the loop's own `JMP`, a cell the loop needs
  anyway: one cell less than a `DAT` of its own.
- The `if` compiled to `JMZ`: when the cell is empty, jump straight back to the loop's head. The
  compiler aimed it at the head rather than at the `JMP`, one cycle sooner: an empty cell costs 2
  cycles, as in the best hand-written scanner.
- `(label bomb)` names a cell you can use as an operand.

`(JN (Ind p))` tests the **B-field** of the cell `p` points to, as `JMZ`/`JMN` do by default: a cell
like `JMP $5, #0` has a zero B-field and reads as empty. A scanner that must see every non-empty cell
compares whole cells instead, as the SEQ scanner in [chapter 3](03-cookbook.md) does.

The conditions are `JZ`, `JN` (zero, not zero), `DZ`, `DN` (decrement, then test), and the
comparisons `EQ`, `NE`, `GT`, `LT`. The constructs are `if` (with or without an else), `while`,
`do-while` and `repeat`.

## 2.5 The header: hill, name, constants

A file may wrap its program in `(program ...)` with a header. The header names the hill, the
warrior, its author and its strategy, defines constants, and states expectations about the whole
warrior:

```red
(program
  (hill 94nop)
  (name scanner)
  (author you)
  (strategy look at every tenth cell and bomb what is there)
  (const stride 10)
  (expect (length <= 8))
  (let (p 20)
    (seq
      (repeat
        (seq
          (ADD stride p)
          (if (JN (Ind p))
            (MOV I bomb (Ind p))))
        (store p))
      (label bomb)
      (DAT 0 0))))
```

```redcode
;redcode-94nop
;name scanner
;author you
;strategy look at every tenth cell and bomb what is there
;assert CORESIZE==8000 && MAXLENGTH==100
stride EQU 10

_REP4
  ADD.AB #stride, $_LET1
  JMZ.B  $_REP4 , @_LET1
  MOV.I  $bomb  , @_LET1
_IF9
_LET1
  JMP    $_REP4 , #20
bomb
  DAT    #0     , #0
  DAT    $0     , $0
```

A constant is emitted as an `EQU`, so the warrior keeps its names and an optimizer can tune it
later. `(hill 94nop)` set the `;redcode-94nop` line and the `;assert`, and the compiler measures the
warrior on that hill's core.

## 2.6 Writing it once: templates and snippets

A template is a named fragment with typed parameters: `Num` (a number, constant, label or
expression), `Lab` (a label), `Var` (a `let` variable) or `Code` (a RED expression). `for` repeats
a fragment at compile time:

```red
(program
  (define (probe (k Num) (miss Lab))
    (if (NE I (Dir (+ top (* k 400))) (Dir (+ top (+ (* k 400) 100))))
      (JMP miss)))
  (seq
    (label top)
    (for k 1 3 (probe k found))
    (JMP top)
    (label found)
    (DAT 0 0)))
```

```redcode
;redcode-94b

top
  SEQ.I  $top+(1*400), $top+((1*400)+100)
  JMP    $found , #0
_IF7
  SEQ.I  $top+(2*400), $top+((2*400)+100)
  JMP    $found , #0
_IF10
  SEQ.I  $top+(3*400), $top+((3*400)+100)
  JMP    $found , #0
_IF13
  JMP    $top   , #0
found
  DAT    #0     , #0
  DAT    $0     , $0
```

Each call emitted exactly its body, and an `if` around one instruction became a skip: `SEQ` jumps
over the `JMP found` when the two cells are equal. Labels a template defines are its own at every
call, so calling it twice never clashes.

Templates you use often go in a file, and `(include "path")` brings them in. The path is relative
to the file that includes it: from your own directory, copy `snippets/` next to your program or
write the path from there (`(include "../snippets/bomber.src")`). The repository comes with a
catalogue of verified templates in `snippets/`; its `README.md` lists each one's parameters and
cost. The stone, a dwarf with a faster step behind an
`SPL 0` that keeps starting it again, in three lines:

```red
(program
  (include "snippets/bomber.src")
  (seq
    (SPL 0)
    (bomber 3044)))
```

```redcode
;redcode-94b

  SPL    #0     , #0
_REP8
  ADD.AB #3044  , $_LET5
  MOV.I  $_LET5 , @_LET5
  JMP    $_REP8 , #0
_LET5
  DAT    #0     , #0
  DAT    $0     , $0
```

## 2.7 Testing what it does

The compiler can check what it can measure; what the warrior does in the core is checked by
running it. State it with the expectations `alive`, `dead` and `cell`:

```red
(program
  (expect (alive 1000))
  (expect (cell 7 "DAT.F #0, #4" 2))
  (let (b 0)
    (seq
      (repeat
        (seq
          (expect (step 4))
          (ADD 4 b)
          (MOV I b (Ind b))))
      (DAT 0 (store b)))))
```

`(cell 7 "DAT.F #0, #4" 2)`: after 2 instructions, cell 7 holds the bomb (the pointer is cell 3,
plus 4). Export them as a behaviour spec and run it in pMARS:

```sh
dune exec execs/run_compile.exe -- --emit-beh dwarf.beh dwarf.src
python3 tools/behave.py dwarf.beh
```

`--emit-beh` writes the spec and the redcode beside it (`dwarf.red`). A round lasts 80000 cycles
on 94b, so `(alive N)` with N of 80000 or more never holds: every warrior is "dead" when the round
is over.

## 2.8 Measuring it against others

`tools/bench.py` scores a warrior against the Wilkies benchmark and the top 20 of the Koenigstuhl
94nop hill (downloaded on first use), with a fixed seed so a score repeats exactly:

```sh
python3 tools/bench.py dwarf.src
python3 tools/bench.py --hill dwarf.src      # also its estimated place on the whole hill (slow)
```

300 beats everything, 100 ties everything. The archetypes in RED score exactly as their
hand-written twins: what separates a warrior from the top of the hill is its strategy, which is
where the [cookbook](03-cookbook.md) starts.
