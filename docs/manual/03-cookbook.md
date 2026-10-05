# 3. Cookbook: the classic strategies in RED

Each recipe is a complete warrior. Each of them is also in `archetypes/` beside a hand-written
twin, and compiles to the same code and scores the same; `snippets/` has several as templates you
can include. Scores below are against the Wilkies benchmark (`tools/bench.py`, fixed seed): 300
beats everything, 100 ties.

## A bomber: the dwarf

Drop a bomb every fourth cell, for ever. 4 cells, 3 cycles a bomb.

```red
(let (b 0)
  (seq
    (repeat
      (seq
        (expect (step 4))
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

## A stone: a bomber that keeps restarting

`SPL 0` starts a new process at the loop on every cycle it runs, so the bomber survives having
some of its processes killed; a step of 3044 bombs cells far apart before it closes in. Wilkies
80.5, the best of the bombers here.

```red
(let (b 0)
  (seq
    (SPL 0)
    (repeat
      (seq
        (expect (step 3044))
        (ADD 3044 b)
        (MOV I b (Ind b))))
    (DAT 0 (store b))))
```

With the snippet, the same warrior:

```red
(program
  (include "snippets/bomber.src")
  (seq (SPL 0) (bomber 3044)))
```

## A scanner: look, then bomb

Look at every tenth cell and bomb the ones that are not empty. 2 cycles an empty cell.

```red
(let (p 20)
  (seq
    (repeat
      (seq
        (expect (step 10))
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

## A SEQ scanner: compare two cells at once

Two pointers 4 cells apart step together; `(NE I (Ind a) (Ind b))` compares the two cells they
point to, whole, and bombs where they differ. `(ADD F inc ptrs)` adds both fields of one cell to
both fields of another: both pointers move in one instruction.

```red
(let (a 100)
  (let (b 104)
    (seq
      (repeat
        (seq
          (ADD F inc ptrs)
          (if (NE I (Ind a) (Ind b))
            (MOV I bomb (Ind b)))))
      (label ptrs)
      (DAT (store a) (store b))
      (label inc)
      (DAT 8 8)
      (label bomb)
      (DAT 0 0))))
```

```redcode
;redcode-94b

_REP5
  ADD.F  $inc   , $ptrs
  SNE.I  *_LET1 , @_LET2
  JMP    $_REP5 , #0
  MOV.I  $bomb  , @_LET2
_IF10
  JMP    $_REP5 , #0
ptrs
_LET1
_LET2
  DAT    #100   , #104
inc
  DAT    #8     , #8
bomb
  DAT    #0     , #0
  DAT    $0     , $0
```

## A core-clear: data before the code

Write a bomb into every cell after the warrior, two cycles a cell. Its pointer sits before its code,
so the warrior starts at `top`: `(start top)` emits `ORG top`.

```red
(program
  (start top)
  (let (p 4)
    (seq
      (DAT 0 (store p))
      (label top)
      (repeat (MOV I bomb (Inc p)))
      (label bomb)
      (DAT 0 0))))
```

```redcode
;redcode-94b
ORG top

_LET1
  DAT    #0     , #4
top
_REP8
  MOV.I  $bomb  , >_LET1
  JMP    $_REP8 , #0
bomb
  DAT    #0     , #0
  DAT    $0     , $0
```

## A paper: copy yourself, start the copy

Copy the warrior's 7 cells 2000 cells away, start a process there, move the target and do it again.
The copies outnumber what a bomber can kill. `do-while` with `(JN n)` runs the copy while the count
is not zero; `(Dec n)` and `(Dec d)` move both pointers back one cell at each copy. Wilkies 81.0.

```red
(let (n 7)
  (let (d 2000)
    (seq
      (repeat
        (seq
          (MOV 7 (store n))
          (do-while (JN n) (MOV I (Dec n) (Dec d)))
          (SPL (Ind d))
          (ADD 2365 d)))
      (DAT 0 (store d)))))
```

```redcode
;redcode-94b

_REP5
_LET1
  MOV.AB #7     , #7
_DWH10
  MOV.I  <_LET1 , <_LET2
  JMN.B  $_DWH10, $_LET1
  SPL    @_LET2 , #0
  ADD.AB #2365  , $_LET2
  JMP    $_REP5 , #0
_LET2
  DAT    #0     , #2000
  DAT    $0     , $0
```

## Copy by index: the idea of Mice

The counter sits before the code, the loop copies the seven cells after it through the counter,
`DN` decrements it and loops while it is not zero, and `JMZ` starts again. Wilkies 82.4, the best
archetype against that benchmark.

```red
(program
  (start entry)
  (let (count 0)
    (let (target 1500)
      (seq
        (DAT 0 (store count))
        (label entry)
        (MOV 7 count)
        (do-while (DN count) (MOV I (Ind count) (Dec target)))
        (SPL (Ind target))
        (ADD 2903 target)
        (JMZ entry count)
        (DAT 0 (store target))))))
```

## A Silk-style paper: copying through the A-field

Eight processes run the same two cells. The `SPL` starts each at the copy; the `MOV` copies one
cell through the A-field postincrement `}` and the B-field one `>` of the `SPL`'s own cell. RED
writes `}`, `{` and `*` on numbers, labels and expressions (`AInc`, `ADec`, `AInd`); on a variable,
its store decides the field. A jump target is written `(Dir 1)`: a bare `1` would be the value
`#1`, and `SPL #1` splits onto its own cell.

```red
(seq
  (SPL (Dir 1))
  (SPL (Dir 1))
  (SPL (Dir 1))
  (label silk)
  (SPL (Ind 0) (} 2731))
  (MOV I (} silk) (Inc silk))
  (MOV I bomb (Inc 2000))
  (label bomb)
  (DAT 0 0))
```

```redcode
;redcode-94b

  SPL    $1     , #0
  SPL    $1     , #0
  SPL    $1     , #0
silk
  SPL    @0     , }2731
  MOV.I  }silk  , >silk
  MOV.I  $bomb  , >2000
bomb
  DAT    #0     , #0
  DAT    $0     , $0
```

## An imp ring: label arithmetic

Three imps 2667 cells apart run in step and repair each other: 3 × 2667 = 8001, so after the
three of them have moved, the ring as a whole has advanced one cell. A constant and expressions on labels place the
launches; `(JMP (Dec vec))` jumps through a pointer that each process decrements first, so the
three processes take the three launch cells in turn.

```red
(program
  (const step 2667)
  (seq
    (SPL (Dir 2))
    (SPL (Dir 1))
    (JMP (Dec vec))
    (JMP (+ imp (* 2 step)))
    (JMP (+ imp step))
    (JMP imp)
    (label vec)
    (DAT 0 0)
    (label imp)
    (MOV I (# 0) (Dir step))))
```

```redcode
;redcode-94b
step EQU 2667

  SPL    $2     , #0
  SPL    $1     , #0
  JMP    <vec   , #0
  JMP    $imp+(2*step), #0
  JMP    $imp+step, #0
  JMP    $imp   , #0
vec
  DAT    #0     , #0
imp
  MOV.I  #0     , $step
  DAT    $0     , $0
```

## A quickscan: sixteen comparisons, written once

Compare 16 pairs of cells across the core before doing anything else; a pair that differs is
probably an enemy, so bomb there first, then clear. The probes and their stubs are two templates
from `snippets/quickscan.src`, each expanded sixteen times by a `for`. The warrior is 73 cells.

```red
(program
  (hill 94nop)
  (name quickscan)
  (const step 400)
  (const gap 100)
  (include "snippets/quickscan.src")
  (seq
    (label first)
    (probes first hits step gap)
    (JMP clear)
    (label hits)
    (stubs first ptr found step)
    (label found)
    (MOV I bomb (Ind ptr))
    (ADD gap ptr)
    (MOV I bomb (Ind ptr))
    (label clear)
    (repeat (MOV I bomb (Inc ptr)))
    (label ptr)
    (DAT 0 300)
    (label bomb)
    (DAT 0 0)))
```

## Two strategies in one warrior

`(SPL launch)` starts a second strategy beside the first: here a stone with an imp ring next to it.
Wilkies 86.9, the best of the warriors in this repository.

```red
(program
  (hill 94nop)
  (name stone and imp ring)
  (strategy a mod-4 stone beside a three-point imp ring)
  (const step 3044)
  (const istep 2667)
  (let (b 0)
    (seq
      (SPL launch)
      (SPL 0)
      (repeat (seq (expect (step 3044)) (ADD step b) (MOV I b (Ind b))))
      (DAT 0 (store b))
      (label launch)
      (SPL (Dir 2))
      (SPL (Dir 1))
      (JMP (Dec vec))
      (JMP (+ imp (* 2 istep)))
      (JMP (+ imp istep))
      (JMP imp)
      (label vec)
      (DAT 0 0)
      (label imp)
      (MOV I (# 0) (Dir istep)))))
```
