# 0. Core War in five minutes

Skip this chapter if you have written redcode before.

## The game

Two programs, called **warriors**, are loaded into a circular memory, the **core**, and take turns
executing one instruction each. A warrior loses when it has nothing left to run; the usual way to
make that happen is to get the other warrior to execute a `DAT`, the instruction that kills the
process running it. A **round** that reaches its cycle limit with both warriors alive is a tie.

On the default settings this manual uses (the 94b hill, ICWS'94 rules):

| Setting | Value |
|---|---|
| core size | 8000 cells, numbered 0 to 7999; cell 8000 is cell 0 again |
| longest warrior | 100 cells |
| cycle limit | 80000 instructions a round, per warrior |
| processes | up to 8000 per warrior |

## A cell is an instruction

Every cell of the core holds one instruction with two operands, the **A-field** and the
**B-field**:

```text
MOV.I  $0, $1        opcode . modifier   A-operand, B-operand
```

There is no separate data memory: a number a warrior keeps is a field of one of its own
instructions. Empty core is `DAT $0, $0` everywhere.

**Addresses are relative.** `$1` is the next cell after the one executing, `$-1` the one before;
every address is taken modulo the core size.

**Addressing modes** say what an operand means:

| Mode | Name | Means |
|---|---|---|
| `#` | immediate | the number itself |
| `$` | direct | the cell that many cells away |
| `@`, `*` | indirect | the cell that the B-field (`@`) or A-field (`*`) of that cell points to |
| `<`, `{` | predecrement | the same, after decrementing that field |
| `>`, `}` | postincrement | the same, then incrementing that field |

**Modifiers** say which fields an instruction reads and writes: `.A`, `.B`, `.AB` (A to B), `.BA`,
`.F` (both), `.X` (both, crossed), `.I` (the whole instruction). `MOV.I` copies a whole cell;
`ADD.AB #4, $3` adds 4 to the B-field of the cell 3 ahead.

## Processes

A warrior starts as one process. `SPL x` adds a new process at `x`; the warrior's processes take its
turns in order, so ten processes each run once every ten of the warrior's turns. A process that
executes `DAT` dies; the warrior dies with its last process.

## The three classic strategies

| Strategy | Idea | In this manual |
|---|---|---|
| **bomber** (stone) | drop `DAT`s across the core, fast | the dwarf, the stone |
| **scanner** | look for the enemy first, bomb where something is | the scanners, the quickscan |
| **paper** (replicator) | copy yourself around the core faster than you can be killed | the paper, Mice, the Silk |

Each is supposed to beat one of the others, like rock, paper and scissors; [chapter 3](03-cookbook.md#what-beats-what)
shows what the simple forms here actually do against each other.

## Hills and benchmarks

A **hill** is a running tournament: a new warrior fights every warrior on it and takes a place by
its score, 3 points a win and 1 a tie. Hills differ in their settings: `94b` (above), `94nop` (the
same without p-space, a small private memory a warrior keeps between rounds), `94x` (a bigger core),
`tiny` and `nano` (small ones). Koenigstuhl keeps archive hills with every published warrior.

A **benchmark** is a fixed set of opponents to measure a warrior against. This repository uses the
Wilkies benchmark (12 classic warriors) and the top 20 of Koenigstuhl's 94nop hill, and reports
the mean of (3 × wins + ties) × 100 / rounds: 300 beats everything, 100 ties everything.

## Where RED comes in

Writing redcode means choosing a modifier and a mode for every operand, keeping track of which field
of which cell holds each number, and counting cells to write every jump. RED lets you write
variables, loops and conditions; the compiler chooses the modifiers and modes the way pMARS would
for the same code written by hand, places every variable in a field, and tells you what the result
costs. Every classic warrior in [chapter 3](03-cookbook.md) compiles to the same code as its
hand-written version.
