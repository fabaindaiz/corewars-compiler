# 1. Getting started

## Install

The compiler is an OCaml program built with dune. With [opam](https://opam.ocaml.org/doc/Install.html)
installed, a switch local to the repository keeps everything under `_opam/`:

```sh
opam init --bare -n
opam switch create . ocaml-base-compiler.5.5.1 --no-install
opam install --switch=. dune containers alcotest
opam exec --switch=. -- dune build
```

The test suite also needs `bbctester` (see [`REFERENCE.md`](../../REFERENCE.md)); compiling
warriors does not. Every command below runs from the repository's root; prefix `dune` with
`opam exec --switch=. --` if the switch is not active in your shell.

To run warriors you need pMARS. `pmars/pmars` is a Linux x86-64 binary; on any other machine build
one from the source in the repository (it needs a C compiler and `unzip`):

```sh
tools/pmars-host.sh          # prints _build/pmars-host/pmars
```

## A first warrior

The smallest warrior is the imp: one instruction that copies itself into the next cell, then runs
the copy. Write it in a file, `imp.src`:

```red
(MOV (Dir 0) (Dir 1))
```

and compile it:

```sh
dune exec execs/run_compile.exe -- imp.src > imp.red
```

```redcode
;redcode-94b

  MOV.I  $0     , $1
  DAT    $0     , $0
```

What happened:

- `(MOV a b)` is the redcode instruction `MOV`. Every redcode opcode is available under its own name.
- `(Dir 0)` is the operand `$0`, this cell; `(Dir 1)` is the next one. RED writes addressing modes
  as words (`Dir`, `Ind`, `Inc`, ...) or as their symbols (`$`, `@`, `>`, ...).
- You wrote no modifier: the compiler chose `.I`, the one pMARS gives the same line written by
  hand, because two cells are copied whole.
- Opcodes are written in capitals, as above: `(mov ...)` is not recognised.
- A bare number is a value, not an address: `(MOV 0 1)` compiles to `MOV.AB #0, #1`, which copies
  nothing anywhere. Write `(Dir 1)`, or a label, for a cell.
- `;redcode-94b` names the hill the warrior is written for, the default.
- The last `DAT $0, $0` is the compiler's: every warrior ends with what empty core holds.

## Run it

pMARS's debugger, cdb, executes a warrior step by step. Load the imp alone, execute a few
instructions and list the first cells:

```sh
printf 'skip 2\nlist 0,4\nquit\n' | _build/pmars-host/pmars -@ pmars/config/94b.opt -e -b imp.red
```

`skip 2` executes three instructions; `list 0,4` shows the imp has copied itself into cells 1, 2
and 3. `-@ pmars/config/94b.opt` loads the 94b hill's settings (core 8000, 100 cells a warrior).
cdb exits with code 4 after `quit`, so do not chain it with `&&`.

pMARS also prints `Warning: Missing ';assert'` for a warrior that names no hill: harmless here.
Naming the hill in the program, `(program (hill 94b) ...)` ([chapter 2](02-tutorial.md)),
emits the `;assert` line and the warning goes away.

## Fight

A battle is two warriors in one core. Compile a second warrior, the dwarf from the
[cookbook](03-cookbook.md#a-bomber-the-dwarf), and let them fight 200 rounds:

```sh
_build/pmars-host/pmars -@ pmars/config/94b.opt -b -k -r 200 -F 4000 imp.red dwarf.red
```

```text
0 149
51 149
```

`-k` prints one line per warrior, in the order of the files: its wins, then the ties. The imp won
no round, the dwarf 51, and 149 were ties. `-F 4000` places the second warrior at cell 4000 in the
first round and makes the whole battle repeat exactly; without it, pMARS picks positions at random
and the numbers change from run to run. Without `-k`, pMARS prints each warrior's score, 3 points a
win and 1 a tie, then the wins of each warrior and the ties:

```text
Unknown by Anonymous scores 149
Unknown by Anonymous scores 302
Results: 0 51 149
```

`Unknown by Anonymous` is a warrior with no name: `(name ...)` and `(author ...)` in the header give
it one ([chapter 2](02-tutorial.md)).

A dwarf bombs every fourth cell, and an imp is never where the bombs fall for long: the battle
mostly ties.

## Where to go next

- [The tutorial](02-tutorial.md) builds a warrior that uses variables, loops and conditions.
- [The cookbook](03-cookbook.md) has the classic strategies ready to copy.
- `dune exec execs/run_compile.exe -- --report imp.src` tells you what the warrior costs:
  [chapter 4](04-tools.md).
