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

## Fight

A battle is two warriors in one core. Compile a second warrior, the dwarf from the
[cookbook](03-cookbook.md#a-bomber-the-dwarf), and let them fight 200 rounds:

```sh
_build/pmars-host/pmars -@ pmars/config/94b.opt -b -k -r 200 imp.red dwarf.red
```

`-k` prints the result as wins and ties for each warrior. A dwarf bombs every fourth cell, and an
imp is never where the bombs fall for long: the battle mostly ties (152 of 200 rounds here; the
dwarf wins the other 48).

## Where to go next

- [The tutorial](02-tutorial.md) builds a warrior that uses variables, loops and conditions.
- [The cookbook](03-cookbook.md) has the classic strategies ready to copy.
- `dune exec execs/run_compile.exe -- --report imp.src` tells you what the warrior costs:
  [chapter 4](04-tools.md).
