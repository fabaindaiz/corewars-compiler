---
name: run-warrior
description: Run a compiled RED program or any redcode warrior in pMARS and observe its execution step by step - trace the program counter, read core cells, count instructions until it dies, list processes, or score it against other warriors. Use when asked to "run", "execute", "trace", "step", "debug" or "see what the warrior does", when checking that compiled code behaves as RED means, or when writing a behaviour spec in behtests/.
allowed-tools: Bash, Read, Write
---

# Run a warrior and watch it

## Get a pMARS that runs here

```sh
tools/pmars-host.sh          # prints the path: _build/pmars-host/pmars (built once from pmars/pmars-0.9.4.zip)
```

`pmars/pmars` is a Linux x86-64 binary (d-7d2612-3d04ba): on macOS it fails with exit 126. The host
build keeps the cdb debugger. Always pass the test settings: `-@ pmars/config/94b.opt`.

## Get the redcode

- From RED: `dune exec execs/run_compile.exe examples/prog8.src > _build/prog8.red` (needs the opam
  switch; `make compile` would also echo the command into the file).
- Without the switch: the EXPECTED section of a golden is the compiler's output
  (`tools/behave.py` extracts it the same way); write it to a file under `_build/`.

## Step it in cdb (deterministic: one warrior always loads at address 0)

```sh
P=_build/pmars-host/pmars
printf 'skip 272\nlist 1\ncalc CYCLE\npqueue\nquit\n' | $P -@ pmars/config/94b.opt -e -b _build/prog8.red
```

| Command | Effect |
|---|---|
| `skip K` | execute **K+1** instructions, then print the next one |
| `step` | execute one instruction, print the next |
| `list A` / `list A,B` | disassemble cells A..B (addresses relative to load address 0); a cell equal to empty core, `DAT.F $0, $0`, prints as its address alone |
| `calc CYCLE` | cycles left (80000 minus instructions executed, with one warrior and any number of processes) |
| `registers` | cycle, processes active, process queue |
| `pqueue` | switch `list` to the process queue: `pqueue` then `list 0,N` lists processes, `pqueue off` returns (`registers` also shows the queue) |
| `write F` … `write` | copy everything printed in between to file F (a trace) |
| `quit` | stop (exit 4); end of input instead lets the battle finish (exit 0) |

**Alive or dead after N instructions:** send `skip N-1` then `calc CYCLE`. A number is printed only
if a process is left. That is exactly how `tools/behave.py` implements `alive N` / `dead N`.

## Gotchas (each measured)

- `list` pages every 23 lines ("RET for more") and eats the commands after it: list short ranges.
- When the warrior dies, cdb exits at once and prints the score line; later commands are ignored.
- Battle mode with one warrior cannot tell dead from alive: the score formula gives 0 either way. To use battles,
  load a `JMP 0` "duck" as warrior 2 and use `-F 4000` (warrior 2 at 4000 in round 1, deterministic positions after;
  `-f` reseeds whenever the code changes).
- pMARS warns "Missing ';assert'" for a compiled warrior that names no hill: harmless; `(hill ...)` emits one (d-7d2612-56cfae).
- A label redefinition is only a warning ("Ignored, redefinition of label") and the first one wins.

## Score against other warriors

```sh
$P -@ pmars/config/94b.opt -b -k -r 200 -F 4000 _build/w.red other.red   # KotH format: wins ties
```

## Predict before you run

`dune exec execs/run_compile.exe -- --report prog.src` prints, without running anything, the
cycles per loop iteration, the boot, and predictions (a pointer's step and coverage, a `DJN`
counter's total and the instruction at which the warrior dies). Check a prediction with a probe:
the prog7 counter predicts 202, and `behtests/prog7_dowhile_dn.beh` measures `dead 202`.

## Turn an observation into a spec

Write `behtests/<name>.beh` (copy `behtests/prog8_while_lt.beh`): `golden:`, then `alive N`,
`dead N` or `cell N ADDR TEXT` lines. Or write `(expect (alive N))`, `(expect (dead N))`,
`(expect (cell ADDR "TEXT" N))` in the RED source and export them with
`run_compile.exe --emit-beh _build/<name>.beh <file>`, which writes a spec with `redcode:`. Run `python3 tools/behave.py behtests/<name>.beh` and make
sure it can fail: change one number and watch it go red, then put it back.
