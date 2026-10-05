# The RED manual

RED is a small s-expression language for writing Core War warriors. Its compiler turns a RED
program into ICWS'94 redcode for pMARS and the KotH hills, chooses the modifiers and addressing
modes you would otherwise write by hand, measures what it emits, and tells you what a warrior costs.

This manual is for people who want to write warriors with it. It is in four chapters:

| Chapter | What it gives you |
|---|---|
| [1. Getting started](01-getting-started.md) | install the compiler, compile a first warrior, run it in pMARS |
| [2. Tutorial](02-tutorial.md) | build a warrior step by step: variables, loops, conditions, the header, templates, tests |
| [3. Cookbook](03-cookbook.md) | the classic strategies written in RED: bombers, scanners, papers, clears, imps, quickscans |
| [4. Tools](04-tools.md) | the compiler's options, its report, warnings and expectations, behaviour specs, the benchmark |

The syntax reference is [`LANGUAGE.md`](../../LANGUAGE.md); what each construct means, formally, is
[`docs/semantics.md`](../semantics.md). The catalogue of reusable templates is
[`snippets/README.md`](../../snippets/README.md).

## How to read the examples

A block marked `red` is a RED program; the `redcode` block after it is exactly what the compiler
prints for it. Both are checked: the test suite compiles every `red` block in this manual and
compares the result with the `redcode` block that follows (`test_manual_examples`), so an example
here is never out of date with the compiler.

The compiled code names its own labels: `_LET1` is the cell of a `let` variable, `_REP4` the head of
a `repeat`, `_IF7` the end of an `if`, and so on. The number is the node of the program that made
it; a name starting with `_` is never yours. Every warrior ends with a `DAT $0, $0`, which is what
empty core holds: a label needs an instruction after it, and the end of a warrior is invisible.
