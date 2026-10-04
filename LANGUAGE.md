# RED language

RED language is a programming language designed to write redcode programs. The language is designed to be easy to learn and use, and to be compiled to redcode programs. The following is a description of the language and its features.

### Language features

- Provide a simple syntax to write redcode programs with control flows
- Eliminate the need to write instruction modifiers and addressing modes

### Language syntax

The language syntax is based on s-expressions. The syntax is designed to be easy to read and write, and to be easy to parse and compile. The following is a description of the language components and their syntax.

### Notices

- The project is in development and RED language is not the final version. Important changes can be made until first release.
- This page is the syntax reference. What each construct means, formally, and what the compiler must preserve is in [docs/semantics.md](docs/semantics.md).
- Known compiler defects are listed in [docs/roadmap.md](docs/roadmap.md); the ones that change what a program on this page does are marked below.


## Arguments (arg)

Arguments are used in instructions and conditions to specify the operands.
If you want to store some value, you need to use a argument.

An argument have a addressing mode and a value.

- none: the atom `none`, replaced by immediate 0
- (var) declare a argument using default mode
- (mode var) declare a argument using specified mode

### Variables (var)

Variables are numbers or strings used to store values.
By default, numbers use immediate mode and strings use direct mode.

- (number) integer signed or unsigned number
- (string) string reference to let variable or label

If the string corresponds to some variable in scope, the string will be replaced by a reference to this variable, otherwise the string will be kept referencing a label.

All numbers are taken modulo the core size (8000 on the 94b hill) and stored as `0..7999`, so `-1` is `7999`.

### Constants and expressions

A constant is defined in the program header, `(const step 3044)`, and emitted as `step EQU 3044` before the code, so the warrior keeps its names and a constant optimizer can tune it. Its value is a number or an expression of earlier constants; it cannot name a label (EQU substitutes text, so a label would mean a different cell at every use). No `let` variable or label may take a constant's name.

An expression is `(op a b)` with `op` one of `+ - * / %` and each operand a number, a constant, a label, one of pMARS's predefined symbols (`CORESIZE`, …, numbers pMARS knows) or another expression; a name must be a valid label, and a division by zero the compiler can see is a compile error (pMARS rejects the warrior): `(+ imp 2667)`, `(* 2 step)`. It is emitted as is (`imp+2667`, `imp+(2*step)`) for pMARS to evaluate, and a label in it counts from the instruction that holds it, as in hand-written redcode. Without a mode, an expression is immediate when it names only constants and numbers, and direct when it names a label, as a number and a label are; `(Dir (+ imp 1))` or `(# step)` write the mode. A constant alone is a number: `(ADD step p)` is `ADD #step, ...`. A `let` variable cannot be part of an expression: it is a field of a cell, not a number known when the warrior is assembled.

### Addresing modes (mode)

Addresing modes are used to specify how the argument is used in the instruction. If the addressing mode is not specified, the default mode is used.

- (Imm var) | (# var) immediate addresing to var
- (Dir var) | ($ var) direct addresing to var
- (Ind var) | (@ var) indirect addresing to var
- (Dec var) | (< var) decrement var and indirect addresing to var
- (Inc var) | (> var) indirect addresing to var and increment var
- (AInd var) | (* var), (ADec var) | ({ var), (AInc var) | (} var) the same through the A-field, on a number, a label or an expression only: a let variable's pointer modes follow the field its `(store x)` gives it
- (store var) | (! var) store var value in this place (field is automatic)

Every `let` variable needs exactly one `(store x)` in its body: the cell holding it is labelled where the store is. Two stores are a compile error; a variable used with no store is a compile error at the use. Names (variables and labels) may not start with `_`: those are reserved for the labels the compiler generates.


## Conditions (cond)

Conditions are used in a control flow intruction to specify when the control flow is executed. There are two types of conditions: unary and binary, each one with different extra instructions added to the code to work.

### Unary conditions (cond1)

Unary conditions are used to specify when the control flow is executed based on one argument. Generate an extra instruction in the code.

- (JZ x) x is zero
- (JN x) x is not zero
- (DZ x) decrement x and x is zero (only in `if`, `if` with else, and `while`)
- (DN x) decrement x and x is not zero (only in `do-while`)

- (JZ mod x) | (JN mod x) | (DZ mod x) | (DN mod x) the same, with the test's modifier given: `(JZ F x)` is zero in both fields

A unary condition tests the field its variable is stored in (`.A` or `.B`); a plain number or label is tested on its B-field.

### Binary conditions (cond2)

Binary conditions are used to specify when the control flow is executed based on two arguments. Generate two extra instructions in the code.

- (EQ x y) x and y equals
- (NE x y) x and y not equals
- (GT x y) x is greater than y
- (LT x y) x is less than y

- (EQ mod x y) | (NE mod x y) | (GT mod x y) | (LT mod x y) the same, with the comparison's modifier given; `AB` reads `x`'s A-field and `y`'s B-field in every comparison (`GT` is emitted as `SLT y, x`, so the compiler writes `.BA` there)

Comparisons are unsigned: values are compared as `0..7999`, so `(LT -1 3)` is false.

A modifier is written as for instructions (`A`, `B`, `AB`, `BA`, `F`, `X`, `I`), right after the operator. Without one, two pointers compare their two target cells whole: `(NE (Ind a) (Ind b))` is `SNE.I`, as a SEQ scanner needs.

In `do-while`, `GT` and `LT` add a third instruction (`SNE #0, #1`, which always skips) so the comparison stays strict; the loop still spends two control instructions per iteration (`SLT` and the jump back), as before.


## Instructions

Instructions are used to specify the operation to be performed. Instruction modifiers are automatically generated during compilation.

### instructions modifiers (mod)

All instruction modifiers are automatically generated, but there is an option to select one manually.
A useful case where to declare them explicitly is when you want to target the entire instruction or both fields.

- A from the A-field to the A-field
- B from the B-field to the B-field
- AB from the A-field to the B-field
- BA from the B-field to the A-field
- F both fields to the same fields
- X both fields to the opposite fields
- I the whole instruction

A plain reference (a label, `(Dir -1)`) or a pointer's target `(Ind p)` names a cell. Beside a number or a variable, a cell is its B-field: with `x` in an A-field, `(MOV x (Dir -1))` is `MOV.AB` (x's value into the cell's B-field), and `(ADD 1 (Ind p))` is `ADD.AB` whichever field `p` lives in. A bomber that copies a whole cell still writes `(MOV I bomb (Ind p))`: a bare variable is its value. When nothing tells a field apart (two cells, or two numbers), the instruction takes the ICWS'94 default, exactly what pMARS gives the same instruction written by hand: `MOV`/`SEQ`/`SNE` `.AB` with an immediate A, `.B` with only an immediate B, `.I` otherwise; arithmetic the same with `.F` instead of `.I`; `SLT`/`LDP`/`STP` `.AB` with an immediate A, `.B` otherwise; jumps `.B`; `DAT` and `NOP` `.F` (pMARS; the ICWS'94 draft's table, section A.2.1.2, gives `NOP` `.B`). So `(ADD 1 1)` is `ADD.AB #1, #1`.

### redcode instructions

All redcode instructions are available for direct use.
Square brackets '[]' indicates that the argument is optional.

#### Misc instructions

- (DAT [arg1] [arg2]) data values
- (NOP [arg1] [arg2]) no operation
- (JMP arg1 [arg2]) jump to arg1
- (SPL arg1 [arg2]) split to arg1

The arguments these four do not use still are evaluated: a `<` or `>` in them moves its pointer each time the instruction runs (ICWS'94 evaluates both operands before the opcode).

#### Arithmetic instructions

- (MOV [mod] arg1 arg2) move arg1 to arg2
- (ADD [mod] arg1 arg2) add arg1 to arg2
- (SUB [mod] arg1 arg2) sub arg1 from arg2
- (MUL [mod] arg1 arg2) mul arg1 to arg2
- (DIV [mod] arg1 arg2) div arg1 from arg2
- (MOD [mod] arg1 arg2) mod arg1 from arg2

#### P-space instructions

- (STP [mod] arg1 arg2) store arg1 into P-space cell arg2
- (LDP [mod] arg1 arg2) load P-space cell arg1 into arg2

P-space does not exist on hills such as 94nop.

#### Conditional instructions

- (JMZ [mod] arg1 arg2) jump to arg1 if arg2 is zero
- (JMN [mod] arg1 arg2) jump to arg1 if arg2 is not zero
- (DJN [mod] arg1 arg2) decrement arg2 and jump to arg1 if arg2 is not zero
- (SEQ [mod] arg1 arg2) skip next instruction if arg1 equals arg2
- (SNE [mod] arg1 arg2) skip next instruction if arg1 not equals arg2
- (SLT [mod] arg1 arg2) skip next instruction if arg1 is less than arg2


### Control flows

Control flows are used to specify the execution order of the instructions. Use control flows generates extra instructions in the code.

- (repeat body) repeat body forever (one extra instruction)
- (repeat body arg) the same, with `arg` as the B operand of that extra `JMP`, which does not jump through it but does evaluate it every iteration (so `(Inc p)` there increments `p` each time): `(repeat body (store p))` keeps the variable `p` there, in a cell the loop needs anyway, instead of a `DAT` of its own (one cell less)
- (if cond then) execute body if cond is true (no extra instruction + cond)

- (while cond body) repeat body while cond is true (one extra instruction + cond). With a `JZ` or `JN` condition and the default policy the test goes after the body and one `JMP` enters it: per iteration only the test runs (one control instruction instead of two), at one more cycle before the first iteration; a policy that puts `boot` first keeps the test at the top
- (do-while cond body) repeat body while cond is true (no extra instruction + cond)

- (if cond then else) execute then if cond is true, otherwise execute else (one extra instruction + cond)

With the default policy, a jump the compiler generated that would land on the very next cell (an empty `else`, an empty `if`, an empty rotated `while`) is left out, unless it also does something: a `DJN` decrements, a `<`/`>` operand moves a pointer, a cell that holds a variable or carries your label stays, and so does a jump right after a `SEQ`/`SNE`/`SLT`, which would otherwise skip a different cell, or one that a number you wrote counts across (`(JMP (Dir 2))` over it). A number that points into the cells of a `while` or an `if` depends on their layout, which the policy chooses: use labels.

A jump the compiler generated that would land on a `JMP` the compiler generated goes straight to that `JMP`'s target: an `if` at the end of a `repeat` jumps back to the loop's head when its condition is false, one cycle sooner. It changes no cell count, never touches a jump you wrote, and never passes a `JMP` that carries one of your labels or that only jumps reach (it would be left dead).


### Other instructions

- (let (id arg) body) introduce a new variable in the scope (no extra instruction)

- (seq instrs) execute a sequence of instructions (no extra instruction)
- (label text) create a label in the code (no extra instruction). Labels are case-sensitive, `[A-Za-z][A-Za-z0-9_]*`: a name starting with `_` is reserved for the labels the compiler generates (`_LET1`, `_WHI9`, …), and a pMARS keyword (`MOV`, `END`, …) or one of pMARS's predefined symbols (`CORESIZE`, `MAXLENGTH`, `CURLINE`, …, case-sensitive) cannot be a label, and a label used or defined must have that form; all are compile errors. A label so long that an emitted line reaches 256 characters is a compile error (pMARS hangs on such lines)
- (com words ...) a comment line in the output, `; words ...` (no instruction)

Every compiled program ends with an extra `DAT $0, $0`, exactly what pMARS fills empty core with, so no scanner can tell it from an empty cell: a label at the end of a program needs an instruction after it (pMARS discards a trailing label), and a program that runs past its last instruction dies there. It costs one cell of length.


## Program header, optimization and expectations

A file may wrap its single body expression in an optional header. A file without it is unchanged.

```
(program
  (optimize speed size)
  (expect (length <= 8))
  body)
```

- (program items) the header: any number of `optimize`, `expect`, `const` and `strategy` items, at most one `hill`, `name` and `author`, and exactly one body expression
- (const name value) a constant (see *Constants and expressions*)
- (start label) execution starts at `label` (emitted as `ORG label`) instead of the first cell, so data can come before the code; the metrics measure from there, and an unknown label is an error
- (hill key) the hill the warrior is written for: `94b` (the default), `94nop`, `94`, `94x`, `tiny`, `nano`. It sets the `;redcode-<key>` line, adds `;assert CORESIZE==… && MAXLENGTH==…`, measures on that core and length, rejects a warrior longer than the hill allows, and rejects `LDP`/`STP` on `94nop` (no p-space). `run_compile.exe --hill KEY` overrides it.
- (name words ...), (author words ...), (strategy words ...) emitted as `;name`, `;author`, `;strategy` lines, only when written (the compiler never invents an author); `strategy` may repeat. Each is one line of words: an empty one, or one with a line break, is an error
- (optimize objectives) at least one objective, in the order in which the compiler weighs its metrics: `speed` (cycles per loop iteration), `size` (warrior length), `stealth` (cells a scanner can see), `boot` (cycles before the first loop). The default is `speed size`; `run_compile.exe --optimize size,speed` overrides the header. The compiler compiles each combination of its optional transformations, measures them and keeps the one the policy prefers.

### Expectations (expect)

`(expect e)` states what the compiled warrior must do. It emits no code and does not change the labels the compiler generates. In the header it applies to the whole warrior; as a statement inside a `repeat`, `while` or `do-while` body it applies to that loop.

Checked when compiling (a failure stops the compilation, or is a warning with `--expect=warn`):

- (length <= N) the warrior is at most N cells, the final `DAT` included
- (cycles N) | (cycles <= N) instructions executed per iteration of the loop
- (overhead <= N) of those, the control instructions the compiler added
- (boot <= N) cycles before the first loop starts
- (step K) a pointer in the loop advances K cells per iteration
- (covers-core) that pointer visits every cell of the core

`length`, `cycles`, `overhead` and `boot` accept both `(m N)` (exactly N) and `(m <= N)`.

Checked by running the warrior in pMARS (`run_compile.exe --emit-beh FILE.beh` writes them as a behaviour spec for `tools/behave.py`, and the redcode beside it as `FILE.red`, the spec naming the warrior's hill (`hill: tiny`) so it runs under that hill's settings; it refuses a program with none of these). N is at least 1:

- (alive N) a process is still running after N executed instructions
- (dead N) no process is left after N executed instructions
- (cell ADDR "TEXT" N) after N instructions, cell ADDR holds the instruction TEXT

### Templates and repetition

A template is a named RED fragment with typed parameters, defined in the header and expanded where it is called, before anything else is compiled (d-7d2612-c4e274):

```
(program
  (const step 400)
  (define (probe (k Num) (hit Lab))
    (if (NE I (Dir (+ start (* k step))) (Dir (+ start (* k step) 4))) (JMP hit)))
  (seq (label start) (for k 1 8 (probe k found)) ...))
```

- (define (name (param Kind) ...) body) a template; its body is one expression. A parameter's kind is `Num` (a number, constant, label or expression), `Lab` (a label), `Var` (a `let` variable in scope at the call) or `Code` (a RED expression, standing as a statement).
- (name args ...) a call, where an expression may stand; the arguments must match the kinds.
- (for k lo hi body) `body` once for each integer `k` from `lo` to `hi` (constants or numbers known when compiling, at most 1000 times), in order.

A label or `let` a template defines is its own at each expansion (renamed `_X<n>_name`), so two calls never clash; a name passed as an argument is used as is, never captured. A template calls only templates defined before it, so expansion always ends. A template costs nothing: it emits what its body emits, once per call.

### Warnings

The compiler warns, on standard error as `file:line:column: warning: ...`, where the compiled warrior pays a cost that could be removed, and says what would remove it. Which ones depend on the policy (`(optimize ...)`):

- with `speed` in the policy: a loop that spends more control instructions per iteration than its construct needs, when a faster variant exists and the policy declined it for an objective ranked higher (a `while` kept with its test at the top under `boot speed`);
- with `size` or `stealth`: cells never executed that hold no data (dead code): no variable, no label on a `DAT`, and no executed instruction reads or writes them; not said when a jump the compiler cannot follow might reach them;
- always: a pointer whose step leaves cells unvisited (a step of 4 visits 2000 of 8000), unless its loop states the step with `(expect (step k))` or `(expect (covers-core))`.

`run_compile.exe --warn=all` gives every warning whatever the policy, `--warn=none` none. Warnings never stop the compilation.

`run_compile.exe --report` prints the measured metrics and predictions on standard error, with the optimizations the policy chose (`optimizations: rotate-unary`, or `none`; in JSON, `"optimizations":[...]`); `--report=json` prints them as JSON instead of the redcode. Any compile error prints `file:line:column: error: ...` on standard error and exits with code 1; an internal compiler error exits with code 2. Definitions: [docs/specs/2026-10-03-cost-model-design.md](docs/specs/2026-10-03-cost-model-design.md).
