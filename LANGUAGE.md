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

### Addresing modes (mode)

Addresing modes are used to specify how the argument is used in the instruction. If the addressing mode is not specified, the default mode is used.

- (Imm var) | (# var) immediate addresing to var
- (Dir var) | ($ var) direct addresing to var
- (Ind var) | (@ var) indirect addresing to var
- (Dec var) | (< var) decrement var and indirect addresing to var
- (Inc var) | (> var) indirect addresing to var and increment var
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

A unary condition tests the field its variable is stored in (`.A` or `.B`); a plain number or label is tested on its B-field.

### Binary conditions (cond2)

Binary conditions are used to specify when the control flow is executed based on two arguments. Generate two extra instructions in the code.

- (EQ x y) x and y equals
- (NE x y) x and y not equals
- (GT x y) x is greater than y
- (LT x y) x is less than y

Comparisons are unsigned: values are compared as `0..7999`, so `(LT -1 3)` is false.

In `do-while`, `GT` and `LT` add a third instruction (`SNE #0, #1`, which always skips) so the comparison stays strict; the loop still costs two instructions per iteration.


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

When the compiler cannot infer a modifier from the variables involved (for example, two plain references), it uses `.I`, which differs from the ICWS'94 default for `SLT`, `JMZ`, `JMN`, `DJN` and arithmetic; whether to keep this is undecided (i-7d2612-96f7b1).

### redcode instructions

All redcode instructions are available for direct use.
Square brackets '[]' indicates that the argument is optional.

#### Misc instructions

- (DAT [arg1] [arg2]) data values (arg1 & arg2 only store data)
- (NOP [arg1] [arg2]) no operation (arg1 & arg2 only store data)
- (JMP arg1 [arg2]) jump to arg1 (arg2 only store data)
- (SPL arg1 [arg2]) split to arg1 (arg2 only store data)

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
- (if cond then) execute body if cond is true (no extra instruction + cond)

- (while cond body) repeat body while cond is true (one extra instruction + cond)
- (do-while cond body) repeat body while cond is true (no extra instruction + cond)

- (if cond then else) execute then if cond is true, otherwise execute else (one extra instruction + cond)


### Other instructions

- (let (id arg) body) introduce a new variable in the scope (no extra instruction)

- (seq instrs) execute a sequence of instructions (no extra instruction)
- (label text) create a label in the code (no extra instruction). Labels are case-sensitive, `[A-Za-z_][A-Za-z0-9_]*`; do not use names the compiler generates (`LET`, `REP`, `IF`, `IFM`, `IFF`, `WHI`, `WHF`, `DWH` followed by a number) or pMARS keywords (i-7d2612-425c66). A label so long that an emitted line reaches 256 characters is a compile error (pMARS hangs on such lines)
- (com words ...) a comment line in the output, `; words ...` (no instruction)

Every compiled program ends with an extra `DAT 0, 0`: a program that runs past its last instruction dies there.


## Program header, optimization and expectations

A file may wrap its single body expression in an optional header. A file without it is unchanged.

```
(program
  (optimize speed size)
  (expect (length <= 8))
  body)
```

- (program items) the header: any number of `optimize` and `expect` items, and exactly one body expression
- (optimize objectives) at least one objective, in the order in which the compiler weighs its metrics: `speed` (cycles per loop iteration), `size` (warrior length), `stealth` (cells a scanner can see), `boot` (cycles before the first loop). The default is `speed size`; `run_compile.exe --optimize size,speed` overrides the header. The compiler measures and reports today; it does not yet change its output by policy.

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

Checked by running the warrior in pMARS (`run_compile.exe --emit-beh FILE.beh` writes them as a behaviour spec for `tools/behave.py`, and the redcode beside it as `FILE.red`; it refuses a program with none of these). N is at least 1:

- (alive N) a process is still running after N executed instructions
- (dead N) no process is left after N executed instructions
- (cell ADDR "TEXT" N) after N instructions, cell ADDR holds the instruction TEXT

`run_compile.exe --report` prints the measured metrics and predictions on standard error; `--report=json` prints them as JSON instead of the redcode. Any compile error prints `file:line:column: error: ...` on standard error and exits with code 1; an internal compiler error exits with code 2. Definitions: [docs/specs/2026-10-03-cost-model-design.md](docs/specs/2026-10-03-cost-model-design.md).
