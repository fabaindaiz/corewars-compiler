# RED language

RED language is a programming language designed to write redcode programs. The language is designed to be easy to learn and use, and to be compiled to redcode programs. The following is a description of the language and its features.

### Language features

- Provide a simple syntax to write redcode programs with control flows
- Eliminate the need to write instruction modifiers and addressing modes

### Language syntax

The language syntax is based on s-expressions. The syntax is designed to be easy to read and write, and to be easy to parse and compile. The following is a description of the language components and their syntax.

### Notices

- The project is in development and RED language is not the final version. Important changes can be made until first release.


## Arguments (arg)

Arguments are used in instructions and conditions to specify the operands.
If you want to store some value, you need to use a argument.

An argument have a addressing mode and a value.

- (mode var) mode can be specified (mode is optional)

### Variables (var)

Variables are numbers or texts used to store values.

- Number Immediate mode by default
- Text Direct mode by default

### Addresing modes (mode)

Addresing modes are used to specify how the argument is used in the instruction. If the addressing mode is not specified, the default mode is used.

- (Dir var) direct addresing to var
- (Ind var) indirect addresing to var
- (Dec var) decrement var and indirect addresing to var
- (Inc var) indirect addresing to var and increment var
- (store var) store var value in this place (field is automatic)


## Conditions (cond)

Conditions are used in a control flow intruction to specify when the control flow is executed. There are two types of conditions: unary and binary, each one with different extra instructions added to the code to work.

### Unary conditions (cond1)

Unary conditions are used to specify when the control flow is executed based on one argument. Generate an extra instruction in the code.

- (JZ x) x is zero
- (JN x) x is not zero
- (DN x) decrement x and x is not zero

### Binary conditions (cond2)

Binary conditions are used to specify when the control flow is executed based on two arguments. Generate two extra instructions in the code.

- (EQ x y) x and y equals
- (NE x y) x and y not equals
- (GT x y) x is greater than y
- (LT x y) x is less than y


## Instructions

Instructions are used to specify the operation to be performed. Instruction modifiers are automatically generated during compilation.

### redcode instructions

All redcode instructions are available for direct use

- (DAT arg1 arg2) data values
- (MOV arg1 arg2) move arg1 to arg2

- (ADD arg1 arg2) add arg1 to arg2
- (SUB arg1 arg2) sub arg1 from arg2
- (MUL arg1 arg2) mul arg1 to arg2
- (DIV arg1 arg2) div arg1 from arg2
- (MOD arg1 arg2) mod arg1 from arg2

- (JMP arg1 [arg2]) jump to arg1 (arg2 is optional)
- (SPL arg1 [arg2]) split to arg1 (arg2 is optional)

- (NOP) no operation

Not added yet

- (JMZ arg1 arg2) jump to arg1 if arg2 is zero
- (JMN arg1 arg2) jump to arg1 if arg2 is not zero
- (DJN arg1 arg2) decrement arg2 and jump to arg1 if arg2 is not zero

- (SEQ arg1 arg2) skip next instruction if arg1 equals arg2
- (SNE arg1 arg2) skip next instruction if arg1 not equals arg2
- (SLT arg1 arg2) skip next instruction if arg1 is less than arg2

### Control flows

Control flows are used to specify the execution order of the instructions. Use control flows generates extra instructions in the code.

- (repeat body) repeat body forever (one extra instruction)
- (while cond body) repeat body while cond is true (one extra instruction + cond)
- (if cond body) execute body if cond is true (no extra instruction + cond)
- (do-while cond body) repeat body while cond is true (no extra instruction + cond)


### Other instructions

- (let (id arg) body) introduce a new variable in the scope (no extra instruction)

- (seq instrs) execure a sequence of instructions (one extra instruction)
- (label text) create a label in the code (no extra instruction)
