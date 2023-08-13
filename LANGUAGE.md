# RED language

Instruction modifiers and addressing modes are automatically generated during compilation


## Arguments (arg)

### Variables

- Number

Immediate mode by default

- Text

Direct mode by default

### Argument modes

- (Dir var)

    Direct var

- (Ind var)

    Indirect var

- (Dec var)

    Predecrement var

- (Inc var)

    Postincrement var

- (store var)

    Variable var is stored here


## Conditions (cond)

### Unary conditions (cond1)

Generate an extra instruction in the code

- (JZ x) x is zero
- (JN x) x is not zero
- (DN x) decrement x and x is not zero

### Binary conditions (cond2)

Generate two extra instructions in the code

- (EQ x y) x and y equals
- (NE x y) x and y not equals
- (GT x y) x is greater than y
- (LT x y) x is less than y


## Instructions

### redcode instructions

All redcode instructions are available for direct use

- (DAT arg1 arg2)
- (MOV arg1 arg2)

- (ADD arg1 arg2)
- (SUB arg1 arg2)
- (MUL arg1 arg2)
- (DIV arg1 arg2)
- (MOD arg1 arg2)

- (JMP arg1 arg2)
- (SPL arg1 arg2)

- (NOP)

Not added yet

- (JMZ)
- (JMN)
- (DJN)

- (SEQ)
- (SNE)
- (SLT)

### Control flows

Some basic control flows were added
The extra instructions are indicated with EI

- (repeat body)

    EI: 1

- (while cond body)

    EI: 1 + cond

- (if cond body)

    EI: 0 + cond

- (do-while cond body)

    EI: 0 + cond

### Other instructions

- (let (id arg) body)

    Variable definition

- (seq instrs)

    Multiple instructions

- (label text)

    Set label


## Notices

- Bottom text
