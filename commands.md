### Interactive execution
```bash
dune utop
```

### red.ml
```bash
open Dev.Red;
pp_instructions (Instr("", MOV(I, Const(0), Const(1))));;
pp_instructions (Instr("", MOV(I, Ref(Imm, 0), Ref(Imm, 1))));;
```