type op1 =
  | Cjz
  | Cjn
  | Cdz
  | Cdn

  | Repeat

and op2 =
  | Dat
  | Jmp
  | Spl
  | Nop
  | Mov
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Jmz
  | Jmn
  | Djn
  | Seq
  | Sne
  | Slt
  | Stp
  | Ldp

  | Ceq
  | Cne
  | Cgt
  | Clt
  
  | If
  | While
  | DoWhile

and op3 =
  | IfElse

and imod =
  | Def
  | N
  | A
  | B
  | AB
  | BA
  | F
  | X
  | I

[@@deriving show { with_path = false }]
