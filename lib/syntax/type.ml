type t =
  | Bool
  | Arg
  | Instr
  | Arrow of t * t

let rec pp fmt = function
  | Bool -> Format.fprintf fmt "bool"
  | Arg -> Format.fprintf fmt "arg"
  | Instr -> Format.fprintf fmt "instr"
  | Arrow (a, b) -> Format.fprintf fmt "(%a -> %a)" pp a pp b

let ty_of_op1 : Prim.op1 -> t * t = function
  | Cjz -> (Arg, Bool)
  | Cjn -> (Arg, Bool)
  | Cdz -> (Arg, Bool)
  | Cdn -> (Arg, Bool)

  | Repeat -> (Instr, Instr)

let ty_of_op2 : Prim.op2 -> t * t * t = function
  | Dat -> (Arg, Arg, Instr)
  | Jmp -> (Arg, Arg, Instr)
  | Spl -> (Arg, Arg, Instr)
  | Nop -> (Arg, Arg, Instr)
  | Mov -> (Arg, Arg, Instr)
  | Add -> (Arg, Arg, Instr)
  | Sub -> (Arg, Arg, Instr)
  | Mul -> (Arg, Arg, Instr)
  | Div -> (Arg, Arg, Instr)
  | Mod -> (Arg, Arg, Instr)
  | Jmz -> (Arg, Arg, Instr)
  | Jmn -> (Arg, Arg, Instr)
  | Djn -> (Arg, Arg, Instr)
  | Seq -> (Arg, Arg, Instr)
  | Sne -> (Arg, Arg, Instr)
  | Slt -> (Arg, Arg, Instr)
  | Stp -> (Arg, Arg, Instr)
  | Ldp -> (Arg, Arg, Instr)

  | Ceq -> (Arg, Arg, Bool)
  | Cne -> (Arg, Arg, Bool)
  | Cgt -> (Arg, Arg, Bool)
  | Clt -> (Arg, Arg, Bool)

  | If -> (Bool, Instr, Instr)
  | While -> (Bool, Instr, Instr)
  | DoWhile -> (Bool, Instr, Instr)

let ty_of_op3 : Prim.op3 -> t * t * t * t = function
  | IfElse -> (Bool, Instr, Instr, Instr)
