open Common

type t =
  | Bool
  | Arg
  | Instr
  | Product of t list

let rec pp fmt = function
  | Bool -> Format.fprintf fmt "bool"
  | Arg -> Format.fprintf fmt "arg"
  | Instr -> Format.fprintf fmt "instr"
  | Product l -> Format.fprintf fmt "@[<4>(%a)@]" (Pp.pp_list " * " pp) l

let ty_of_op1 : Prim.op1 -> t * t = function
  | Cjz | Cjn | Cdz | Cdn -> (Arg, Bool)
  | Repeat -> (Instr, Instr)

let ty_of_op2 : Prim.op2 -> t * t * t = function
  | Dat | Jmp | Spl | Nop | Mov | Add | Sub | Mul | Div | Mod
  | Jmz | Jmn | Djn | Seq | Sne | Slt | Stp | Ldp -> (Arg, Arg, Instr)
  | Ceq | Cne | Cgt | Clt -> (Arg, Arg, Bool)
  | If | While | DoWhile -> (Bool, Instr, Instr)

let ty_of_op3 : Prim.op3 -> t * t * t * t = function
  | IfElse -> (Bool, Instr, Instr, Instr)
