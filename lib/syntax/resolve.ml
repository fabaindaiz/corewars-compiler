type e =
  | Value of value
  | Arg of arg
  | Label of string
  | Lam of string * e
  | App of e * e
  | Prim1 of Prim.op1 * e
  | Prim2 of Prim.op2 * e * e
  | Prim3 of Prim.op3 * e * e * e
  | Let of string * e * e
  | Tuple of e list

and value =
  | Var of string

and arg =
  | None
  | Store of string
  | Num of int
  | Id of string
  | Ref of mode * int
  | Lab of mode * string

and mode =
  | MImm
  | MDir
  | MInd
  | MIndInc
  | MIndDec

and place =
  | PNone
  | PA
  | PB

[@@deriving show { with_path = false }]