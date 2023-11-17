type e =
  | Value of value
  | Label of string
  | Lam of string * Type.t * e
  | App of e * e
  | Prim1 of Prim.op1 * e
  | Prim2 of Prim.op2 * e * e
  | Prim3 of Prim.op3 * e * e * e
  | Let of binding * e
  | Tuple of e list
  | Ascribe of e * Type.t

and value =
  | Arg of arg
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

and binding = { name : string; e : e; ty : Type.t }

[@@deriving show { with_path = false }]
