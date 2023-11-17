type e =
  | Value of value
  | Label of string
  | Lam of string * e
  | App of e * e
  | Prim1 of Prim.op1 * e
  | Prim2 of Prim.op2 * e * e
  | Prim3 of Prim.op3 * e * e * e
  | Let of string * e * e
  | Tuple of e list

and value =
  | Arg of place * arg
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


let of_mode (m : Typed_ast.mode) : mode =
  match m with
  | Typed_ast.MImm -> MImm
  | Typed_ast.MDir -> MDir
  | Typed_ast.MInd -> MInd
  | Typed_ast.MIndInc -> MIndInc
  | Typed_ast.MIndDec -> MIndDec

let of_arg (a : Typed_ast.arg) : arg =
  match a with
  | Typed_ast.None -> None
  | Typed_ast.Store x -> Store x
  | Typed_ast.Num n -> Num n
  | Typed_ast.Id x -> Id x
  | Typed_ast.Ref (m, n) -> Ref (of_mode m, n)
  | Typed_ast.Lab (m, l) -> Lab (of_mode m, l)

let of_value (v : Typed_ast.value) : value =
  match v with
  | Typed_ast.Arg a -> Arg (PNone, of_arg a)
  | Typed_ast.Var x -> Var x

let rec of_typed_ast (e : Typed_ast.e) : e =
  match e with
  | Value v -> Value (of_value v)
  | Label l -> Label l
  | Lam (x, _, e) -> Lam (x, of_typed_ast e)
  | App (e1, e2) -> App (of_typed_ast e1, of_typed_ast e2)
  | Prim1 (op1, e) -> Prim1 (op1, of_typed_ast e)
  | Prim2 (op2, e1, e2) -> Prim2 (op2, of_typed_ast e1, of_typed_ast e2)
  | Prim3 (op3, e1, e2, e3) -> Prim3 (op3, of_typed_ast e1, of_typed_ast e2, of_typed_ast e3)
  | Let (b, e') -> Let (b.name, of_typed_ast b.e, of_typed_ast e')
  | Tuple exprs -> Tuple (List.map of_typed_ast exprs)
  | Ascribe (e, _) -> of_typed_ast e
