type 'a t = { desc : 'a desc; meta : 'a }

and 'a desc =
  | Arg of Arg_stored.t
  | Var of string
  | Lab of string
  | Prim1 of Prim.op1 * 'a t
  | Prim2 of Prim.op2 * 'a t * 'a t
  | Prim3 of Prim.op3 * 'a t * 'a t * 'a t
  | Let of 'a binding * 'a t
  | Seq of 'a t list

and 'a binding =
  | Bname of { name : string }
  | Bexpr of { name : string; term : 'a t }

[@@deriving show { with_path = false }]

let rec trans0 (t : 'a Ast.t) : 'a t =
  match t.desc with
  | Arg arg -> None
    | Var x -> None
    | Lab l -> None
    | Prim1 (op1, t1) -> None
    | Prim2 (op2, t1, t2) -> None
    | Prim3 (op3, t1, t2, t3) -> None
    | Let (binding, body) ->
        let binding' =
          (match binding with
          | Bname { name = _ } -> Bname { name = name' }
          | Bexpr { name = _; term } -> Bexpr { name = name'; term = go term env })
        in
        let body' =  in
        Let (binding', body')

    | Seq exprs -> None

let rec store (_ : 'a Ast.t) =
  let rec go =
    None
  in
  go