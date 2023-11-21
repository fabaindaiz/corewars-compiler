type e = { desc : desc }

and desc =
  | Arg of Arg.t
  | Var of string
  | Label of string
  | Prim1 of Prim.op1 * e
  | Prim2 of Prim.op2 * e * e
  | Prim3 of Prim.op3 * e * e * e
  | Let of binding * e
  | Seq of e list

and binding = { name : string; ty : Type.t; e : e }

[@@deriving show { with_path = false }]

let to_ast (e : e) : Type.t Ast.t =
  let module Env = Map.Make (String) in
  let lift desc meta = Ast.{ desc; meta } in
  let rec go e env =
    match e.desc with
    | Arg arg -> lift (Ast.Arg arg) (Arg.type_of arg)
    | Var x -> lift (Ast.Var x) (Env.find x env)
    | Label l -> lift (Ast.Label l) Type.Instr
    | Prim1 (op1, e) ->
        let t1 = go e env in
        let _, ty = Type.ty_of_op1 op1 in
        lift (Ast.Prim1 (op1, t1)) ty
    | Prim2 (op2, e1, e2) ->
        let t1 = go e1 env in
        let t2 = go e2 env in
        let _, _, ty = Type.ty_of_op2 op2 in
        lift (Ast.Prim2 (op2, t1, t2)) ty
    | Prim3 (op3, e1, e2, e3) ->
        let t1 = go e1 env in
        let t2 = go e2 env in
        let t3 = go e3 env in
        let _, _, _, ty = Type.ty_of_op3 op3 in
        lift (Ast.Prim3 (op3, t1, t2, t3)) ty
    | Let (binding, body) ->
        let binding' = Ast.{ name = binding.name; term = go binding.e env } in
        let env' = Env.add binding.name binding.ty env in
        let body' = go body env' in
        lift (Ast.Let (binding', body')) body'.meta
    | Seq exprs ->
        let terms = List.map (fun e -> go e env) exprs in
        let types = List.map (fun (t : 'a Ast.t) -> t.meta) terms in
        lift (Ast.Seq terms) (Type.Product types)
  in
  go e Env.empty
