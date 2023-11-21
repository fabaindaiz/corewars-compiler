open Common
open Ast

module Env = struct
  module StringMap = Map.Make (String)

  type 'a t = 'a StringMap.t

  let empty = StringMap.empty
  let add id value env = StringMap.add id value env
  let add_seq seq env = StringMap.add_seq seq env

  let find id env =
    match StringMap.find_opt id env with
    | Some v -> v
    | None -> Error.(error (Syntax (UnboundVariable id)))
end

let uniquify t =
  let lift desc = { t with desc } in
  let rec go (t : 'a t) env : 'a t =
    (match t.desc with
    | Arg arg -> Arg arg
    | Var x -> Var (Env.find x env)
    | Lab l -> Lab l
    | Prim1 (op1, t1) -> Prim1 (op1, go t1 env)
    | Prim2 (op2, t1, t2) -> Prim2 (op2, go t1 env, go t2 env)
    | Prim3 (op3, t1, t2, t3) -> Prim3 (op3, go t1 env, go t2 env, go t3 env)
    | Let (binding, body) ->
        let name' =
          (match binding with
          | Bname { name } -> Gensym.fresh name
          | Bexpr { name; term = _ } -> Gensym.fresh name)
        in
        let env' = 
          (match binding with
          | Bname { name } -> Env.add name name' env
          | Bexpr { name; term = _ } -> Env.add name name' env)
        in
        let binding' =
          (match binding with
          | Bname { name = _ } -> Bname { name = name' }
          | Bexpr { name = _; term } -> Bexpr { name = name'; term = go term env })
        in
        Let (binding', go body env')
    | Seq exprs -> Seq (List.map (fun e -> go e env) exprs))
    |> lift
  in
  go t Env.empty
