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
    | Label l -> Label l
    | Prim1 (op1, t1) -> Prim1 (op1, go t1 env)
    | Prim2 (op2, t1, t2) -> Prim2 (op2, go t1 env, go t2 env)
    | Prim3 (op3, t1, t2, t3) -> Prim3 (op3, go t1 env, go t2 env, go t3 env)
    | Let (binding, body) ->
        let name' = Gensym.fresh binding.name in
        let env' = Env.add binding.name name' env in
        Let ({ name = name'; term = go binding.term env }, go body env')
    | Seq exprs -> Seq (List.map (fun e -> go e env) exprs))
    |> lift
  in
  go t Env.empty
