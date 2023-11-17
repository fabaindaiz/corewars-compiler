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

let uniquify e =
  let rec go (e : e) env : e =
    match (e : e) with
    | Value v -> Value
      (match v with
      | Arg (p, a) -> Arg (p,
        (match a with
        | None | Num _ | Ref _ -> a
        | Store x -> Store (Env.find x env)
        | Id x -> Id (Env.find x env)
        | Lab (m, x) -> Lab (m, Env.find x env)))
      | Var x -> Var (Env.find x env))
    | Label (l) -> Label (l)
    | Lam (x, e) ->
        let x' = Gensym.fresh x in
        let env' = Env.add x x' env in
        Lam (x', go e env')
    | App (e1, e2) -> App (go e1 env, go e2 env)
    | Prim1 (op, e) -> Prim1 (op, go e env)
    | Prim2 (op, e1, e2) -> Prim2 (op, go e1 env, go e2 env)
    | Prim3 (op, e1, e2, e3) -> Prim3 (op, go e1 env, go e2 env, go e3 env)
    | Let (x, e1, e2) ->
        let x' = Gensym.fresh x in
        let env' = Env.add x x' env in
        Let (x', go e1 env, go e2 env')
    | Tuple exprs -> Tuple (List.map (fun e -> go e env) exprs)
  in
  go e Env.empty
