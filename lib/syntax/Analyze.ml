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

let rec analize_store_expr (e : e) (id : string) (place : place) env =
  match e with
  | Value v ->
      (match v with
      | Arg (_, a) -> 
        (match a with
        | Store x ->
          (match (String.equal id x) with
          | true -> Env.add id place env
          | false -> env)
        | _ -> env)
      | Var _ -> env)
  | Label _ -> env
  | Lam (_, e) ->
      analize_store_expr e id PNone env
  | App (_, e2) ->
      analize_store_expr e2 id PNone env
  | Prim1 (op, e) ->
      let p =
        (match op with
        | Repeat -> PNone
        | _ -> PB) in
      analize_store_expr e id p env
  | Prim2 (op, e1, e2) ->
      let p1, p2 =
        (match op with
        | If | While | DoWhile -> PNone, PNone
        | _ -> PA, PB) in
      let env' = analize_store_expr e1 id p1 env in
      analize_store_expr e2 id p2 env'
  | Prim3 (op, e1, e2, e3) ->
      let p1, p2, p3 =
        (match op with
        | IfElse -> PNone, PNone, PNone) in
      let env' = analize_store_expr e1 id p1 env in
      let env'' = analize_store_expr  e2 id p2 env' in
      analize_store_expr e3 id p3 env''
  | Let (_, e1, e2) ->
      let env' = analize_store_expr e1 id PNone env in
      analize_store_expr e2 id PNone env'
  | Tuple exprs ->
      List.fold_left (fun env' e -> analize_store_expr e id PNone env') env exprs

let resolve_store (e : Ast.e) : e =
  let rec go (e : e) env : e =
    match e with
    | Value v -> Value
      (match v with
      | Arg (_, a) ->
        (match a with
        | Store x -> Arg (Env.find x env, Store x)
        | Id x -> Arg (Env.find x env, Id x)
        | Lab (m, x) -> Arg (Env.find x env, Lab (m, x))
        | _ -> v)
      | Var _ -> v)
    | Label s -> Label s
    | Lam (x, e) ->
      let env' = analize_store_expr e x PNone env in
      Lam (x, go e env')
    | App (e1, e2) -> App (go e1 env, go e2 env)
    | Prim1 (op, e) -> Prim1 (op, go e env)
    | Prim2 (op, e1, e2) -> Prim2 (op, go e1 env, go e2 env)
    | Prim3 (op, e1, e2, e3) -> Prim3 (op, go e1 env, go e2 env, go e3 env)
    | Let (x, e1, e2) ->
        let env' = analize_store_expr e2 x PNone env in
        Let (x, go e1 env, go e2 env')
    | Tuple exprs -> Tuple (List.map (fun e -> go e env) exprs)
  in
  go e Env.empty
