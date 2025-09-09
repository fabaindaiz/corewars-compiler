open Common.Env
open Common.Type
open Ast

let rec surface_typing (m : surf) (env : tenv) : ttype =
  match m with
  | Var x -> lookup_env x env
  | Const k -> TBase (const_typing k)
  | Let (x, e, b) ->
    let env' = (x, surface_typing e env) :: env in
    surface_typing b env'
  | Abs (x, a, n) ->
    let env' = (x, a) :: env in
    let b = surface_typing n env' in
    TArrow (a, b)
  | App (l, m) ->
    (match surface_typing l env with
    | TArrow (a, b) ->
      let a' = surface_typing m env in
      check (=) a' a; b
    | _ -> raise (TypeError "Application type mismatch"))
  | Seq exprs ->
    let types = List.rev_map (fun m -> surface_typing m env) exprs in
    List.hd types