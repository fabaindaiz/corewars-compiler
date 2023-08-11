(** Analysis **)
open Printf
open Ast
open Lib

exception CTError of string


let verify_arg_let (arg : arg) =
  match arg with
  | AStore (s) -> raise (CTError (sprintf "Not a valid store for var: %s" s))
  | _ -> None

let analyse_arg (arg : arg) (place : place) (env) : env =
  let aenv, penv, lenv = env in
  match arg with
  | AStore (s) ->
    let penv' = (extend_penv s place penv) in
    (aenv, penv', lenv)
  | _ -> env

let analyse_cond (pcond : cond) (env : env) : env =
  match pcond with
  | Cond1 (_, a) -> (analyse_arg a PB env)
  | Cond2 (_, a1, a2) -> 
    let env' = (analyse_arg a1 PA env) in
    (analyse_arg a2 PB env')

let rec analyse_expr (expr : expr) (env : env) : env =
  match expr with
  | Prim2 (_, a1, a2) ->
    let env' = (analyse_arg a1 PA env) in
    (analyse_arg a2 PB env')
  | Let (_, a, e) ->
    let _ = (verify_arg_let a) in
    (analyse_expr e env)
  | Cont1 (_, cond, expr) ->
    let env' = (analyse_cond cond env) in
    (analyse_expr expr env')
  | Repeat (expr) ->(analyse_expr expr env)
  | Seq (exprs) -> List.fold_left (fun res e -> (analyse_expr e res)) env exprs
  | _ -> env
