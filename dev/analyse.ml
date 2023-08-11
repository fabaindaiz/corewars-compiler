(** Analyser **)
open String
open Ast
open Lib
open Verify

exception CTError of string


let analyse_store_arg (arg : arg) (id : string) (place : place) (penv : penv) : penv =
  match arg with
  | AStore (s) ->
    (match (equal id s) with
    | true -> (extend_penv s place penv)
    | _ -> penv)
  | _ -> penv

let analyse_store_cond (cond : cond) (id : string) (penv : penv) : penv =
  match cond with
  | Cond1 (_, a) -> (analyse_store_arg a id PB penv)
  | Cond2 (_, a1, a2) -> 
    let penv' = (analyse_store_arg a1 id PA penv) in
    (analyse_store_arg a2 id PB penv')

let rec analyse_store_expr (e : tag eexpr) (id : string) (penv : penv) : penv =
  match e with
  | EPrim2 (_, a1, a2, _) ->
    let env' = (analyse_store_arg a1 id PA penv) in
    (analyse_store_arg a2 id PB env')
  | ELet (_, _, e, _) ->
    (analyse_store_expr e id penv)
  | ECont1 (_, cond, expr, _) ->
    let env' = (analyse_store_cond cond id penv) in
    (analyse_store_expr expr id env')
  | ERepeat (e, _) -> (analyse_store_expr e id penv)
  | ESeq (exprs, _) -> List.fold_left (fun penv' e -> (analyse_store_expr e id penv')) penv exprs
  | _ -> penv

let analyse_let (id : string) (arg : arg) (body : tag eexpr) (label : string) (env : env) : env =
  let _ = (verify_let arg) in
  let aenv, penv, lenv = env in
  let aenv' = (extend_aenv id arg aenv) in
  let penv' = (analyse_store_expr body id penv) in
  let lenv' = (extend_lenv id label lenv) in
  (aenv', penv', lenv')