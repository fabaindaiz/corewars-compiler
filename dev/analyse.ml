(** Analyser **)
open String
open Ast
open Lib


let analyse_store_arg (arg : arg) (id : string) (place : place) (penv : penv) : penv =
  match arg with
  | AStore (s) ->
    (match (equal id s) with
    | true -> (extend_penv s place penv)
    | false -> penv)
  | _ -> penv

let analyse_store_cond (cond : cond) (id : string) (penv : penv) : penv =
  match cond with
  | Cond0 -> penv
  | Cond1 (_, a) -> (analyse_store_arg a id PB penv)
  | Cond2 (_, a1, a2) -> 
    let penv' = (analyse_store_arg a1 id PA penv) in
    (analyse_store_arg a2 id PB penv')

let rec analyse_store_expr (e : tag eexpr) (id : string) (penv : penv) : penv =
  match e with
  | EPrim2 (_, _, a1, a2, _) ->
    let env' = (analyse_store_arg a1 id PA penv) in
    (analyse_store_arg a2 id PB env')
  | EFlow1 (_, cond, exp, _) ->
    let env' = (analyse_store_cond cond id penv) in
    (analyse_store_expr exp id env')
  | EFlow2 (_, cond, exp1, exp2, _) ->
    let penv' = (analyse_store_cond cond id penv) in
    let penv'' = (analyse_store_expr exp1 id penv') in
    (analyse_store_expr exp2 id penv'')
  | ELet (_, _, exp, _) -> (analyse_store_expr exp id penv)
  | ESeq (exps, _) -> List.fold_left (fun penv' exp -> (analyse_store_expr exp id penv')) penv exps
  | _ -> penv


let analyse_let (id : string) (arg : arg) (body : tag eexpr) (label : string) (env : env) : env =
  let aenv, penv, lenv = env in
  let aenv' = (extend_aenv id arg aenv) in
  let penv' = (analyse_store_expr body id penv) in
  let lenv' = (extend_lenv id label lenv) in
  (aenv', penv', lenv')
