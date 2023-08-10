(* Lib *)
open Printf
open Red
open Ast


let gensym =
  let a_counter = ref 0 in
  (fun basename ->
    if (compare basename "") == 0 then
      a_counter := 0
    else
      incr a_counter;
      (sprintf "%s%d" basename !a_counter) );;


type aenv = (string * arg) list
type penv = (string * place) list
type lenv = (string * string) list

type env = aenv * penv * lenv

let empty_env : env = ([], [], [])


let extend_aenv (x : string) (arg : arg) (aenv : aenv) : aenv =
  ((x, arg) :: aenv)

let extend_penv (x : string) (place : place) (penv : penv) : penv =
  ((x, place) :: penv)

let extend_lenv (x : string) (label : string) (lenv : lenv) : lenv =
  ((x, label) :: lenv)


let translate_aenv (x : string) (aenv : aenv) : arg =
  (match List.assoc_opt x aenv with
  | Some arg -> arg
  | None -> failwith (sprintf "unbound variable %s in aenv" x) )

let translate_penv (x : string) (penv : penv) : place =
  (match List.assoc_opt x penv with
  | Some place -> place
  | None -> failwith (sprintf "unbound variable %s in penv" x) )

let translate_lenv (x : string) (lenv : lenv) : string =
  (match List.assoc_opt x lenv with
  | Some label -> label
  | None -> failwith (sprintf "unbound variable %s in lenv" x) )


type opmod =
| TNum
| TRef
| TA
| TB

let rec compile_opmod (arg : arg) (env : env) : opmod =
  let aenv, penv, _ = env in
  match arg with
  | ANone -> TNum
  | Num _ -> TNum
  | Ref (_, _) -> TRef
  | Id s | Lab (_, s) ->
    (match List.assoc_opt s penv with
    | Some place ->
      (match place with
      | A -> TA
      | B -> TB )
    | None -> TRef )
  | Place (s) -> 
    let arg = (translate_aenv s aenv) in
    (compile_opmod arg env)


let compile_mod_mov (arg1 : arg) (arg2 : arg) (env : env) : rmod =
  let mod1 = (compile_opmod arg1 env) in
  let mod2 = (compile_opmod arg2 env) in
  match mod1, mod2 with
  | TNum, TNum -> RI
  | TNum, TA -> RA
  | TNum, TB -> RAB
  | TA, TNum -> RAB
  | TA, TA -> RA
  | TA, TB -> RAB
  | TB, TNum -> RB
  | TB, TA -> RBA
  | TB, TB -> RB
  | _, _ -> RI
    
let compile_mod_sum (arg1 : arg) (arg2 : arg) (env : env) : rmod =
  let mod1 = (compile_opmod arg1 env) in
  let mod2 = (compile_opmod arg2 env) in
  match mod1, mod2 with
  | TNum, TNum -> RI
  | TNum, TA -> RA
  | TNum, TB -> RAB
  | TA, TNum -> RAB
  | TA, TA -> RA
  | TA, TB -> RAB
  | TB, TNum -> RB
  | TB, TA -> RBA
  | TB, TB -> RB
  | _, _ -> RF
    
let compile_mod_jmp (arg1 : arg) (arg2 : arg) (env : env) : rmod =
  let mod1 = (compile_opmod arg1 env) in
  let mod2 = (compile_opmod arg2 env) in
  match mod1, mod2 with
  | TNum, TNum -> RI
  | TNum, TA -> RA
  | TNum, TB -> RAB
  | TA, TNum -> RAB
  | TA, TA -> RA
  | TA, TB -> RAB
  | TB, TNum -> RB
  | TB, TA -> RBA
  | TB, TB -> RB
  | _, _ -> RI
