(* Lib *)
open Printf
open Red
open Ast


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


let compile_mode (mode : mode) (dest : place) : rmode =
  match dest with
  | PA ->
    (match mode with
    | MDir -> RDir
    | MInd -> RAInd
    | MDec -> RAPre
    | MInc -> RAPos )
  | PB -> 
    (match mode with
    | MDir -> RDir
    | MInd -> RBInd
    | MDec -> RBPre
    | MInc -> RBPos )


type opmod =
| TNum
| TRef
| TA
| TB

let rec compile_opmod (arg : arg) (env : env) : opmod =
  let aenv, penv, _ = env in
  match arg with
  | ANone -> TNum
  | ANum _ -> TNum
  | ARef (_, _) -> TRef
  | AId s | ALab (_, s) ->
    (match List.assoc_opt s aenv with
    | Some arg ->
      (match arg with
      | AId (x) | ALab (_, x) ->
        (match List.assoc_opt x penv with
        | Some store -> 
          (match store with
          | PA -> TA
          | PB -> TB )
        | None -> 
          let store = (translate_penv s penv) in
          (match store with
          | PA -> TA
          | PB -> TB ))
      | _ ->
        let store = (translate_penv s penv) in
        (match store with
        | PA -> TA
        | PB -> TB ))
    | None -> TRef )
  | AStore (s) -> 
    let arg = (translate_aenv s aenv) in
    (compile_opmod arg env)


let compile_mod (arg1 : arg) (arg2 : arg) (env : env) : rmod =
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
