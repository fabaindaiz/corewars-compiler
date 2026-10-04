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
  match List.assoc_opt x aenv with
  | Some arg -> arg
  | None -> let x = Rename.original x in error (sprintf "(store %s): %s is not a variable of an enclosing let" x x)

let translate_penv (x : string) (penv : penv) : place =
  match List.assoc_opt x penv with
  | Some place -> place
  | None -> let x = Rename.original x in error (sprintf "variable `%s` is used but no (store %s) places it" x x)

let translate_lenv (x : string) (lenv : lenv) : string =
  match List.assoc_opt x lenv with
  | Some label -> label
  | None -> failwith (sprintf "no label for variable %s" x)


let replace_store (arg : arg) (env : env) : arg =
  let aenv, _, _ = env in
  match arg with
  | AStore (s) -> (translate_aenv s aenv)
  | _ -> arg


let jump_label (label : string) : instruction =
  INSTR (IJMP, RN, RLab(RDir, label), RNone)
