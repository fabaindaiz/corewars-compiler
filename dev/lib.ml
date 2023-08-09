(* Lib *)
open Printf
open Ast

type aenv = (string * arg) list
type penv = (string * place) list
type lenv = (string * string) list

type env = aenv * penv * lenv

let empty_env : env = ([], [], [])


let extend_aenv (x : string) (arg : arg) (aenv : aenv) : aenv =
  ((x, arg) :: aenv)

let translate_aenv (x : string) (aenv : aenv) : arg =
  (match List.assoc_opt x aenv with
  | Some arg -> arg
  | None -> failwith (sprintf "unbound variable %s in aenv" x) )

let extend_penv (x : string) (place : place) (penv : penv) : penv =
  ((x, place) :: penv)

let extend_lenv (x : string) (label : string) (lenv : lenv) : lenv =
  ((x, label) :: lenv)
