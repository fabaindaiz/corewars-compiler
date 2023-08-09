(* Lib *)
open Printf
open Ast


type aenv = (string * arg) list
type lenv = (string * string) list

let empty_aenv : aenv = []
let empty_lenv : lenv = []


let extend_aenv (x : string) (arg : arg) (aenv : aenv) : aenv =
  ((x, arg) :: aenv)

let extend_lenv (x : string) (label : string) (lenv : lenv) : lenv =
  ((x, label) :: lenv)
