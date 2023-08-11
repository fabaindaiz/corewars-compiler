(** Verifier **)
open Printf
open Ast

exception CTError of string


let verify_let (arg : arg) =
  match arg with
  | AStore (s) -> raise (CTError (sprintf "Not a valid store for var: %s" s))
  | _ -> None
