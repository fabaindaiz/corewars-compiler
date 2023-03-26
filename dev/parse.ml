(** Parser **)
open Ast
open Printf
open CCSexp

exception CTError of string


let rec parse_arg (sexp : sexp) : arg =
  match sexp with
  | `Atom s -> (
    match Int64.of_string_opt s with Some n -> Num (Int64.to_int n) | None -> Id s )
  | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp)))

let rec parse_expr (sexp : sexp) : expr =
  match sexp with
  | `List [eop; e1; e2] -> (
    match eop with 
    | `Atom "MOV" -> Mov(parse_arg e1, parse_arg e2)
    | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp))) )
  | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp)))


(* parse a program from a file *)
let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
   match CCSexp.parse_file filename with
   | Ok s -> s
   | Error msg -> raise (CTError (sprintf "Unable to parse file %s: %s" filename msg))
 
 let sexp_from_string (src : string) : CCSexp.sexp =
   match CCSexp.parse_string src with
   | Ok s -> s
   | Error msg -> raise (CTError (sprintf "Unable to parse string %s: %s" src msg))