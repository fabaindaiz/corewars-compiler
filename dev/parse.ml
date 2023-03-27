(** Parser **)
open Ast
open Printf
open CCSexp

exception CTError of string


let parse_mode (sexp : sexp) : mode =
  match sexp with
  | `Atom "Dir" -> Dir
  | `Atom "Ind" -> Ind
  | _ -> raise (CTError (sprintf "Not a valid mode: %s" (to_string sexp)))

let parse_arg (sexp : sexp) : arg =
  match sexp with
  | `Atom s ->
    (match Int64.of_string_opt s with
    |Some n -> Num (Int64.to_int n) | None -> Id (s) )
  | `List [m; `Atom s] ->
    (let m = (parse_mode m) in
    match Int64.of_string_opt s with Some n -> Ref (m, (Int64.to_int n)) | None -> Lab (m, s) )
  | _ -> raise (CTError (sprintf "Not a valid arg: %s" (to_string sexp)))

let parse_aarg (sexp : sexp) : aarg =
  match sexp with
  | `List [`Atom "Dec"; e] -> Dec (parse_arg e)
  | `List [`Atom "Inc"; e] -> Inc (parse_arg e)
  | e -> Not (parse_arg e)

let parse_cond (sexp : sexp) : cond =
  match sexp with
  | `List [`Atom "JZ"; e] -> Cjz (parse_aarg e)
  | `List [`Atom "JN"; e] -> Cjn (parse_aarg e)
  | `List [`Atom "DN"; e] -> Cdn (parse_aarg e)
  | `List [`Atom "EQ"; e1 ; e2] -> Ceq (parse_aarg e1, parse_aarg e2)
  | `List [`Atom "NE"; e1 ; e2] -> Cne (parse_aarg e1, parse_aarg e2)
  | `List [`Atom "GT"; e1 ; e2] -> Cgt (parse_aarg e1, parse_aarg e2)
  | `List [`Atom "LT"; e1 ; e2] -> Clt (parse_aarg e1, parse_aarg e2)
  | _ -> raise (CTError (sprintf "Not a valid cond: %s" (to_string sexp)))

let rec parse_exp (sexp : sexp) : expr =
  match sexp with
  | `List (`Atom "seq" :: exps) ->
    Seq (List.map parse_exp exps)
  | `List [eop; e] ->
    (match eop with
    | `Atom "repeat" -> Repeat (parse_exp e)
    | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp))) )
  | `List [eop; e1; e2] ->
    (match eop with 
    | `Atom "let" ->
      (match e1 with
      | `List [`Atom id; e] -> Let (id, parse_aarg e, parse_exp e2)
      | _ -> raise (CTError (sprintf "Not a valid let assignment: %s" (to_string e1))) )
    | `Atom "MOV" -> Mov (parse_aarg e1, parse_aarg e2)
    | `Atom "SUB" -> Sub (parse_aarg e1, parse_aarg e2)
    | `Atom "if" -> If (parse_cond e1, parse_exp e2)
    | `Atom "while" -> While (parse_cond e1, parse_exp e2)
    | `Atom "do-while" -> Dowhile (parse_cond e1, parse_exp e2)
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