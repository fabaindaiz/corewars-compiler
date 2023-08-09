(** Parser **)
open Ast
open Printf
open CCSexp

exception CTError of string


let parse_place (sexp : sexp) : place =
  match sexp with
  | `Atom "A" -> A
  | `Atom "B" -> B
  | _ -> raise (CTError (sprintf "Not a valid mode: %s" (to_string sexp)))

let parse_mode (sexp : sexp) : mode =
  match sexp with
  | `Atom "Dir" -> ADir
  | `Atom "Ind" -> AInd
  | `Atom "Dec" -> ADec
  | `Atom "Inc" -> AInc
  | _ -> raise (CTError (sprintf "Not a valid mode: %s" (to_string sexp)))

let parse_arg (sexp : sexp) : arg =
  match sexp with
  | `List [`Atom "place"; `Atom s] -> Place (s)
  | `Atom s ->
    (match Int64.of_string_opt s with
    | Some n -> Num (Int64.to_int n)
    | None -> Id (s) )
  | `List [m; `Atom s] ->
    (match Int64.of_string_opt s with
    | Some n -> Ref ((parse_mode m), (Int64.to_int n))
    | None -> Lab ((parse_mode m), s) )
  | _ -> raise (CTError (sprintf "Not a valid arg: %s" (to_string sexp)))

let parse_cond (sexp : sexp) : cond =
  match sexp with
  | `List [`Atom "JZ"; e] -> Cjz (parse_arg e)
  | `List [`Atom "JN"; e] -> Cjn (parse_arg e)
  | `List [`Atom "DN"; e] -> Cdn (parse_arg e)
  | `List [`Atom "EQ"; e1 ; e2] -> Ceq (parse_arg e1, parse_arg e2)
  | `List [`Atom "NE"; e1 ; e2] -> Cne (parse_arg e1, parse_arg e2)
  | `List [`Atom "GT"; e1 ; e2] -> Cgt (parse_arg e1, parse_arg e2)
  | `List [`Atom "LT"; e1 ; e2] -> Clt (parse_arg e1, parse_arg e2)
  | _ -> raise (CTError (sprintf "Not a valid cond: %s" (to_string sexp)))

let rec parse_exp (sexp : sexp) : expr =
  match sexp with
  | `List [`Atom "NOP"] -> Nop
  | `List [`Atom "label"; `Atom s] -> Label (s)
  | `List [`Atom "point"; `Atom s] -> Point (s)
  | `List (`Atom "seq" :: exps) -> Seq (List.map parse_exp exps)
  | `List [eop; e] ->
    (match eop with
    | `Atom "JMP" -> Jmp (ANone, parse_arg e)
    | `Atom "SPL" -> Spl (ANone, parse_arg e)
    | `Atom "repeat" -> Repeat (parse_exp e)
    | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp))) )
  | `List [eop; e1; e2] ->
    (match eop with 
    | `Atom "let" ->
      (match e1 with
      | `List [`Atom id; p; e] -> Let (id, parse_place p, parse_arg e, parse_exp e2)
      | _ -> raise (CTError (sprintf "Not a valid let assignment: %s" (to_string e1))) )
    | `Atom "MOV" -> Mov (parse_arg e1, parse_arg e2)
    | `Atom "ADD" -> Add (parse_arg e1, parse_arg e2)
    | `Atom "SUB" -> Sub (parse_arg e1, parse_arg e2)
    | `Atom "MUL" -> Mul (parse_arg e1, parse_arg e2)
    | `Atom "DIV" -> Div (parse_arg e1, parse_arg e2)
    | `Atom "MOD" -> Mod (parse_arg e1, parse_arg e2)
    | `Atom "JMP" -> Jmp (parse_arg e1, parse_arg e2)
    | `Atom "SPL" -> Spl (parse_arg e1, parse_arg e2)
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