(** Parser **)
open CCSexp
open Printf
open Ast

exception CTError of string


let parse_mode (sexp : sexp) : mode =
  match sexp with
  | `Atom "Dir" -> MDir
  | `Atom "Ind" -> MInd
  | `Atom "Dec" -> MDec
  | `Atom "Inc" -> MInc
  | _ -> raise (CTError (sprintf "Not a valid mode: %s" (to_string sexp)))

let parse_arg (sexp : sexp) : arg =
  match sexp with
  | `Atom "None" -> ANone
  | `List [`Atom "store"; `Atom s] -> AStore (s)
  | `Atom s ->
    (match Int64.of_string_opt s with
    | Some n -> ANum (Int64.to_int n)
    | None -> AId (s) )
  | `List [m; `Atom s] ->
    (match Int64.of_string_opt s with
    | Some n -> ARef ((parse_mode m), (Int64.to_int n))
    | None -> ALab ((parse_mode m), s) )
  | _ -> raise (CTError (sprintf "Not a valid arg: %s" (to_string sexp)))

let parse_cond (sexp : sexp) : cond =
  match sexp with
  | `List [`Atom "JZ"; e] -> Cond1 (Cjz, parse_arg e)
  | `List [`Atom "JN"; e] -> Cond1 (Cjn, parse_arg e)
  | `List [`Atom "DN"; e] -> Cond1 (Cdn, parse_arg e)
  | `List [`Atom "EQ"; e1 ; e2] -> Cond2 (Ceq, parse_arg e1, parse_arg e2)
  | `List [`Atom "NE"; e1 ; e2] -> Cond2 (Cne, parse_arg e1, parse_arg e2)
  | `List [`Atom "GT"; e1 ; e2] -> Cond2 (Cgt, parse_arg e1, parse_arg e2)
  | `List [`Atom "LT"; e1 ; e2] -> Cond2 (Clt, parse_arg e1, parse_arg e2)
  | _ -> raise (CTError (sprintf "Not a valid cond: %s" (to_string sexp)))

let rec parse_exp (sexp : sexp) : expr =
  match sexp with
  | `List [`Atom "NOP"] -> Nop
  | `List [`Atom "label"; `Atom s] -> Label (s)
  | `List (`Atom "seq" :: exps) -> Seq (List.map parse_exp exps)
  | `List [eop; e] ->
    (match eop with
    | `Atom "JMP" -> Prim2 (Jmp, parse_arg e, ANone)
    | `Atom "SPL" -> Prim2 (Spl, parse_arg e, ANone)
    | `Atom "repeat" -> Repeat (parse_exp e)
    | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp))) )
  | `List [eop; e1; e2] ->
    (match eop with 
    | `Atom "let" ->
      (match e1 with
      | `List [`Atom id; e] -> Let (id, parse_arg e, parse_exp e2)
      | _ -> raise (CTError (sprintf "Not a valid let assignment: %s" (to_string e1))) )
    | `Atom "DAT" -> Prim2 (Dat, parse_arg e1, parse_arg e2)
    | `Atom "MOV" -> Prim2 (Mov, parse_arg e1, parse_arg e2)
    | `Atom "ADD" -> Prim2 (Add, parse_arg e1, parse_arg e2)
    | `Atom "SUB" -> Prim2 (Sub, parse_arg e1, parse_arg e2)
    | `Atom "MUL" -> Prim2 (Mul, parse_arg e1, parse_arg e2)
    | `Atom "DIV" -> Prim2 (Div, parse_arg e1, parse_arg e2)
    | `Atom "MOD" -> Prim2 (Mod, parse_arg e1, parse_arg e2)
    | `Atom "JMP" -> Prim2 (Jmp, parse_arg e1, parse_arg e2)
    | `Atom "SPL" -> Prim2 (Spl, parse_arg e1, parse_arg e2)
    | `Atom "if" -> Cont1 (If, parse_cond e1, parse_exp e2)
    | `Atom "while" -> Cont1 (While, parse_cond e1, parse_exp e2)
    | `Atom "do-while" -> Cont1 (Dowhile, parse_cond e1, parse_exp e2)
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
