(** Parser **)
open CCSexp
open Printf
open String
open Ast

exception CTError of string


let parse_mode (sexp : sexp) : mode =
  match sexp with
  | `Atom "ins" | `Atom "I" -> MIns (MI)
  | `Atom "sam" | `Atom "F" -> MIns (MF)
  | `Atom "opp" | `Atom "X" -> MIns (MX)
  | `Atom "Imm" | `Atom "#" -> MImm
  | `Atom "Dir" | `Atom "$" -> MDir
  | `Atom "Ind" | `Atom "@" -> MInd (MINone)
  | `Atom "Dec" | `Atom "<" -> MInd (MIDec)
  | `Atom "Inc" | `Atom ">" -> MInd (MIInc)
  | _ -> raise (CTError (sprintf "Not a valid mode: %s" (to_string sexp)))

let parse_arg (sexp : sexp) : arg =
  match sexp with
  | `Atom "none" -> ANone
  | `List [`Atom "store"; `Atom s] | `List [`Atom "!"; `Atom s] -> AStore (s)
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
  | `List [`Atom "DZ"; e] -> Cond1 (Cdz, parse_arg e)
  | `List [`Atom "DN"; e] -> Cond1 (Cdn, parse_arg e)
  | `List [`Atom "EQ"; e1 ; e2] -> Cond2 (Ceq, parse_arg e1, parse_arg e2)
  | `List [`Atom "NE"; e1 ; e2] -> Cond2 (Cne, parse_arg e1, parse_arg e2)
  | `List [`Atom "GT"; e1 ; e2] -> Cond2 (Cgt, parse_arg e1, parse_arg e2)
  | `List [`Atom "LT"; e1 ; e2] -> Cond2 (Clt, parse_arg e1, parse_arg e2)
  | _ -> raise (CTError (sprintf "Not a valid cond: %s" (to_string sexp)))


let rec parse_exp (sexp : sexp) : expr =
  match sexp with
  | `List (`Atom "com" :: s) ->
    Comment (List.fold_left (fun res s -> res ^ " " ^ (escaped (to_string s))) "" s)
  | `List [`Atom "label"; `Atom s] -> Label (s)
  | `List (`Atom "seq" :: exps) -> Seq (List.map parse_exp exps)
  | `List [eop] ->
    (match eop with
    | `Atom "NOP" -> Prim2 (Nop, ANone, ANone)
    | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp))) )
  | `List [eop; e] ->
    (match eop with
    | `Atom "JMP" -> Prim2 (Jmp, parse_arg e, ANone)
    | `Atom "SPL" -> Prim2 (Spl, parse_arg e, ANone)
    | `Atom "NOP" -> Prim2 (Nop, parse_arg e, ANone)
    | `Atom "repeat" -> Flow (Repeat, parse_exp e)
    | _ -> raise (CTError (sprintf "Not a valid unary expr: %s" (to_string sexp))) )
  | `List [eop; e1; e2] ->
    (match eop with 
    | `Atom "DAT" -> Prim2 (Dat, parse_arg e1, parse_arg e2)
    | `Atom "MOV" -> Prim2 (Mov, parse_arg e1, parse_arg e2)
    | `Atom "ADD" -> Prim2 (Add, parse_arg e1, parse_arg e2)
    | `Atom "SUB" -> Prim2 (Sub, parse_arg e1, parse_arg e2)
    | `Atom "MUL" -> Prim2 (Mul, parse_arg e1, parse_arg e2)
    | `Atom "DIV" -> Prim2 (Div, parse_arg e1, parse_arg e2)
    | `Atom "MOD" -> Prim2 (Mod, parse_arg e1, parse_arg e2)
    | `Atom "JMP" -> Prim2 (Jmp, parse_arg e1, parse_arg e2)
    | `Atom "SPL" -> Prim2 (Spl, parse_arg e1, parse_arg e2)
    | `Atom "NOP" -> Prim2 (Nop, parse_arg e1, parse_arg e2)
    | `Atom "JMZ" -> Prim2 (Jmz, parse_arg e1, parse_arg e2)
    | `Atom "JMN" -> Prim2 (Jmn, parse_arg e1, parse_arg e2)
    | `Atom "DJN" -> Prim2 (Djn, parse_arg e1, parse_arg e2)
    | `Atom "SEQ" -> Prim2 (Seq, parse_arg e1, parse_arg e2)
    | `Atom "SNE" -> Prim2 (Sne, parse_arg e1, parse_arg e2)
    | `Atom "SLT" -> Prim2 (Slt, parse_arg e1, parse_arg e2)
    | `Atom "STP" -> Prim2 (Stp, parse_arg e1, parse_arg e2)
    | `Atom "LDP" -> Prim2 (Ldp, parse_arg e1, parse_arg e2)
    | `Atom "if" -> Flow1 (If, parse_cond e1, parse_exp e2)
    | `Atom "while" -> Flow1 (While, parse_cond e1, parse_exp e2)
    | `Atom "do-while" -> Flow1 (DoWhile, parse_cond e1, parse_exp e2)
    | `Atom "let" ->
      (match e1 with
      | `List [`Atom id; e] -> Let (id, parse_arg e, parse_exp e2)
      | _ -> raise (CTError (sprintf "Not a valid let assignment: %s" (to_string e1))) )
    | _ -> raise (CTError (sprintf "Not a valid binary expr: %s" (to_string sexp))) )
  | `List [eop; e1; e2; e3] ->
    (match eop with
    | `Atom "if" -> Flow2 (IfElse, parse_cond e1, parse_exp e2, parse_exp e3)
    | _ -> raise (CTError (sprintf "Not a valid ternary expr: %s" (to_string sexp))) )
  | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp)))


(* parse a program from a file *)
let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
   match CCSexp.parse_file filename with
   | Ok s -> s
   | Error msg -> raise (CTError (sprintf "Unable to parse file %s: %s" filename msg))
 
(* parse a program from a string *)
let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> raise (CTError (sprintf "Unable to parse string %s: %s" src msg))
