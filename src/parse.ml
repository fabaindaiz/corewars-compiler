(** Parser **)
open CCSexp
open Printf
open Ast

exception CTError of string


let parse_mode (sexp : sexp) : mode =
  match sexp with
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
  | `List [cop; a] ->
    let parg = (parse_arg a) in
    (match cop with
    | `Atom "JZ" -> Cond1 (Cjz, parg)
    | `Atom "JN" -> Cond1 (Cjn, parg)
    | `Atom "DZ" -> Cond1 (Cdz, parg)
    | `Atom "DN" -> Cond1 (Cdn, parg)
    | _ -> raise (CTError (sprintf "Not a valid unary cond: %s" (to_string sexp))) )
  | `List [cop; a1; a2] ->
    let parg1 = (parse_arg a1) in
    let parg2 = (parse_arg a2) in
    (match cop with
    | `Atom "EQ" -> Cond2 (Ceq, parg1, parg2)
    | `Atom "NE" -> Cond2 (Cne, parg1, parg2)
    | `Atom "GT" -> Cond2 (Cgt, parg1, parg2)
    | `Atom "LT" -> Cond2 (Clt, parg1, parg2)
    | _ -> raise (CTError (sprintf "Not a valid binary cond: %s" (to_string sexp))) )
  | _ -> raise (CTError (sprintf "Not a valid cond: %s" (to_string sexp)))


let parse_imod (sexp : sexp) : imod =
  match sexp with
  | `Atom "A" -> MA
  | `Atom "B" -> MB
  | `Atom "AB" -> MAB
  | `Atom "BA" -> MBA
  | `Atom "I" -> MI
  | `Atom "F" -> MF
  | `Atom "X" -> MX
  | _ -> raise (CTError (sprintf "Not a valid imod: %s" (to_string sexp)))

let rec parse_exp (sexp : sexp) : expr =
  match sexp with
  | `List (`Atom "com" :: exps) -> Comment (List.fold_left (fun res s -> res ^ " " ^ (String.escaped (to_string s))) "" exps)
  | `List (`Atom "seq" :: exps) -> Seq (List.map parse_exp exps)
  | `List [`Atom "label"; `Atom s] -> Label (s)
  | `List [eop] ->
    (match eop with
    | `Atom "DAT" -> Prim2 (Dat, MN, ANone, ANone)
    | `Atom "NOP" -> Prim2 (Nop, MN, ANone, ANone)
    | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp))) )
  | `List [eop; e] ->
    (match eop with
    | `Atom "DAT" -> Prim2 (Dat, MN, ANone, parse_arg e)
    | `Atom "JMP" -> Prim2 (Jmp, MN, parse_arg e, ANone)
    | `Atom "SPL" -> Prim2 (Spl, MN, parse_arg e, ANone)
    | `Atom "NOP" -> Prim2 (Nop, MN, parse_arg e, ANone)
    | `Atom "repeat" -> Flow1 (Repeat, Cond0, parse_exp e)
    | _ -> raise (CTError (sprintf "Not a valid unary expr: %s" (to_string sexp))) )
  | `List [eop; e1; e2] ->
    (match eop with 
    | `Atom "DAT" -> Prim2 (Dat, MN, parse_arg e1, parse_arg e2)
    | `Atom "JMP" -> Prim2 (Jmp, MN, parse_arg e1, parse_arg e2)
    | `Atom "SPL" -> Prim2 (Spl, MN, parse_arg e1, parse_arg e2)
    | `Atom "NOP" -> Prim2 (Nop, MN, parse_arg e1, parse_arg e2)
    | `Atom "MOV" -> Prim2 (Mov, MDef, parse_arg e1, parse_arg e2)
    | `Atom "ADD" -> Prim2 (Add, MDef, parse_arg e1, parse_arg e2)
    | `Atom "SUB" -> Prim2 (Sub, MDef, parse_arg e1, parse_arg e2)
    | `Atom "MUL" -> Prim2 (Mul, MDef, parse_arg e1, parse_arg e2)
    | `Atom "DIV" -> Prim2 (Div, MDef, parse_arg e1, parse_arg e2)
    | `Atom "MOD" -> Prim2 (Mod, MDef, parse_arg e1, parse_arg e2)
    | `Atom "JMZ" -> Prim2 (Jmz, MDef, parse_arg e1, parse_arg e2)
    | `Atom "JMN" -> Prim2 (Jmn, MDef, parse_arg e1, parse_arg e2)
    | `Atom "DJN" -> Prim2 (Djn, MDef, parse_arg e1, parse_arg e2)
    | `Atom "SEQ" -> Prim2 (Seq, MDef, parse_arg e1, parse_arg e2)
    | `Atom "SNE" -> Prim2 (Sne, MDef, parse_arg e1, parse_arg e2)
    | `Atom "SLT" -> Prim2 (Slt, MDef, parse_arg e1, parse_arg e2)
    | `Atom "STP" -> Prim2 (Stp, MDef, parse_arg e1, parse_arg e2)
    | `Atom "LDP" -> Prim2 (Ldp, MDef, parse_arg e1, parse_arg e2)
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
    | `Atom "MOV" -> Prim2 (Mov, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "ADD" -> Prim2 (Add, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "SUB" -> Prim2 (Sub, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "MUL" -> Prim2 (Mul, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "DIV" -> Prim2 (Div, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "MOD" -> Prim2 (Mod, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "JMZ" -> Prim2 (Jmz, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "JMN" -> Prim2 (Jmn, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "DJN" -> Prim2 (Djn, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "SEQ" -> Prim2 (Seq, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "SNE" -> Prim2 (Sne, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "SLT" -> Prim2 (Slt, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "STP" -> Prim2 (Stp, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "LDP" -> Prim2 (Ldp, parse_imod e1, parse_arg e2, parse_arg e3)
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
