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
    (match s with
    | "none" -> ANone
    | _ -> 
      (match Int64.of_string_opt s with
      | Some n -> ANum (Int64.to_int n)
      | None -> AId (s) ))
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
    | `Atom "DAT" -> Prim2 (Dat, ANone, ANone)
    | `Atom "NOP" -> Prim2 (Nop, ANone, ANone)
    | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp))) )
  | `List [eop; e] ->
    (match eop with
    | `Atom "DAT" -> Prim2 (Dat, ANone, parse_arg e)
    | `Atom "NOP" -> Prim2 (Nop, parse_arg e, ANone)
    | `Atom "JMP" -> Prim2 (Jmp, parse_arg e, ANone)
    | `Atom "SPL" -> Prim2 (Spl, parse_arg e, ANone)
    | `Atom "repeat" -> Flow (Repeat, parse_exp e)
    | _ -> raise (CTError (sprintf "Not a valid unary expr: %s" (to_string sexp))) )
  | `List [eop; e1; e2] ->
    (match eop with 
    | `Atom "DAT" -> Prim2 (Dat, parse_arg e1, parse_arg e2)
    | `Atom "NOP" -> Prim2 (Nop, parse_arg e1, parse_arg e2)
    | `Atom "JMP" -> Prim2 (Jmp, parse_arg e1, parse_arg e2)
    | `Atom "SPL" -> Prim2 (Spl, parse_arg e1, parse_arg e2)
    | `Atom "MOV" -> Prim2m (Mov, MNone, parse_arg e1, parse_arg e2)
    | `Atom "ADD" -> Prim2m (Add, MNone, parse_arg e1, parse_arg e2)
    | `Atom "SUB" -> Prim2m (Sub, MNone, parse_arg e1, parse_arg e2)
    | `Atom "MUL" -> Prim2m (Mul, MNone, parse_arg e1, parse_arg e2)
    | `Atom "DIV" -> Prim2m (Div, MNone, parse_arg e1, parse_arg e2)
    | `Atom "MOD" -> Prim2m (Mod, MNone, parse_arg e1, parse_arg e2)
    | `Atom "JMZ" -> Prim2m (Jmz, MNone, parse_arg e1, parse_arg e2)
    | `Atom "JMN" -> Prim2m (Jmn, MNone, parse_arg e1, parse_arg e2)
    | `Atom "DJN" -> Prim2m (Djn, MNone, parse_arg e1, parse_arg e2)
    | `Atom "SEQ" -> Prim2m (Seq, MNone, parse_arg e1, parse_arg e2)
    | `Atom "SNE" -> Prim2m (Sne, MNone, parse_arg e1, parse_arg e2)
    | `Atom "SLT" -> Prim2m (Slt, MNone, parse_arg e1, parse_arg e2)
    | `Atom "STP" -> Prim2m (Stp, MNone, parse_arg e1, parse_arg e2)
    | `Atom "LDP" -> Prim2m (Ldp, MNone, parse_arg e1, parse_arg e2)
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
    | `Atom "MOV" -> Prim2m (Mov, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "ADD" -> Prim2m (Add, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "SUB" -> Prim2m (Sub, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "MUL" -> Prim2m (Mul, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "DIV" -> Prim2m (Div, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "MOD" -> Prim2m (Mod, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "JMZ" -> Prim2m (Jmz, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "JMN" -> Prim2m (Jmn, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "DJN" -> Prim2m (Djn, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "SEQ" -> Prim2m (Seq, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "SNE" -> Prim2m (Sne, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "SLT" -> Prim2m (Slt, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "STP" -> Prim2m (Stp, parse_imod e1, parse_arg e2, parse_arg e3)
    | `Atom "LDP" -> Prim2m (Ldp, parse_imod e1, parse_arg e2, parse_arg e3)
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
