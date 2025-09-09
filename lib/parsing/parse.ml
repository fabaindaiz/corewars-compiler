open Common.Type
open Surface.Ast
open Printf
open CCSexp

exception ParseError of string

let parse_const (s : string) : const option =
  match s with
  | "unit" -> Some Unit
  | "true" -> Some (Bool true)
  | "false" -> Some (Bool false)
  | s ->
    (match Int32.of_string_opt s with
    | Some n -> Some (Int (Int32.to_int n))
    | None -> None)

let rec parse_ttype (sexp : sexp) : ttype =
  match sexp with
  | `Atom "Unit" -> TBase TUnit
  | `Atom "Bool" -> TBase TBool
  | `Atom "Int" -> TBase TInt
  | `List [t1; `Atom "->"; t2] -> TArrow (parse_ttype t1, parse_ttype t2)
  | _ -> raise (ParseError (sprintf "Not a valid ttype %s" (to_string sexp)))

let rec parse_surface (sexp : sexp) : surf =
  match sexp with
  | `Atom s ->
    (match parse_const s with
    | Some k -> Const k
    | None -> Var s)
    | `List [`Atom "let"; `List [x; a]; n] -> Let (parse_id x, parse_surface a, parse_surface n)
    | `List [`Atom "lam"; `List [x; a]; n] -> Abs (parse_id x, parse_ttype a, parse_surface n)
    | `List (`Atom "seq" :: exps) -> Seq (List.map parse_surface exps)
  | `List [eop; _; _] ->
    (match eop with
    | _ -> raise (ParseError (sprintf "Not a valid binary term : %s" (to_string sexp))))
    | `List [l; m] -> App (parse_surface l, parse_surface m)
    | _ -> raise (ParseError (sprintf "Not a valid term: %s" (to_string sexp)))

and parse_id (sexp : sexp) : string =
  match sexp with
  | `Atom name -> name
  | _ -> raise (ParseError (sprintf "Not a valid name: %s" (to_string sexp)))

let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> raise (ParseError (sprintf "Unable to parse file %s: %s" filename msg))

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> raise (ParseError (sprintf "Unable to parse string %s: %s" src msg))