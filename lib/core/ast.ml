open Printf
open Common.Type

type core =
  | Var of string
  | Const of const
  | Let of string * core * core
  | Seq of core list

let rec string_of_core =
  function
  | Var x -> x
  | Const k -> string_of_const k
  | Let (x, e, b) -> sprintf "let %s = %s in %s" x (string_of_core e) (string_of_core b)
  | Seq (exprs) -> List.fold_left (fun acc m -> sprintf "%s\n%s" acc (string_of_core m)) "" exprs