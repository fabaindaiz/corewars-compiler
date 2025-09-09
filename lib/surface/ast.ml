open Printf
open Common.Type

type surf =
  | Var of string
  | Const of const
  | Let of string * surf * surf
  | Abs of string * ttype * surf
  | App of surf * surf
  | Seq of surf list

let rec string_of_surf =
  function
  | Var x -> x
  | Const k -> string_of_const k
  | Let (x, e, b) -> sprintf "let %s in %s = %s" x (string_of_surf e) (string_of_surf b)
  | Abs (x, a, n) -> sprintf "(λ %s:%s.%s)" x (string_of_ttype a) (string_of_surf n)
  | App (l, m) -> sprintf "(%s %s)" (string_of_surf l) (string_of_surf m)
  | Seq (exprs) -> List.fold_left (fun acc m -> sprintf "%s\n%s" acc (string_of_surf m)) "" exprs