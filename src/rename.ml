(** Rename: every let-bound variable gets a name no other binder uses **)
open Printf
open Ast

(* A let's initializer is pasted where its (store x) sits, and was resolved there: an inner let of
   the same name captured it. Renaming each binder apart (the "uniquify" pass of Essentials of
   Compilation) resolves every name where its let binds it. Variable names never reach the output
   (labels come from tags), and the tree keeps its shape, so tags and goldens are unchanged. *)

type renames = (string * string) list

let rename_name (env : renames) (s : string) : string =
  match List.assoc_opt s env with Some s' -> s' | None -> s

let rename_arg (env : renames) (a : arg) : arg =
  match a with
  | AStore s -> AStore (rename_name env s)
  | AId s -> AId (rename_name env s)
  | ALab (m, s) -> ALab (m, rename_name env s)
  | ANone | ANum _ | ARef _ -> a

let rename_cond (env : renames) (c : cond) : cond =
  match c with
  | Cond0 -> c
  | Cond1 (op, a) -> Cond1 (op, rename_arg env a)
  | Cond2 (op, a1, a2) -> Cond2 (op, rename_arg env a1, rename_arg env a2)

(* The first binder of a name keeps it; later ones become x#1, x#2, ... *)
let uniquify (e : expr) : expr =
  let used = Hashtbl.create 8 in
  let fresh x =
    let n = Option.value (Hashtbl.find_opt used x) ~default:0 in
    Hashtbl.replace used x (n + 1) ;
    if n = 0 then x else sprintf "%s#%d" x n in
  let rec go env e = match e with
    | Comment _ | Label _ | Expect _ -> e
    | Prim2 (op, m, a1, a2) -> Prim2 (op, m, rename_arg env a1, rename_arg env a2)
    | Flow1 (op, c, body) -> Flow1 (op, rename_cond env c, go env body)
    | Flow2 (op, c, b1, b2) -> Flow2 (op, rename_cond env c, go env b1, go env b2)
    | Let (x, a, body) ->
      let a' = rename_arg env a in
      let x' = fresh x in
      Let (x', a', go ((x, x') :: env) body)
    | Seq es -> Seq (List.map (go env) es) in
  go [] e
