(** Rename: every let-bound variable gets a name no other binder uses **)
open Printf
open Ast

(* A let's initializer is pasted where its (store x) sits, and was resolved there: an inner let of
   the same name captured it. Renaming each binder apart (the "uniquify" pass of Essentials of
   Compilation) resolves every name where its let binds it. Variable names never reach the output
   (labels come from tags), and the tree keeps its shape, so tags and goldens are unchanged. *)

type renames = (string * string) list

(* The name the user wrote, for messages: _x#1 is x. *)
let original (s : string) : string =
  if String.length s > 1 && s.[0] = '_' then
    match String.rindex_opt s '#' with
    | Some i -> String.sub s 1 (i - 1)
    | None -> s
  else s

let rename_name (env : renames) (s : string) : string =
  match List.assoc_opt s env with Some s' -> s' | None -> s

let rename_arg (env : renames) (a : arg) : arg =
  match a with
  | AStore s -> AStore (rename_name env s)
  | AId s -> AId (rename_name env s)
  | ALab (m, s) -> ALab (m, rename_name env s)
  | ANone | ANum _ | ARef _ | AExp _ -> a

let rename_cond (env : renames) (c : cond) : cond =
  match c with
  | Cond0 -> c
  | Cond1 (op, m, a) -> Cond1 (op, m, rename_arg env a)
  | Cond2 (op, m, a1, a2) -> Cond2 (op, m, rename_arg env a1, rename_arg env a2)

let rename_flow (env : renames) (op : flow1) : flow1 =
  match op with
  | Repeat a -> Repeat (rename_arg env a)
  | If | While | DoWhile -> op

let stores_in_arg (a : arg) : string list = match a with
  | AStore s -> [s]
  | ANone | ANum _ | AId _ | ARef _ | ALab _ | AExp _ -> []

let stores_in_flow (op : flow1) : string list = match op with
  | Repeat a -> stores_in_arg a
  | If | While | DoWhile -> []

let stores_in_cond (c : cond) : string list = match c with
  | Cond0 -> []
  | Cond1 (_, _, a) -> stores_in_arg a
  | Cond2 (_, _, a1, a2) -> stores_in_arg a1 @ stores_in_arg a2

(* After renaming every name is bound once, so counting stores per name checks each let: a let
   variable lives in one cell, so a second (store x) would define its label twice. *)
let check_single_stores (binders : (string * (string * loc)) list) (e : expr) : unit =
  let rec stores (e : expr) = match e with
    | EComment _ | ELabel _ | EExpect _ -> []
    | EPrim2 (_, _, a1, a2, _) -> stores_in_arg a1 @ stores_in_arg a2
    | EFlow1 (op, c, b, _) -> stores_in_flow op @ stores_in_cond c @ stores b
    | EFlow2 (_, c, b1, b2, _) -> stores_in_cond c @ stores b1 @ stores b2
    | ELet (_, a, b, _) -> stores_in_arg a @ stores b
    | ESeq (es, _) -> List.concat_map stores es in
  let all = stores e in
  List.iter (fun (unique, (original, loc)) ->
    if List.length (List.filter (( = ) unique) all) > 1 then
      raise (Error (Some loc, sprintf "variable `%s` is stored twice; a let variable lives in one cell" original)))
    (List.rev binders)

(* The first binder of a name keeps it; later ones become _x#1, _x#2, ...: user names may not start
   with "_", so a fresh name never equals one the user wrote. *)
let uniquify (e : expr) : expr =
  let used = Hashtbl.create 8 and binders = ref [] in
  let fresh x =
    let n = Option.value (Hashtbl.find_opt used x) ~default:0 in
    Hashtbl.replace used x (n + 1) ;
    if n = 0 then x else sprintf "_%s#%d" x n in
  let rec go env (e : expr) : expr = match e with
    | EComment _ | ELabel _ | EExpect _ -> e
    | EPrim2 (op, m, a1, a2, loc) -> EPrim2 (op, m, rename_arg env a1, rename_arg env a2, loc)
    | EFlow1 (op, c, body, loc) -> EFlow1 (rename_flow env op, rename_cond env c, go env body, loc)
    | EFlow2 (op, c, b1, b2, loc) -> EFlow2 (op, rename_cond env c, go env b1, go env b2, loc)
    | ELet (x, a, body, loc) ->
      let a' = rename_arg env a in
      let x' = fresh x in
      binders := (x', (x, loc)) :: !binders ;
      ELet (x', a', go ((x, x') :: env) body, loc)
    | ESeq (es, loc) -> ESeq (List.map (go env) es, loc) in
  let renamed = go [] e in
  check_single_stores !binders renamed ;
  renamed
