(** Consts: EQU constants and operand expressions, resolved before renaming **)
open Printf
open Ast

let rec names (x : Red.rexpr) : string list =
  match x with
  | Red.XNum _ -> []
  | Red.XName s -> [s]
  | Red.XBin (_, a, b) -> names a @ names b

(* A constant used as an operand is a number (immediate, unless a mode is written). An expression
   without a written mode is immediate when every name in it is a constant and direct when one is a
   label, as a number and a label are. A let variable is a field of a cell, not a number pMARS knows
   when assembling: it cannot be part of an expression, and no let or label may take a constant's
   name. *)
let resolve (consts : string list) (e : expr) : expr =
  let is_const s = List.mem s consts in
  let constant loc s = raise (Error (Some loc, sprintf "`%s` is a constant (const %s ...)" s s)) in
  let arg vars loc (a : arg) : arg =
    match a with
    | AId s when is_const s -> AExp (Some MImm, Red.XName s)
    | ALab (m, s) when is_const s -> AExp (Some m, Red.XName s)
    | AStore s when is_const s -> constant loc s
    | AExp (m, x) ->
      (match List.find_opt (fun s -> List.mem s vars) (names x) with
      | Some v -> raise (Error (Some loc, sprintf "variable `%s` cannot be part of an expression: it is a cell's field, not a number" v))
      | None ->
        let m = match m with
          | Some m -> m
          | None -> if List.for_all is_const (names x) then MImm else MDir in
        AExp (Some m, x))
    | ANone | ANum _ | AId _ | ARef _ | ALab _ | AStore _ -> a in
  let cond vars loc (c : cond) : cond =
    match c with
    | Cond0 -> c
    | Cond1 (op, m, a) -> Cond1 (op, m, arg vars loc a)
    | Cond2 (op, m, a1, a2) -> Cond2 (op, m, arg vars loc a1, arg vars loc a2) in
  let rec go vars (e : expr) : expr =
    match e with
    | EComment _ | EExpect _ -> e
    | ELabel (l, loc) -> if is_const l then constant loc l else e
    | EPrim2 (op, m, a1, a2, loc) -> EPrim2 (op, m, arg vars loc a1, arg vars loc a2, loc)
    | EFlow1 (op, c, b, loc) ->
      let op = match op with Repeat a -> Repeat (arg vars loc a) | If | While | DoWhile -> op in
      EFlow1 (op, cond vars loc c, go vars b, loc)
    | EFlow2 (op, c, b1, b2, loc) -> EFlow2 (op, cond vars loc c, go vars b1, go vars b2, loc)
    | ELet (x, a, b, loc) ->
      if is_const x then constant loc x ;
      ELet (x, arg vars loc a, go (x :: vars) b, loc)
    | ESeq (es, loc) -> ESeq (List.map (go vars) es, loc) in
  go [] e
