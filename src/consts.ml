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
          | None -> if List.for_all (fun s -> is_const s || List.mem s pmars_predefined) (names x) then MImm else MDir in
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

(* The value of an expression of numbers and constants; None when it names a label or a symbol whose
   value only pMARS knows. *)
let rec value (consts : (string * Red.rexpr) list) (x : Red.rexpr) : int option =
  match x with
  | Red.XNum n -> Some n
  | Red.XName s -> Option.bind (List.assoc_opt s consts) (value consts)
  | Red.XBin (op, a, b) ->
    (match value consts a, value consts b with
    | Some a, Some b ->
      (match op with
      | '+' -> Some (a + b) | '-' -> Some (a - b) | '*' -> Some (a * b)
      | '/' -> if b = 0 then None else Some (a / b)
      | '%' -> if b = 0 then None else Some (a mod b)
      | _ -> None)
    | (Some _ | None), (Some _ | None) -> None)

(* pMARS rejects a warrior that divides by zero; a divisor known here to be 0 is a compile error at
   the node that holds it. A divisor naming a label is evaluated only by Layout. *)
let rec divides_by_zero (consts : (string * Red.rexpr) list) (x : Red.rexpr) : bool =
  match x with
  | Red.XNum _ | Red.XName _ -> false
  | Red.XBin (op, a, b) ->
    ((op = '/' || op = '%') && value consts b = Some 0) || divides_by_zero consts a || divides_by_zero consts b

let check_divisions (consts : (string * Red.rexpr) list) (e : expr) : unit =
  let zero = divides_by_zero consts in
  let arg loc (a : arg) = match a with
    | AExp (_, x) when zero x -> raise (Error (Some loc, "a division by zero in an expression: pMARS rejects the warrior"))
    | AExp _ | ANone | ANum _ | AId _ | ARef _ | ALab _ | AStore _ -> () in
  let cond loc (c : cond) = match c with
    | Cond0 -> ()
    | Cond1 (_, _, a) -> arg loc a
    | Cond2 (_, _, a1, a2) -> arg loc a1 ; arg loc a2 in
  let rec go (e : expr) = match e with
    | EComment _ | EExpect _ | ELabel _ -> ()
    | EPrim2 (_, _, a1, a2, loc) -> arg loc a1 ; arg loc a2
    | EFlow1 (op, c, b, loc) ->
      cond loc c ; go b ; (match op with Repeat a -> arg loc a | If | While | DoWhile -> ())
    | EFlow2 (_, c, b1, b2, loc) -> cond loc c ; go b1 ; go b2
    | ELet (_, a, b, loc) -> arg loc a ; go b
    | ESeq (es, _) -> List.iter go es in
  go e
