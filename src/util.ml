(** Util **)
open Printf
open Red
open Ast
open Lib



let imode_to_rmode (imode : imode) (place : place) : rmode =
  match imode, place with
  | MINone, PA -> RAInd
  | MINone, PB -> RBInd
  | MIDec, PA -> RADec
  | MIDec, PB -> RBDec
  | MIInc, PA -> RAInc
  | MIInc, PB -> RBInc

let compile_mode (mode : mode) (dest : place) : rmode =
  match mode with
  | MImm -> RImm
  | MDir -> RDir
  | MInd (m) -> (imode_to_rmode m dest)


type darg =
| ADRef of mode * int
| ADLab of mode * string
| ADExp of mode * rexpr

let arg_to_darg (arg : arg) : darg =
  match arg with
  | ANone -> ADRef (MImm, 0)
  | ANum (n) -> ADRef (MImm, n)
  | AId (s) -> ADLab (MDir, s)
  | ARef (m, n) -> ADRef (m, n)
  | ALab (m, s) -> ADLab (m, s)
  | AStore (s) -> error (sprintf "Not a valid place to store: %s" s)
  | AExp (Some m, e) -> ADExp (m, e)
  | AExp (None, _) -> failwith "an expression without a mode: Consts.resolve did not run"


type carg =
| ACRef of mode * int    (* number variable *)
| ACLab of mode * string (* label variable *)
| ACVar of mode * string (* direct reference *)
| ACPnt of mode * string (* indirect reference *)
| ACExp of mode * rexpr  (* expression, evaluated by pMARS *)

let darg_to_carg (darg : darg) (env : env) : carg =
  let _, _, lenv = env in
  match darg with
  | ADRef (m, n) -> ACRef (m, n)
  | ADExp (m, e) -> ACExp (m, e)
  | ADLab (m, s) ->
    (match List.assoc_opt s lenv with
    | Some _ ->
      (match m with
      | MImm | MDir -> ACVar (m, s)
      | MInd (_) -> ACPnt (m, s) )
    (* A label the user wrote must be one pMARS reads as a label; the compiler's own start with `_`,
       which user names may not. *)
    | None -> if valid_label s || String.starts_with ~prefix:"_" s then ACLab (m, s) else error (invalid_label s) )

let carg_to_rarg (carg : carg) (env : env) : rarg =
  let _, penv, lenv = env in
  match carg with
  | ACRef (m, n) -> RRef ((compile_mode m PB), n)
  | ACExp (m, e) -> RExp ((compile_mode m PB), e)
  | ACLab (m, s) -> RLab ((compile_mode m PB), s)
  | ACVar (m, s) ->
    let l = (translate_lenv s lenv) in
    RLab ((compile_mode m PB), l)
  | ACPnt (m, s) ->
    let p = (translate_penv s penv) in
    let l = (translate_lenv s lenv) in
    RLab ((compile_mode m p), l)


(* What an operand names, for the modifier: a number, a variable's field (A or B), or a cell (a plain
   reference, or the target of a pointer). *)
type opmod =
| TNum
| TCell
| TA
| TB

let place_to_opmod (place : place) : opmod =
  match place with
  | PA -> TA
  | PB -> TB

let carg_to_opmod (carg : carg) (env : env) : opmod =
  let _, penv, _ = env in
  match carg with
  | ACRef (m, _) | ACLab (m, _) | ACExp (m, _) ->
    (match m with
    | MImm -> TNum
    | MDir | MInd (_) -> TCell )
  | ACVar (m, s) ->
    (match m with
    | MImm | MDir ->
      let p = (translate_penv s penv) in
      (place_to_opmod p)
    | MInd (_) -> failwith "a direct variable reference with an indirect mode" )
  | ACPnt (m, _) ->
    (match m with
    | MInd (_) -> TCell
    | MImm | MDir -> failwith "a pointer reference with a direct mode" )

(* A variable is read through its field; a cell beside a number or a variable is its B-field, the
   ICWS'94 convention (docs/semantics.md, Uses); between two cells, or two numbers, nothing tells
   a field apart, and the opcode's ICWS'94 default (rmod) applies: the whole cell in MOV, SEQ, SNE. *)
let opmod_to_rmod (mod1 : opmod) (mod2 : opmod) (rmod : rmod) : rmod =
  match mod1, mod2 with
  | TNum, TA -> RA
  | TNum, (TB | TCell) -> RAB
  | TA, TNum -> RAB
  | (TB | TCell), TNum -> RB
  | TA, TA -> RA
  | TA, (TB | TCell) -> RAB
  | (TB | TCell), TA -> RBA
  | TB, (TB | TCell) -> RB
  | TCell, TB -> RB
  | TCell, TCell | TNum, TNum -> rmod
