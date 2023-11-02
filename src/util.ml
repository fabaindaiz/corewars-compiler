(** Util **)
open Printf
open Red
open Ast
open Lib

exception CTError of string


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

let arg_to_darg (arg : arg) : darg =
  match arg with
  | ANone -> ADRef (MImm, 0)
  | ANum (n) -> ADRef (MImm, n)
  | AId (s) -> ADLab (MDir, s)
  | ARef (m, n) -> ADRef (m, n)
  | ALab (m, s) -> ADLab (m, s)
  | AStore (s) -> raise (CTError (sprintf "Not a valid place to store: %s" s))


type carg =
| ACRef of mode * int    (* number variable *)
| ACLab of mode * string (* label variable *)
| ACVar of mode * string (* direct reference *)
| ACPnt of mode * string (* indirect reference *)

let darg_to_carg (darg : darg) (env : env) : carg =
  let _, _, lenv = env in
  match darg with
  | ADRef (m, n) -> ACRef (m, n)
  | ADLab (m, s) ->
    (match List.assoc_opt s lenv with
    | Some _ ->
      (match m with
      | MImm | MDir -> ACVar (m, s)
      | MInd (_) -> ACPnt (m, s) )
    | None -> ACLab (m, s) )

let carg_to_rarg (carg : carg) (env : env) : rarg =
  let _, penv, lenv = env in
  match carg with
  | ACRef (m, n) -> RRef ((compile_mode m PB), n)
  | ACLab (m, s) -> RLab ((compile_mode m PB), s)
  | ACVar (m, s) ->
    let l = (translate_lenv s lenv) in
    RLab ((compile_mode m PB), l)
  | ACPnt (m, s) ->
    let p = (translate_penv s penv) in
    let l = (translate_lenv s lenv) in
    RLab ((compile_mode m p), l)


type opmod =
| TNum
| TRef
| TA
| TB

let place_to_opmod (place : place) : opmod =
  match place with
  | PA -> TA
  | PB -> TB

let carg_to_opmod (carg : carg) (env : env) : opmod =
  let aenv, penv, _ = env in
  match carg with
  | ACRef (m, _) | ACLab (m, _) ->
    (match m with
    | MImm -> TNum
    | MDir | MInd (_) -> TRef )
  | ACVar (m, s) ->
    (match m with
    | MImm | MDir ->
      let p = (translate_penv s penv) in
      (place_to_opmod p)
    | MInd (_) -> raise (CTError ("please report this bug, this error should not happen")) )
  | ACPnt (m, s) ->
    (match m with
    | MInd (_) ->
      let arg = (translate_aenv s aenv) in
      let darg = (arg_to_darg arg) in
      (match darg with
      | ADRef (_, _) ->
        (match List.assoc_opt s penv with
        | Some p -> (place_to_opmod p)
        | None -> TB )
      | ADLab (_, s) -> 
        (match List.assoc_opt s penv with
        | Some p -> (place_to_opmod p)
        | None -> TB ))
    | MImm | MDir -> raise (CTError ("please report this bug, this error should not happen")) )

let opmod_to_rmod (mod1 : opmod) (mod2 : opmod) (rmod : rmod) : rmod =
  match mod1, mod2 with
  | TNum, TA -> RA
  | TNum, TB -> RAB
  | TA, TNum -> RAB
  | TB, TNum -> RB
  | TA, TA -> RA
  | TA, TB -> RAB
  | TB, TA -> RBA
  | TB, TB -> RB
  | _, _ -> rmod
