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
  match dest with
  | PA ->
    (match mode with
    | MIns (_) -> RDir
    | MImm -> RImm
    | MDir -> RDir
    | MInd (m) -> (imode_to_rmode m PA) )
  | PB -> 
    (match mode with
    | MIns (_) -> RDir
    | MImm -> RImm
    | MDir -> RDir
    | MInd (m) -> (imode_to_rmode m PB) )


type darg =
| ADRef of mode * int
| ADLab of mode * string

let replace_store (arg : arg) (env : env) : arg =
  let aenv, _, _ = env in
  match arg with
  | AStore (s) -> (translate_aenv s aenv)
  | _ -> arg

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
      | MIns (_) | MImm | MDir -> ACVar (m, s)
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


let compile_arg (arg : arg) (env : env) : carg * rarg =
  let arg' = (replace_store arg env) in
  let darg = (arg_to_darg arg') in
  let carg = (darg_to_carg darg env) in
  let rarg = (carg_to_rarg carg env) in
  (carg, rarg)


type opmod =
| TIns of imod
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
    | MIns (i) ->  TIns (i)
    | MImm -> TNum
    | MDir | MInd (_) -> TRef )
  | ACVar (m, s) ->
    (match m with
    | MIns (i) -> TIns (i)
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
    | MIns (_) | MImm | MDir -> raise (CTError ("please report this bug, this error should not happen")) )


let imod_to_rmod (imod : imod) : rmod =
  match imod with
  | MI -> RI
  | MX -> RX
  | MF -> RF

let compile_mod (carg1 : carg) (carg2 : carg) (def : rmod) (env : env) : rmod =
  let mod1 = (carg_to_opmod carg1 env) in
  let mod2 = (carg_to_opmod carg2 env) in
  match mod1, mod2 with
  | TIns (b), _ -> (imod_to_rmod b)
  | _, TIns (b) -> (imod_to_rmod b)
  | TNum, TA -> RA
  | TNum, TB -> RAB
  | TA, TNum -> RAB
  | TB, TNum -> RB
  | TA, TA -> RA
  | TA, TB -> RAB
  | TB, TA -> RBA
  | TB, TB -> RB
  | _, _ -> def
