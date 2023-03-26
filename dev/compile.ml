(** Compile **)
open Printf
open Red
open Ast
open Lib


let compile_mode (mode: mode) (act : act) : rmode =
  match mode with
  | Dir -> MDir
  | Ind ->
    (match act with
    | ANot  -> BInd
    | ADec -> BPre
    | AInc -> BPos)

let compile_arg (arg : arg) (act : act) (aenv : aenv) : rarg =
  match arg with
  | Num (n) -> RNum (n)
  | Id (s) ->
    (match List.assoc_opt s aenv with
    | Some arg -> RLab ((compile_mode Ind act), arg)
    | None ->  RId (s) )
  | Ref (m, n) -> RRef ((compile_mode m act), n)
  | Lab (m, s) ->
    (match List.assoc_opt s aenv with
    | Some arg -> RLab ((compile_mode m act), arg)
    | None ->  RLab ((compile_mode m act), s) )

let compile_aarg (aarg : aarg) (aenv : aenv) : rarg =
  match aarg with
  | Not (e) -> (compile_arg e ANot aenv)
  | Dec (e) -> (compile_arg e ADec aenv)
  | Inc (e) -> (compile_arg e AInc aenv)

let rec compile_expr (e : expr) (aenv : aenv) : instruction list =
  match e with
  | Mov (e1, e2) -> 
    let a1 = (compile_aarg e1 aenv) in
    let a2 = (compile_aarg e2 aenv) in
    [IMOV ("", MI, a1, a2)]
  | Let (id, e, body) ->
    let label = "label1" in
    let aenv' = (extend_aenv id label aenv) in
    (compile_expr body aenv') @ [IDAT (label, (compile_aarg e aenv), (compile_aarg e aenv))]
  | Repeat (e) ->
    let label = "label2" in
    [ILabel (label)] @ (compile_expr e aenv) @ [IJMP ("", RLab(MDir, label))]

let prelude = sprintf "
;redcode
"

let last_instrs : instruction list = 
  [INOP ("finish")]

let compile_prog (e : expr) : string =
  let instrs = (compile_expr e empty_aenv) in
  (prelude) ^ (pp_instrs instrs) ^ (pp_instrs last_instrs)