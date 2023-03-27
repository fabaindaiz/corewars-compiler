(** Compile **)
open Printf
open Red
open Ast
open Lib

exception CTError of string


let gensym =
  let a_counter = ref 0 in
  (fun basename ->
    a_counter := !a_counter + 1;
    sprintf "%s%d" basename !a_counter);;

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
    | None ->  RLab ((compile_mode Dir act), s) )
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

  let compile_precond (instrs : instruction list) (cond : cond) (label : string ) (aenv: aenv) : instruction list =
    match cond with
    | Cjz (e) -> [IJMN ("", MB, RLab(MDir, label), (compile_aarg e aenv))] @ instrs
    | Cjn (e) -> [IJMZ ("", MB, RLab(MDir, label), (compile_aarg e aenv))] @ instrs
    | Cdn (_) -> raise (CTError (sprintf "DN cond is not available on precondition"))
    | Ceq (e1, e2) -> [ISEQ ("", MB, (compile_aarg e1 aenv), (compile_aarg e2 aenv)) ; IJMP ("", RLab(MDir, label))] @ instrs
    | Cne (e1, e2) -> [ISNE ("", MB, (compile_aarg e1 aenv), (compile_aarg e2 aenv)) ; IJMP ("", RLab(MDir, label))] @ instrs
    | Cgt (e1, e2) -> [ISLT ("", MB, (compile_aarg e2 aenv), (compile_aarg e1 aenv)) ; IJMP ("", RLab(MDir, label))] @ instrs
    | Clt (e1, e2) -> [ISLT ("", MB, (compile_aarg e1 aenv), (compile_aarg e2 aenv)) ; IJMP ("", RLab(MDir, label))] @ instrs

let compile_postcond (cond : cond) (label : string ) (aenv: aenv) : instruction list =
  match cond with
  | Cjz (e) -> [IJMZ ("", MB, RLab(MDir, label), (compile_aarg e aenv))]
  | Cjn (e) -> [IJMN ("", MB, RLab(MDir, label), (compile_aarg e aenv))]
  | Cdn (e) -> [IDJN ("", MB, RLab(MDir, label), (compile_aarg e aenv))]
  | Ceq (e1, e2) -> [ISNE ("", MB, (compile_aarg e1 aenv), (compile_aarg e2 aenv)) ; IJMP ("", RLab(MDir, label))]
  | Cne (e1, e2) -> [ISEQ ("", MB, (compile_aarg e1 aenv), (compile_aarg e2 aenv)) ; IJMP ("", RLab(MDir, label))]
  | Cgt (e1, e2) -> [ISLT ("", MB, (compile_aarg e1 aenv), (compile_aarg e2 aenv)) ; IJMP ("", RLab(MDir, label))]
  | Clt (e1, e2) -> [ISLT ("", MB, (compile_aarg e2 aenv), (compile_aarg e1 aenv)) ; IJMP ("", RLab(MDir, label))]

let rec compile_expr (e : expr) (aenv : aenv) : instruction list =
  match e with
  | Mov (e1, e2) -> 
    let a1 = (compile_aarg e1 aenv) in
    let a2 = (compile_aarg e2 aenv) in
    [IMOV ("", MI, a1, a2)]
  | Sub (e1, e2) -> 
      let a1 = (compile_aarg e1 aenv) in
      let a2 = (compile_aarg e2 aenv) in
      [ISUB ("", MAB, a1, a2)]
  | Let (id, e, body) ->
    let label = (gensym "l") in
    let aenv' = (extend_aenv id label aenv) in
    (compile_expr body aenv') @ [IDAT (label, RNum (0), (compile_aarg e aenv))]
  | Repeat (e) ->
    let start = (gensym "l") in
    [ILabel (start)] @ (compile_expr e aenv) @ [IJMP ("", RLab(MDir, start))]
  | Seq (exprs) ->
    List.fold_left (fun res e -> res @ (compile_expr e aenv)) [] exprs
  | If (c, e) ->
    let fin = (gensym "l") in
    (compile_precond (compile_expr e aenv) c fin aenv) @ [ILabel (fin)]
  | While (c, e) ->
    let start = (gensym "l") in
    let fin = (gensym "l") in
    [ILabel (start)] @ (compile_precond (compile_expr e aenv) c fin aenv) @ [IJMP ("", RLab(MDir, start))] @ [ILabel (fin)]
  | Dowhile (c, e) ->
    let start = (gensym "l") in
    [ILabel (start)] @ (compile_expr e aenv) @ (compile_postcond c start aenv)


let prelude = sprintf "
;redcode
"

let last_instrs : instruction list = 
  [INOP ("finish")]

let compile_prog (e : expr) : string =
  let instrs = (compile_expr e empty_aenv) in
  (prelude) ^ (pp_instrs instrs) ^ (pp_instrs last_instrs)
