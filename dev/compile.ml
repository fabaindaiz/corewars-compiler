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


let compile_mode (mode: mode) : rmode =
  match mode with
  | ADir -> RDir
  | AInd -> RBInd
  | ADec -> RBPre
  | AInc -> RBPos

let compile_arg (arg : arg) (aenv : aenv) : rarg =
  match arg with
  | Num (n) -> RNum (n)
  | Id (s) ->
    (match List.assoc_opt s aenv with
    | Some arg -> RLab ((compile_mode ADir), arg)
    | None -> RLab ((compile_mode ADir), s) )
  | Ref (m, n) -> RRef ((compile_mode m), n)
  | Lab (m, s) ->
    (match List.assoc_opt s aenv with
    | Some arg -> RLab ((compile_mode m), arg)
    | None ->  RLab ((compile_mode m), s) )

let compile_precond (instrs : instruction list) (cond : cond) (label : string ) (aenv: aenv) : instruction list =
  match cond with
  | Cjz (e) -> [IJMN (RB, RLab(RDir, label), (compile_arg e aenv))] @ instrs
  | Cjn (e) -> [IJMZ (RB, RLab(RDir, label), (compile_arg e aenv))] @ instrs
  | Cdn (_) -> raise (CTError (sprintf "DN cond is not available on precondition"))
  | Ceq (e1, e2) -> [ISEQ (RB, (compile_arg e1 aenv), (compile_arg e2 aenv)) ; IJMP (RLab(RDir, label))] @ instrs
  | Cne (e1, e2) -> [ISNE (RB, (compile_arg e1 aenv), (compile_arg e2 aenv)) ; IJMP (RLab(RDir, label))] @ instrs
  | Cgt (e1, e2) -> [ISLT (RB, (compile_arg e2 aenv), (compile_arg e1 aenv)) ; IJMP (RLab(RDir, label))] @ instrs
  | Clt (e1, e2) -> [ISLT (RB, (compile_arg e1 aenv), (compile_arg e2 aenv)) ; IJMP (RLab(RDir, label))] @ instrs

let compile_postcond (cond : cond) (label : string ) (aenv: aenv) : instruction list =
  match cond with
  | Cjz (e) -> [IJMZ (RB, RLab(RDir, label), (compile_arg e aenv))]
  | Cjn (e) -> [IJMN (RB, RLab(RDir, label), (compile_arg e aenv))]
  | Cdn (e) -> [IDJN (RB, RLab(RDir, label), (compile_arg e aenv))]
  | Ceq (e1, e2) -> [ISNE (RB, (compile_arg e1 aenv), (compile_arg e2 aenv)) ; IJMP (RLab(RDir, label))]
  | Cne (e1, e2) -> [ISEQ (RB, (compile_arg e1 aenv), (compile_arg e2 aenv)) ; IJMP (RLab(RDir, label))]
  | Cgt (e1, e2) -> [ISLT (RB, (compile_arg e1 aenv), (compile_arg e2 aenv)) ; IJMP (RLab(RDir, label))]
  | Clt (e1, e2) -> [ISLT (RB, (compile_arg e2 aenv), (compile_arg e1 aenv)) ; IJMP (RLab(RDir, label))]

let rec compile_expr (e : expr) (aenv : aenv) : instruction list =
  match e with
  | Label (l, e) -> [ILabel (l)] @ (compile_expr e aenv)
  | Mov (e1, e2) ->
    [IMOV (RAB, (compile_arg e1 aenv), (compile_arg e2 aenv))]
  | Add (e1, e2) ->
    [IADD (RAB, (compile_arg e1 aenv), (compile_arg e2 aenv))]
  | Sub (e1, e2) ->
    [ISUB (RAB, (compile_arg e1 aenv), (compile_arg e2 aenv))]
  | Mul (e1, e2) ->
    [IMUL (RAB, (compile_arg e1 aenv), (compile_arg e2 aenv))]
  | Div (e1, e2) ->
    [IDIV (RAB, (compile_arg e1 aenv), (compile_arg e2 aenv))]
  | Mod (e1, e2) ->
    [IMOD (RAB, (compile_arg e1 aenv), (compile_arg e2 aenv))]
  | Spl (e) ->
    [ISPL ((compile_arg e aenv))]
  | Nop -> [INOP]
  | Let (id, e, body) ->
    let label = (gensym "l") in
    let aenv' = (extend_aenv id label aenv) in
    (compile_expr body aenv') @ [ILabel (label) ; IDAT (RNum (0), (compile_arg e aenv))]
  | Seq (exprs) ->
    List.fold_left (fun res e -> res @ (compile_expr e aenv)) [] exprs
  | Repeat (e) ->
    let ini = (gensym "l") in
    [ILabel (ini)] @ (compile_expr e aenv) @ [IJMP (RLab(RDir, ini))]
  | If (c, e) ->
    let fin = (gensym "l") in
    (compile_precond (compile_expr e aenv) c fin aenv) @ [ILabel (fin)]
  | While (c, e) ->
    let ini = (gensym "l") in
    let fin = (gensym "l") in
    [ILabel (ini)] @ (compile_precond (compile_expr e aenv) c fin aenv) @ [IJMP (RLab(RDir, ini))] @ [ILabel (fin)]
  | Dowhile (c, e) ->
    let ini = (gensym "l") in
    [ILabel (ini)] @ (compile_expr e aenv) @ (compile_postcond c ini aenv)


let prelude = sprintf "
;redcode-94b
"

let compile_prog (e : expr) : string =
  let instrs = (compile_expr e empty_aenv) in
  (prelude) ^ (pp_instrs instrs)
