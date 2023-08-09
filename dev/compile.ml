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
  | APlace -> raise (CTError ("Not a valid red mode: Place"))

let rec compile_arg (arg : arg) (aenv : aenv) (lenv : lenv) : rarg =
  match arg with
  | Num (n) -> RNum (n)
  | Id (s) ->
    (match List.assoc_opt s lenv with
    | Some l -> RLab ((compile_mode ADir), l)
    | None -> RLab ((compile_mode ADir), s) )
  | Ref (m, n) -> RRef ((compile_mode m), n)
  | Lab (m, s) ->
    (match m with
    | APlace ->
      (match List.assoc_opt s aenv with
      | Some arg -> (compile_arg arg aenv lenv)
      | None -> failwith (sprintf "unbound variable %s in aenv" s) )
    | _ ->
      (match List.assoc_opt s lenv with
      | Some arg -> RLab ((compile_mode m), arg)
      | None ->  RLab ((compile_mode m), s) ))

let compile_precond (instrs : instruction list) (cond : cond) (label : string ) (aenv : aenv) (lenv : lenv) : instruction list =
  match cond with
  | Cjz (e) -> [IJMN (RB, RLab(RDir, label), (compile_arg e aenv lenv))] @ instrs
  | Cjn (e) -> [IJMZ (RB, RLab(RDir, label), (compile_arg e aenv lenv))] @ instrs
  | Cdn (_) -> raise (CTError (sprintf "DN cond is not available on precondition"))
  | Ceq (e1, e2) -> [ISEQ (RB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv)) ; IJMP (RLab(RDir, label))] @ instrs
  | Cne (e1, e2) -> [ISNE (RB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv)) ; IJMP (RLab(RDir, label))] @ instrs
  | Cgt (e1, e2) -> [ISLT (RB, (compile_arg e2 aenv lenv), (compile_arg e1 aenv lenv)) ; IJMP (RLab(RDir, label))] @ instrs
  | Clt (e1, e2) -> [ISLT (RB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv)) ; IJMP (RLab(RDir, label))] @ instrs

let compile_postcond (cond : cond) (label : string ) (aenv: aenv) (lenv : lenv) : instruction list =
  match cond with
  | Cjz (e) -> [IJMZ (RB, RLab(RDir, label), (compile_arg e aenv lenv))]
  | Cjn (e) -> [IJMN (RB, RLab(RDir, label), (compile_arg e aenv lenv))]
  | Cdn (e) -> [IDJN (RB, RLab(RDir, label), (compile_arg e aenv lenv))]
  | Ceq (e1, e2) -> [ISNE (RB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv)) ; IJMP (RLab(RDir, label))]
  | Cne (e1, e2) -> [ISEQ (RB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv)) ; IJMP (RLab(RDir, label))]
  | Cgt (e1, e2) -> [ISLT (RB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv)) ; IJMP (RLab(RDir, label))]
  | Clt (e1, e2) -> [ISLT (RB, (compile_arg e2 aenv lenv), (compile_arg e1 aenv lenv)) ; IJMP (RLab(RDir, label))]

let rec compile_expr (e : expr) (aenv : aenv) (lenv : lenv) : instruction list =
  match e with
  | Dat (e1, e2) -> [IDAT ((compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv))]
  | Label (l) -> [ILabel (l)]
  | Point (s) -> 
    (match List.assoc_opt s lenv with
    | Some l -> [ILabel (l)]
    | None -> failwith (sprintf "unbound variable %s in lenv" s) )
  | Mov (e1, e2) -> [IMOV (RAB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv))]
  | Add (e1, e2) -> [IADD (RAB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv))]
  | Sub (e1, e2) -> [ISUB (RAB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv))]
  | Mul (e1, e2) -> [IMUL (RAB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv))]
  | Div (e1, e2) -> [IDIV (RAB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv))]
  | Mod (e1, e2) -> [IMOD (RAB, (compile_arg e1 aenv lenv), (compile_arg e2 aenv lenv))]
  | Spl (e) -> [ISPL ((compile_arg e aenv lenv))]
  | Nop -> [INOP]
  | Let (id, arg, body) ->
    let label = (gensym "l") in
    let aenv' = (extend_aenv id arg aenv) in
    let lenv' = (extend_lenv id label lenv) in
    (compile_expr body aenv' lenv')
  | Seq (exprs) ->
    List.fold_left (fun res e -> res @ (compile_expr e aenv lenv)) [] exprs
  | Repeat (e) ->
    let ini = (gensym "l") in
    [ILabel (ini)] @ (compile_expr e aenv lenv) @ [IJMP (RLab(RDir, ini))]
  | If (c, e) ->
    let fin = (gensym "l") in
    (compile_precond (compile_expr e aenv lenv) c fin aenv lenv) @ [ILabel (fin)]
  | While (c, e) ->
    let ini = (gensym "l") in
    let fin = (gensym "l") in
    [ILabel (ini)] @ (compile_precond (compile_expr e aenv lenv) c fin aenv lenv) @ [IJMP (RLab(RDir, ini))] @ [ILabel (fin)]
  | Dowhile (c, e) ->
    let ini = (gensym "l") in
    [ILabel (ini)] @ (compile_expr e aenv lenv) @ (compile_postcond c ini aenv lenv)


let prelude = sprintf "
;redcode-94b
"

let compile_prog (e : expr) : string =
  let instrs = (compile_expr e empty_aenv empty_lenv) in
  (prelude) ^ (pp_instrs instrs)
