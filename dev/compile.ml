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

let rec compile_arg (arg : arg) (env : env) : rarg =
  let aenv, _, lenv = env in
  match arg with
  | ANone -> RNone
  | Num (n) -> RNum (n)
  | Id (s) ->
    (match List.assoc_opt s lenv with
    | Some l -> RLab ((compile_mode ADir), l)
    | None -> RLab ((compile_mode ADir), s) )
  | Lab (m, s) ->
    (match List.assoc_opt s lenv with
    | Some arg -> RLab ((compile_mode m), arg)
    | None ->  RLab ((compile_mode m), s) )
  | Ref (m, n) -> RRef ((compile_mode m), n)
  | Place (s) ->
    let arg = (translate_aenv s aenv) in
    (compile_arg arg env)

let compile_label (args : arg list) (lenv : lenv) : instruction list =
  let compile_label_aux (arg : arg) (lenv: lenv) : instruction list =
    match arg with
    | Place (s) ->
      (match List.assoc_opt s lenv with
      | Some l -> [ILabel (l)]
      | None -> failwith (sprintf "unbound variable %s in lenv" s) )
    | _ -> [] in
  List.fold_left (fun res i -> res @ (compile_label_aux i lenv)) [] args

let compile_mod_mov (arg1 : arg) (arg2 : arg) (env : env) : rmod =
  RI

let compile_mod_sum (arg1 : arg) (arg2 : arg) (env : env) : rmod =
  RAB

let compile_mod_jmp (arg1 : arg) (arg2 : arg) (env : env) : rmod =
  RB

let compile_precond (instrs : instruction list) (cond : cond) (label : string ) (env : env) : instruction list =
  match cond with
  | Cjz (e) -> [IJMN (RB, RLab(RDir, label), (compile_arg e env))] @ instrs
  | Cjn (e) -> [IJMZ (RB, RLab(RDir, label), (compile_arg e env))] @ instrs
  | Cdn (_) -> raise (CTError (sprintf "DN cond is not available on precondition"))
  | Ceq (e1, e2) -> [ISEQ (RB, (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))] @ instrs
  | Cne (e1, e2) -> [ISNE (RB, (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))] @ instrs
  | Cgt (e1, e2) -> [ISLT (RB, (compile_arg e2 env), (compile_arg e1 env)) ; IJMP (RB, RNone, RLab(RDir, label))] @ instrs
  | Clt (e1, e2) -> [ISLT (RB, (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))] @ instrs

let compile_postcond (cond : cond) (label : string ) (env : env) : instruction list =
  match cond with
  | Cjz (e) -> [IJMZ (RB, RLab(RDir, label), (compile_arg e env))]
  | Cjn (e) -> [IJMN (RB, RLab(RDir, label), (compile_arg e env))]
  | Cdn (e) -> [IDJN (RB, RLab(RDir, label), (compile_arg e env))]
  | Ceq (e1, e2) -> [ISNE (RB, (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))]
  | Cne (e1, e2) -> [ISEQ (RB, (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))]
  | Cgt (e1, e2) -> [ISLT (RB, (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))]
  | Clt (e1, e2) -> [ISLT (RB, (compile_arg e2 env), (compile_arg e1 env)) ; IJMP (RB, RNone, RLab(RDir, label))]

let rec compile_expr (e : expr) (env : env) : instruction list =
  let aenv, penv, lenv = env in
  match e with
  | Label (l) -> [ILabel (l)]
  | Prim2 (op, e1, e2) ->
    (compile_label [e1; e2] lenv) @ 
    (match op with
    | Dat -> [IDAT ((compile_arg e1 env), (compile_arg e2 env))]
    | Mov -> [IMOV ((compile_mod_mov e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Add -> [IADD ((compile_mod_sum e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Sub -> [ISUB ((compile_mod_sum e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Mul -> [IMUL ((compile_mod_sum e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Div -> [IDIV ((compile_mod_sum e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Mod -> [IMOD ((compile_mod_sum e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Jmp -> [IJMP ((compile_mod_jmp e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Spl -> [ISPL ((compile_mod_jmp e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))] )
  | Nop -> [INOP]
  | Let (id, place, arg, body) ->
    let label = (gensym "L") in
    let aenv' = (extend_aenv id arg aenv) in
    let penv' = (extend_penv id place penv) in
    let lenv' = (extend_lenv id label lenv) in
    let env' = (aenv', penv', lenv') in
    (compile_expr body env')
  | Seq (exprs) ->
    List.fold_left (fun res e -> res @ (compile_expr e env)) [] exprs
  | Repeat (e) ->
    let ini = (gensym "L") in
    [ILabel (ini)] @ (compile_expr e env) @ [IJMP (RB, RNone, RLab(RDir, ini))]
  | If (c, e) ->
    let fin = (gensym "L") in
    (compile_precond (compile_expr e env) c fin env) @ [ILabel (fin)]
  | While (c, e) ->
    let ini = (gensym "L") in
    let fin = (gensym "L") in
    [ILabel (ini)] @ (compile_precond (compile_expr e env) c fin env) @ [IJMP (RB, RNone, RLab(RDir, ini))] @ [ILabel (fin)]
  | Dowhile (c, e) ->
    let ini = (gensym "L") in
    [ILabel (ini)] @ (compile_expr e env) @ (compile_postcond c ini env)


let prelude = sprintf "
;redcode-94b
"

let compile_prog (e : expr) : string =
  let instrs = (compile_expr e empty_env) @ [IDAT (RNum 0, RNum 0)] in
  (prelude) ^ (pp_instrs instrs)
