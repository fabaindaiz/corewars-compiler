(** Compile **)
open Printf
open Red
open Ast
open Lib

exception CTError of string


let compile_mode (mode : mode) : rmode =
  match mode with
  | ADir -> RDir
  | AInd -> RBInd
  | ADec -> RBPre
  | AInc -> RBPos

let compile_mode_id (mode : mode) (dest : place) : rmode =
  match dest with
  | A ->
    (match mode with
    | ADir -> RDir
    | AInd -> RAInd
    | ADec -> RAPre
    | AInc -> RAPos )
  | B -> 
    (match mode with
    | ADir -> RDir
    | AInd -> RBInd
    | ADec -> RBPre
    | AInc -> RBPos )

let rec compile_arg (arg : arg) (env : env) : rarg =
  let aenv, penv, lenv = env in
  match arg with
  | ANone -> RNone
  | Num (n) -> RNum (n)
  | Id (s) ->
    (match List.assoc_opt s lenv with
    | Some l -> RLab ((compile_mode ADir), l)
    | None -> RLab ((compile_mode ADir), s) )
  | Ref (m, n) -> RRef ((compile_mode m), n)
  | Lab (m, s) ->
    (match List.assoc_opt s lenv with
    | Some arg ->
      let dest = (translate_penv s penv) in
      RLab ((compile_mode_id m dest), arg)
    | None ->  RLab ((compile_mode m), s) )
  | Place (s) ->
    let arg = (translate_aenv s aenv) in
    (match arg with
    | Id (s) ->
      (match List.assoc_opt s lenv with
      | Some label -> RLab ((compile_mode ADir), label)
      | None -> (compile_arg arg env) )
    | Lab (m, s) ->
      (match List.assoc_opt s lenv with
      | Some label ->
        let dest = (translate_penv s penv) in
        RLab ((compile_mode_id m dest), label)
      | None -> (compile_arg arg env) )
    | _ -> (compile_arg arg env) )


let compile_label (args : arg list) (lenv : lenv) : instruction list =
  let compile_label_aux (arg : arg) (lenv: lenv) : instruction list =
    match arg with
    | Place (s) ->
      (match List.assoc_opt s lenv with
      | Some l -> [ILabel (l)]
      | None -> failwith (sprintf "unbound variable %s in lenv" s) )
    | _ -> [] in
  List.fold_left (fun res i -> res @ (compile_label_aux i lenv)) [] args


let compile_precond (cond : cond) (label : string ) (env : env) : instruction list =
  match cond with
  | Cjz (e) -> [IJMN (RB, RLab(RDir, label), (compile_arg e env))]
  | Cjn (e) -> [IJMZ (RB, RLab(RDir, label), (compile_arg e env))]
  | Cdn (_) -> raise (CTError (sprintf "DN cond is not available on precondition"))
  | Ceq (e1, e2) -> [ISEQ ((compile_mod_jmp e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))]
  | Cne (e1, e2) -> [ISNE ((compile_mod_jmp e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))]
  | Cgt (e1, e2) -> [ISLT ((compile_mod_jmp e1 e2 env), (compile_arg e2 env), (compile_arg e1 env)) ; IJMP (RB, RNone, RLab(RDir, label))]
  | Clt (e1, e2) -> [ISLT ((compile_mod_jmp e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))]

let compile_postcond (cond : cond) (label : string ) (env : env) : instruction list =
  match cond with
  | Cjz (e) -> [IJMZ (RB, RLab(RDir, label), (compile_arg e env))]
  | Cjn (e) -> [IJMN (RB, RLab(RDir, label), (compile_arg e env))]
  | Cdn (e) -> [IDJN (RB, RLab(RDir, label), (compile_arg e env))]
  | Ceq (e1, e2) -> [ISNE ((compile_mod_jmp e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))]
  | Cne (e1, e2) -> [ISEQ ((compile_mod_jmp e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))]
  | Cgt (e1, e2) -> [ISLT ((compile_mod_jmp e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RB, RNone, RLab(RDir, label))]
  | Clt (e1, e2) -> [ISLT ((compile_mod_jmp e1 e2 env), (compile_arg e2 env), (compile_arg e1 env)) ; IJMP (RB, RNone, RLab(RDir, label))]


let rec compile_expr (e : expr) (env : env) : instruction list =
  let aenv, penv, lenv = env in
  match e with
  | Nop -> [INOP]
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
    (compile_precond c fin env) @ (compile_expr e env) @ [ILabel (fin)]
  | While (c, e) ->
    let ini = (gensym "L") in
    let fin = (gensym "L") in
    [ILabel (ini)] @ (compile_precond c fin env) @ (compile_expr e env) @ [IJMP (RB, RNone, RLab(RDir, ini))] @ [ILabel (fin)]
  | Dowhile (c, e) ->
    let ini = (gensym "L") in
    [ILabel (ini)] @ (compile_expr e env) @ (compile_postcond c ini env)


let prelude = sprintf "
;redcode-94b
"

let compile_prog (e : expr) : string =
  let _ = (gensym "") in
  let instrs = (compile_expr e empty_env) @ [IDAT (RNum 0, RNum 0)] in
  (prelude) ^ (pp_instrs instrs)
