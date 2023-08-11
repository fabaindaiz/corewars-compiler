(** Compiler **)
open Printf
open Red
open Ast
open Lib
open Analyse

exception CTError of string


let rec compile_arg (arg : arg) (env : env) : rarg =
  let aenv, penv, lenv = env in
  match arg with
  | ANone -> RNone
  | ANum (n) -> RNum (n)
  | AId (s) ->
    (match List.assoc_opt s lenv with
    | Some l -> RLab ((compile_mode MDir PB), l)
    | None -> RLab ((compile_mode MDir PB), s) )
  | ARef (m, n) -> RRef ((compile_mode m PB), n)
  | ALab (m, s) ->
    (match List.assoc_opt s lenv with
    | Some arg ->
      let dest = (translate_penv s penv) in
      RLab ((compile_mode m dest), arg)
    | None ->  RLab ((compile_mode m PB), s) )
  | AStore (s) ->
    let arg = (translate_aenv s aenv) in
    (match arg with
    | AId (s) ->
      (match List.assoc_opt s lenv with
      | Some label -> RLab ((compile_mode MDir PB), label)
      | None -> (compile_arg arg env) )
    | ALab (m, s) ->
      (match List.assoc_opt s lenv with
      | Some label ->
        let dest = (translate_penv s penv) in
        RLab ((compile_mode m dest), label)
      | None -> (compile_arg arg env) )
    | _ -> (compile_arg arg env) )

let compile_label (args : arg list) (env : env) : instruction list =
  let compile_label_aux (arg : arg) (env: env) : instruction list =
    match arg with
    | AStore (s) ->
      let _, _, lenv = env in
      (match List.assoc_opt s lenv with
      | Some l -> [ILabel (l)]
      | None -> failwith (sprintf "unbound variable %s in lenv" s) )
    | _ -> [] in
  List.fold_left (fun res i -> res @ (compile_label_aux i env)) [] args

  
let compile_precond (cond : cond) (label : string ) (env : env) : instruction list =
  match cond with
  | Cond1 (op, e) ->
    (match op with
    | Cjz -> [IJMN (RB, RLab(RDir, label), (compile_arg e env))]
    | Cjn -> [IJMZ (RB, RLab(RDir, label), (compile_arg e env))]
    | Cdn -> raise (CTError (sprintf "DN cond is not available on precondition")) )
  | Cond2 (op, e1, e2) ->
    (match op with
    | Ceq -> [ISEQ ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RLab(RDir, label), RNone)]
    | Cne -> [ISNE ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RLab(RDir, label), RNone)]
    | Cgt -> [ISLT ((compile_mod e2 e1 env), (compile_arg e2 env), (compile_arg e1 env)) ; IJMP (RLab(RDir, label), RNone)]
    | Clt -> [ISLT ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RLab(RDir, label), RNone)] )

let compile_postcond (cond : cond) (label : string ) (env : env) : instruction list =
  match cond with
  | Cond1 (op, e) ->
    (match op with
    | Cjz -> [IJMZ (RB, RLab(RDir, label), (compile_arg e env))]
    | Cjn -> [IJMN (RB, RLab(RDir, label), (compile_arg e env))]
    | Cdn -> [IDJN (RB, RLab(RDir, label), (compile_arg e env))] )
  | Cond2 (op, e1, e2) ->
    (match op with
    | Ceq -> [ISNE ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RLab(RDir, label), RNone)]
    | Cne -> [ISEQ ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RLab(RDir, label), RNone)]
    | Cgt -> [ISLT ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env)) ; IJMP (RLab(RDir, label), RNone)]
    | Clt -> [ISLT ((compile_mod e2 e1 env), (compile_arg e2 env), (compile_arg e1 env)) ; IJMP (RLab(RDir, label), RNone)] )


let rec compile_expr (e : tag eexpr) (env : env) : instruction list =
  match e with
  | ENop (_) -> [INOP]
  | ELabel (l, _) -> [ILabel (l)]
  | EPrim2 (op, e1, e2, _) ->
    (compile_label [e1; e2] env) @ 
    (match op with
    | Dat -> [IDAT ((compile_arg e1 env), (compile_arg e2 env))]
    | Mov -> [IMOV ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Add -> [IADD ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Sub -> [ISUB ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Mul -> [IMUL ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Div -> [IDIV ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Mod -> [IMOD ((compile_mod e1 e2 env), (compile_arg e1 env), (compile_arg e2 env))]
    | Spl -> [ISPL ((compile_arg e1 env), (compile_arg e2 env))]
    | Jmp -> [IJMP ((compile_arg e1 env), (compile_arg e2 env))] )
  | ECont1 (op, c, e, tag) ->
    (match op with
    | If ->
      let fin = (sprintf "IF%d" tag) in
      (compile_precond c fin env) @ (compile_expr e env) @ [ILabel (fin)]
    | While ->
      let ini = (sprintf "WHI%d" tag) in
      let fin = (sprintf "WHF%d" tag) in
      [ILabel (ini)] @ (compile_precond c fin env) @ (compile_expr e env) @ [IJMP (RLab(RDir, ini), RNone)] @ [ILabel (fin)]
    | Dowhile ->
      let ini = (sprintf "DWH%d" tag) in
      [ILabel (ini)] @ (compile_expr e env) @ (compile_postcond c ini env) )
  | ELet (id, arg, body, tag) ->
    let label = (sprintf "LET%d" tag) in
    let env' = (analyse_let id arg body label env) in
    (compile_expr body env')
  | ERepeat (e, tag) ->
    let ini = (sprintf "REP%d" tag) in
    [ILabel (ini)] @ (compile_expr e env) @ [IJMP (RLab(RDir, ini), RNone)]
  | ESeq (exprs, _) ->
    List.fold_left (fun res e -> res @ (compile_expr e env)) [] exprs


let prelude = sprintf "
;redcode-94b
"

let compile_prog (e : expr) : string =
  let tagged = (tag_expr e) in
  let instrs = (compile_expr tagged empty_env) @ [IDAT (RNum 0, RNum 0)] in
  (prelude) ^ (pp_instrs instrs)
