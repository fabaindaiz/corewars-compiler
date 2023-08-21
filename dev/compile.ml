(** Compiler **)
open Printf
open Red
open Ast
open Lib
open Util
open Analyse

exception CTError of string


let compile_label (args : arg list) (env : env) : instruction list =
  let compile_label_aux (arg : arg) (env : env) : instruction list =
    match arg with
    | AStore (s) ->
      let _, _, lenv = env in
      (match List.assoc_opt s lenv with
      | Some l -> [ILAB (l)]
      | None -> raise (CTError (sprintf "unbound variable %s in lenv" s)) )
    | _ -> [] in
  List.fold_left (fun res i -> res @ (compile_label_aux i env)) [] args

let compile_args (arg1 : arg) (arg2 : arg) (env : env) : rmod * rarg * rarg =
  let carg1, rarg1 = (compile_arg arg1 env) in
  let carg2, rarg2 = (compile_arg arg2 env) in
  let rmod = (compile_mod carg1 carg2 MNone RB env) in
  rmod, rarg1, rarg2

  
let compile_precond (cond : cond) (label : string ) (env : env) : instruction list =
  match cond with
  | Cond1 (op, a2) ->
    let a1 = ALab (MDir, label) in
    let carg1, rarg1 = (compile_arg a1 env) in
    let carg2, rarg2 = (compile_arg a2 env) in
    let rmod = (compile_mod carg1 carg2 MNone RI env) in
    (match op with
    | Cjz -> [IJMN (rmod, rarg1, rarg2)]
    | Cjn -> [IJMZ (rmod, rarg1, rarg2)]
    | Cdz -> [IDJN (rmod, rarg1, rarg2)]
    | Cdn -> raise (CTError (sprintf "DN cond is not available on precondition")) )
  | Cond2 (op, a1, a2) ->
    let carg1, rarg1 = (compile_arg a1 env) in
    let carg2, rarg2 = (compile_arg a2 env) in
    let rmod = (compile_mod carg1 carg2 MNone RI env) in
    (match op with
    | Ceq -> [ISEQ (rmod, rarg1, rarg2) ; IJMP (RLab (RDir, label), RNone)]
    | Cne -> [ISNE (rmod, rarg1, rarg2) ; IJMP (RLab (RDir, label), RNone)]
    | Cgt -> 
      let rmod' = (compile_mod carg2 carg1 MNone RI env) in
      [ISLT (rmod', rarg2, rarg1) ; IJMP (RLab (RDir, label), RNone)]
    | Clt -> [ISLT (rmod, rarg1, rarg2) ; IJMP (RLab (RDir, label), RNone)] )

let compile_postcond (cond : cond) (label : string ) (env : env) : instruction list =
  match cond with
  | Cond1 (op, a2) ->
    let a1 = ALab (MDir, label) in
    let carg1, rarg1 = (compile_arg a1 env) in
    let carg2, rarg2 = (compile_arg a2 env) in
    let rmod = (compile_mod carg1 carg2 MNone RB env) in
    (match op with
    | Cjz -> [IJMZ (rmod, rarg1, rarg2)]
    | Cjn -> [IJMN (rmod, rarg1, rarg2)]
    | Cdz -> raise (CTError (sprintf "DZ cond is not available on postcondition"))
    | Cdn -> [IDJN (rmod, rarg1, rarg2)] )
  | Cond2 (op, a1, a2) ->
    let carg1, rarg1 = (compile_arg a1 env) in
    let carg2, rarg2 = (compile_arg a2 env) in
    let rmod = (compile_mod carg1 carg2 MNone RI env) in
    (match op with
    | Ceq -> [ISNE (rmod, rarg1, rarg2) ; IJMP (RLab (RDir, label), RNone)]
    | Cne -> [ISEQ (rmod, rarg1, rarg2) ; IJMP (RLab (RDir, label), RNone)]
    | Cgt -> [ISLT (rmod, rarg1, rarg2) ; IJMP (RLab (RDir, label), RNone)]
    | Clt ->
      let rmod' = (compile_mod carg2 carg1 MNone RI env) in
      [ISLT (rmod', rarg2, rarg1) ; IJMP (RLab (RDir, label), RNone)] )


let rec compile_expr (e : tag eexpr) (env : env) : instruction list =
  match e with
  | EComment (s) -> [ICOM (s)]
  | ELabel (l, _) -> [ILAB (l)]
  | EPrim2 (op, arg1, arg2, _) ->
    let _, rarg1 = (compile_arg arg1 env) in
    let _, rarg2 = (compile_arg arg2 env) in
    (compile_label [arg1; arg2] env) @ 
    (match op with
    | Dat -> [IDAT (rarg1, rarg2)]
    | Nop -> [INOP (rarg1, rarg2)]
    | Spl -> [ISPL (rarg1, rarg2)]
    | Jmp -> [IJMP (rarg1, rarg2)] )
  | EPrim2m (op, imod, arg1, arg2, _) ->
    let carg1, rarg1 = (compile_arg arg1 env) in
    let carg2, rarg2 = (compile_arg arg2 env) in
    let rmod = (compile_mod carg1 carg2 imod RI env) in
    (compile_label [arg1; arg2] env) @ 
    (match op with
    | Mov -> [IMOV (rmod, rarg1, rarg2)]
    | Add -> [IADD (rmod, rarg1, rarg2)]
    | Sub -> [ISUB (rmod, rarg1, rarg2)]
    | Mul -> [IMUL (rmod, rarg1, rarg2)]
    | Div -> [IDIV (rmod, rarg1, rarg2)]
    | Mod -> [IMOD (rmod, rarg1, rarg2)]
    | Jmz -> [IJMZ (rmod, rarg1, rarg2)]
    | Jmn -> [IJMN (rmod, rarg1, rarg2)]
    | Djn -> [IDJN (rmod, rarg1, rarg2)]
    | Seq -> [ISEQ (rmod, rarg1, rarg2)]
    | Sne -> [ISNE (rmod, rarg1, rarg2)]
    | Slt -> [ISLT (rmod, rarg1, rarg2)]
    | Stp -> [ISTP (rmod, rarg1, rarg2)]
    | Ldp -> [ILDP (rmod, rarg1, rarg2)] )
  | EFlow (op, exp, tag) ->
    (match op with
    | Repeat ->
      let ini = (sprintf "REP%d" tag) in
      [ILAB (ini)] @ (compile_expr exp env) @ [IJMP (RLab (RDir, ini), RNone)] )
  | EFlow1 (op, cond, exp, tag) ->
    (match op with
    | If ->
      let fin = (sprintf "IF%d" tag) in
      (compile_precond cond fin env) @ (compile_expr exp env) @ [ILAB (fin)]
    | While ->
      let ini = (sprintf "WHI%d" tag) in
      let fin = (sprintf "WHF%d" tag) in
      [ILAB (ini)] @ (compile_precond cond fin env) @ (compile_expr exp env) @
      [IJMP (RLab(RDir, ini), RNone)] @ [ILAB (fin)]
    | DoWhile ->
      let ini = (sprintf "DWH%d" tag) in
      [ILAB (ini)] @ (compile_expr exp env) @ (compile_postcond cond ini env) )
  | EFlow2 (op, cond, exp1, exp2, tag) ->
    (match op with
    | IfElse ->
      let mid = (sprintf "IFM%d" tag) in
      let fin = (sprintf "IFF%d" tag) in
      (compile_precond cond mid env) @ (compile_expr exp1 env) @ [IJMP (RLab (RDir, fin), RNone)] @
      [ILAB (mid)] @ (compile_expr exp2 env) @ [ILAB (fin)] )
  | ELet (id, arg, body, tag) ->
    let label = (sprintf "LET%d" tag) in
    let env' = (analyse_let id arg body label env) in
    (compile_expr body env')
  | ESeq (exps, _) ->
    List.fold_left (fun res exp -> res @ (compile_expr exp env)) [] exps


let prelude = "
;redcode-94b
"

let epilogue = [IDAT (RNone, RNone)]

let compile_prog (e : expr) : string =
  let tag_e = (tag_expr e) in
  let instrs = (compile_expr tag_e empty_env) in
  (prelude) ^ (pp_instrs instrs) ^ (pp_instrs epilogue)
