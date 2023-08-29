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

let compile_args (arg1 : arg) (arg2 : arg) (imod : imod) (rmod : rmod) (env : env) : rmod * rarg * rarg =
  let carg1, rarg1 = (compile_arg arg1 env) in
  let carg2, rarg2 = (compile_arg arg2 env) in
  let rmod = (compile_mod carg1 carg2 imod rmod env) in
  rmod, rarg1, rarg2


type mcond =
| Cpre
| Cpos

let compile_cond1 (cond : cond1) (mode : mcond) : opcode =
  match mode with
  | Cpre ->
    (match cond with
    | Cjz -> IJMN
    | Cjn -> IJMZ
    | Cdz -> IDJN
    | Cdn -> raise (CTError (sprintf "DN cond is not available on precondition")) )
  | Cpos ->
    (match cond with
    | Cjz -> IJMZ
    | Cjn -> IJMN
    | Cdz -> raise (CTError (sprintf "DN cond is not available on postcondition"))
    | Cdn -> IDJN )

let compile_cond2 (cond : cond2) (mode : mcond) (a1 : arg) (a2 : arg) : opcode * arg * arg =
  match mode with
  | Cpre ->
    (match cond with
    | Ceq -> ISEQ, a1, a2
    | Cne -> ISNE, a1, a2
    | Cgt -> ISLT, a2, a1
    | Clt -> ISLT, a1, a2 )
  | Cpos ->
    (match cond with
    | Ceq -> ISNE, a1, a2
    | Cne -> ISEQ, a1, a2
    | Cgt -> ISLT, a1, a2
    | Clt -> ISLT, a2, a1 )

let compile_cond (cond : cond) (mode : mcond) (label : string ) (env : env) : instruction list =
  match cond with
  | Cond0 -> raise (CTError (sprintf "Cond0 is not available on condition"))
  | Cond1 (op, a2) ->
    let a1 = ALab (MDir, label) in
    let opcode = (compile_cond1 op mode) in
    let rmod, rarg1, rarg2 = (compile_args a1 a2 MDef RB env) in
    [INSTR (opcode, rmod, rarg1, rarg2)]
  | Cond2 (op, a1, a2) ->
    let opcode, a1, a2 = (compile_cond2 op mode a1 a2) in
    let rmod, rarg1, rarg2 = (compile_args a1 a2 MDef RI env) in
    [INSTR (opcode, rmod, rarg1, rarg2) ; (jump_label label)]


let compile_prim2 (op : prim2) : opcode =
  match op with
  | Dat -> IDAT
  | Nop -> INOP
  | Spl -> ISPL
  | Jmp -> IJMP
  | Mov -> IMOV
  | Add -> IADD
  | Sub -> ISUB
  | Mul -> IMUL
  | Div -> IDIV
  | Mod -> IMOD
  | Jmz -> IJMZ
  | Jmn -> IJMN
  | Djn -> IDJN
  | Seq -> ISEQ
  | Sne -> ISNE
  | Slt -> ISLT
  | Stp -> ISTP
  | Ldp -> ILDP

let rec compile_expr (e : tag eexpr) (env : env) : instruction list =
  match e with
  | EComment (s) -> [ICOM (s)]
  | ELabel (l, _) -> [ILAB (l)]
  | EPrim2 (op, imod, arg1, arg2, _) ->
    let opcode = (compile_prim2 op) in
    let rmod, rarg1, rarg2 = (compile_args arg1 arg2 imod RI env) in
    (compile_label [arg1; arg2] env) @ [INSTR (opcode, rmod, rarg1, rarg2)]
  | EFlow1 (op, cond, exp, tag) ->
    (match op with
    | Repeat ->
      let ini = (sprintf "REP%d" tag) in
      [ILAB (ini)] @ (compile_expr exp env) @ [(jump_label ini)]
    | If ->
      let fin = (sprintf "IF%d" tag) in
      (compile_cond cond Cpre fin env) @ (compile_expr exp env) @ [ILAB (fin)]
    | While ->
      let ini = (sprintf "WHI%d" tag) in
      let fin = (sprintf "WHF%d" tag) in
      [ILAB (ini)] @ (compile_cond cond Cpre fin env) @ (compile_expr exp env) @ [(jump_label ini) ; ILAB (fin)]
    | DoWhile ->
      let ini = (sprintf "DWH%d" tag) in
      [ILAB (ini)] @ (compile_expr exp env) @ (compile_cond cond Cpos ini env) )
  | EFlow2 (op, cond, exp1, exp2, tag) ->
    (match op with
    | IfElse ->
      let mid = (sprintf "IFM%d" tag) in
      let fin = (sprintf "IFF%d" tag) in
      (compile_cond cond Cpre mid env) @ (compile_expr exp1 env) @ [(jump_label fin) ; ILAB (mid)] @ (compile_expr exp2 env) @ [ILAB (fin)] )
  | ELet (id, arg, body, tag) ->
    let label = (sprintf "LET%d" tag) in
    let env' = (analyse_let id arg body label env) in
    (compile_expr body env')
  | ESeq (exps, _) ->
    List.fold_left (fun res exp -> res @ (compile_expr exp env)) [] exps


let prelude = "
;redcode-94b
"

let epilogue = [INSTR (IDAT, RN, RNone, RNone)]

let compile_prog (e : expr) : string =
  let tag_e = (tag_expr e) in
  let instrs = (compile_expr tag_e empty_env) in
  (prelude) ^ (pp_instrs instrs) ^ (pp_instrs epilogue)
