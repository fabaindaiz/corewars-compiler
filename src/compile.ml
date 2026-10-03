(** Compiler **)
open Printf
open Red
open Ast
open Lib
open Util
open Analyse

exception CTError of string


(* An instruction with what produced it: the AST node (its tag), the control construct that
   generated it (None for what the user wrote), and the variables stored in its operands. Layout
   and Metrics read these; printing ignores them. *)
type emitted = {
  instr : instruction;
  origin : tag option;
  construct : string option;
  stores : (string * place) list;
}

let emit ?origin ?construct ?(stores = []) (instr : instruction) : emitted =
  { instr; origin; construct; stores }

let stores_of (a1 : arg) (a2 : arg) : (string * place) list =
  let one a p = match a with
    | AStore s -> [(s, p)]
    | ANone | ANum _ | AId _ | ARef _ | ALab _ -> [] in
  one a1 PA @ one a2 PB


let compile_label (arg : arg) (env : env) : instruction list =
  match arg with
  | AStore (s) ->
    let _, _, lenv = env in
    (match List.assoc_opt s lenv with
    | Some l -> [ILAB (l)]
    | None -> raise (CTError (sprintf "unbound variable %s in lenv" s)) )
  | _ -> []


let compile_arg (arg : arg) (env : env) : carg * rarg =
  let arg' = (replace_store arg env) in
  let darg = (arg_to_darg arg') in
  let carg = (darg_to_carg darg env) in
  let rarg = (carg_to_rarg carg env) in
  (carg, rarg)

let compile_mod (carg1 : carg) (carg2 : carg) (imod : imod) (rmod : rmod) (env : env) : rmod =
  let mod1 = (carg_to_opmod carg1 env) in
  let mod2 = (carg_to_opmod carg2 env) in
  match imod with
  | MDef -> (opmod_to_rmod mod1 mod2 rmod)
  | MN -> RN
  | MA -> RA
  | MB -> RB
  | MAB -> RAB
  | MBA -> RBA
  | MI -> RI
  | MF -> RF
  | MX -> RX

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

let compile_cond (cond : cond) (mode : mcond) (label : string ) (env : env) (tag : tag) (construct : string) : emitted list =
  let emit = emit ~origin:tag ~construct in
  match cond with
  | Cond0 -> []
  | Cond1 (op, a2) ->
    let a1 = ALab (MDir, label) in
    let opcode = (compile_cond1 op mode) in
    let rmod, rarg1, rarg2 = (compile_args a1 a2 MDef RB env) in
    [emit ~stores:(stores_of a1 a2) (INSTR (opcode, rmod, rarg1, rarg2))]
  | Cond2 (op, a1, a2) ->
    let opcode, a1, a2 = (compile_cond2 op mode a1 a2) in
    let rmod, rarg1, rarg2 = (compile_args a1 a2 MDef RI env) in
    [emit ~stores:(stores_of a1 a2) (INSTR (opcode, rmod, rarg1, rarg2)) ; emit (jump_label label)]


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

let rec compile_expr (e : tag eexpr) (env : env) : emitted list =
  match e with
  | EComment (s) -> [emit (ICOM (s))]
  | ELabel (l, tag) -> [emit ~origin:tag (ILAB (l))]
  | EPrim2 (op, imod, arg1, arg2, tag) ->
    let opcode = (compile_prim2 op) in
    let rmod, rarg1, rarg2 = (compile_args arg1 arg2 imod RI env) in
    let labels = List.map (emit ~origin:tag) ((compile_label arg1 env) @ (compile_label arg2 env)) in
    labels @ [emit ~origin:tag ~stores:(stores_of arg1 arg2) (INSTR (opcode, rmod, rarg1, rarg2))]
  | EFlow1 (op, cond, exp, tag) ->
    (match op with
    | Repeat ->
      let gen = emit ~origin:tag ~construct:"repeat" in
      let ini = (sprintf "REP%d" tag) in
      [gen (ILAB (ini))] @ (compile_expr exp env) @ [gen (jump_label ini)]
    | If ->
      let gen = emit ~origin:tag ~construct:"if" in
      let fin = (sprintf "IF%d" tag) in
      (compile_cond cond Cpre fin env tag "if") @ (compile_expr exp env) @ [gen (ILAB (fin))]
    | While ->
      let gen = emit ~origin:tag ~construct:"while" in
      let ini = (sprintf "WHI%d" tag) in
      let fin = (sprintf "WHF%d" tag) in
      [gen (ILAB (ini))] @ (compile_cond cond Cpre fin env tag "while") @ (compile_expr exp env) @ [gen (jump_label ini) ; gen (ILAB (fin))]
    | DoWhile ->
      let gen = emit ~origin:tag ~construct:"do-while" in
      let ini = (sprintf "DWH%d" tag) in
      [gen (ILAB (ini))] @ (compile_expr exp env) @ (compile_cond cond Cpos ini env tag "do-while") )
  | EFlow2 (op, cond, exp1, exp2, tag) ->
    (match op with
    | IfElse ->
      let gen = emit ~origin:tag ~construct:"if-else" in
      let mid = (sprintf "IFM%d" tag) in
      let fin = (sprintf "IFF%d" tag) in
      (compile_cond cond Cpre mid env tag "if-else") @ (compile_expr exp1 env) @ [gen (jump_label fin) ; gen (ILAB (mid))] @ (compile_expr exp2 env) @ [gen (ILAB (fin))] )
  | ELet (id, arg, body, tag) ->
    let label = (sprintf "LET%d" tag) in
    let env' = (analyse_let id arg body label env) in
    (compile_expr body env')
  | ESeq (exps, _) ->
    List.fold_left (fun res exp -> res @ (compile_expr exp env)) [] exps

let compile_body (e : expr) : emitted list =
  compile_expr (tag_expr e) empty_env


let prelude = "
;redcode-94b
"

let epilogue = [INSTR (IDAT, RN, RNone, RNone)]

let compile_prog (e : expr) : string =
  let instrs = List.map (fun (x : emitted) -> x.instr) (compile_body e) in
  (prelude) ^ (pp_instrs instrs) ^ (pp_instrs epilogue)
