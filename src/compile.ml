(** Compiler **)
open Printf
open Red
open Ast
open Lib
open Util
open Analyse



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
    | None -> error (sprintf "(store %s): %s is not a variable of an enclosing let" s s) )
  | _ -> []


let compile_arg (arg : arg) (env : env) : carg * rarg =
  let arg' = (replace_store arg env) in
  let darg = (arg_to_darg arg') in
  let carg = (darg_to_carg darg env) in
  let rarg = (carg_to_rarg carg env) in
  (carg, rarg)

let immediate (carg : carg) : bool =
  match carg with
  | ACRef (m, _) | ACLab (m, _) | ACVar (m, _) | ACPnt (m, _) -> m = MImm

(* With no modifier written, the variables' fields decide (opmod_to_rmod); where they do not, the
   ICWS'94 default for the opcode, as pMARS would give the same redcode written by hand. *)
let compile_mod (carg1 : carg) (carg2 : carg) (imod : imod) (opcode : opcode) (env : env) : rmod =
  let mod1 = (carg_to_opmod carg1 env) in
  let mod2 = (carg_to_opmod carg2 env) in
  match imod with
  | MDef -> (opmod_to_rmod mod1 mod2 (default_modifier opcode ~a_imm:(immediate carg1) ~b_imm:(immediate carg2)))
  | MN -> RN
  | MA -> RA
  | MB -> RB
  | MAB -> RAB
  | MBA -> RBA
  | MI -> RI
  | MF -> RF
  | MX -> RX

let compile_args (arg1 : arg) (arg2 : arg) (imod : imod) (opcode : opcode) (env : env) : rmod * rarg * rarg =
  let carg1, rarg1 = (compile_arg arg1 env) in
  let carg2, rarg2 = (compile_arg arg2 env) in
  let rmod = (compile_mod carg1 carg2 imod opcode env) in
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
    | Cdn -> error "(DN x) is only available in do-while" )
  | Cpos ->
    (match cond with
    | Cjz -> IJMZ
    | Cjn -> IJMN
    | Cdz -> error "(DZ x) is not available in do-while"
    | Cdn -> IDJN )

(* The test, its operands, and whether an always-skipping SNE must follow it. A pre-condition jumps
   past the body when the condition is false; a post-condition jumps back when it is true. SEQ/SNE
   can skip on either outcome, but SLT only skips when strictly less: a post-condition GT or LT
   skips past an always-skipping SNE when true, and runs it, skipping the jump, when false. *)
let compile_cond2 (cond : cond2) (mode : mcond) (a1 : arg) (a2 : arg) : opcode * arg * arg * bool =
  match mode with
  | Cpre ->
    (match cond with
    | Ceq -> ISEQ, a1, a2, false
    | Cne -> ISNE, a1, a2, false
    | Cgt -> ISLT, a2, a1, false
    | Clt -> ISLT, a1, a2, false )
  | Cpos ->
    (match cond with
    | Ceq -> ISNE, a1, a2, false
    | Cne -> ISEQ, a1, a2, false
    | Cgt -> ISLT, a2, a1, true
    | Clt -> ISLT, a1, a2, true )

let compile_cond (cond : cond) (mode : mcond) (label : string ) (env : env) (tag : tag) (construct : string) : emitted list =
  let emit = emit ~origin:tag ~construct in
  match cond with
  | Cond0 -> []
  | Cond1 (op, a2) ->
    let a1 = ALab (MDir, label) in
    let opcode = (compile_cond1 op mode) in
    let _, rarg1 = (compile_arg a1 env) in
    let carg2, rarg2 = (compile_arg a2 env) in
    (* JMZ/JMN/DJN test or decrement one field of their B-target: the field the tested variable
       is stored in, .B otherwise (the ICWS'94 default). *)
    let rmod = (match carg_to_opmod carg2 env with
      | TA -> RA
      | TB | TNum | TRef -> RB) in
    [emit ~stores:(stores_of a1 a2) (INSTR (opcode, rmod, rarg1, rarg2))]
  | Cond2 (op, a1, a2) ->
    let opcode, a1, a2, always_skip = (compile_cond2 op mode a1 a2) in
    let rmod, rarg1, rarg2 = (compile_args a1 a2 MDef opcode env) in
    let skip = if always_skip then [emit (INSTR (ISNE, RAB, RRef (RImm, 0), RRef (RImm, 1)))] else [] in
    [emit ~stores:(stores_of a1 a2) (INSTR (opcode, rmod, rarg1, rarg2))] @ skip @ [emit (jump_label label)]


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

(* An error raised without a location takes the location of the innermost node being compiled. *)
let at (m : meta) (f : unit -> 'a) : 'a =
  try f () with Error (None, msg) -> raise (Error (Some m.loc, msg))

let rec compile_expr (e : meta eexpr) (env : env) : emitted list =
  match e with
  | EComment (s) -> [emit (ICOM (s))]
  | ELabel (l, m) -> [emit ~origin:m.tag (ILAB (l))]
  | EPrim2 (op, imod, arg1, arg2, m) -> at m @@ fun () ->
    let tag = m.tag in
    let opcode = (compile_prim2 op) in
    let rmod, rarg1, rarg2 = (compile_args arg1 arg2 imod opcode env) in
    let labels = List.map (emit ~origin:tag) ((compile_label arg1 env) @ (compile_label arg2 env)) in
    labels @ [emit ~origin:tag ~stores:(stores_of arg1 arg2) (INSTR (opcode, rmod, rarg1, rarg2))]
  | EFlow1 (op, cond, exp, m) -> at m @@ fun () ->
    let tag = m.tag in
    (match op with
    | Repeat ->
      let gen = emit ~origin:tag ~construct:"repeat" in
      let ini = (sprintf "_REP%d" tag) in
      [gen (ILAB (ini))] @ (compile_expr exp env) @ [gen (jump_label ini)]
    | If ->
      let gen = emit ~origin:tag ~construct:"if" in
      let fin = (sprintf "_IF%d" tag) in
      (compile_cond cond Cpre fin env tag "if") @ (compile_expr exp env) @ [gen (ILAB (fin))]
    | While ->
      let gen = emit ~origin:tag ~construct:"while" in
      let ini = (sprintf "_WHI%d" tag) in
      let fin = (sprintf "_WHF%d" tag) in
      [gen (ILAB (ini))] @ (compile_cond cond Cpre fin env tag "while") @ (compile_expr exp env) @ [gen (jump_label ini) ; gen (ILAB (fin))]
    | DoWhile ->
      let gen = emit ~origin:tag ~construct:"do-while" in
      let ini = (sprintf "_DWH%d" tag) in
      [gen (ILAB (ini))] @ (compile_expr exp env) @ (compile_cond cond Cpos ini env tag "do-while") )
  | EFlow2 (op, cond, exp1, exp2, m) -> at m @@ fun () ->
    let tag = m.tag in
    (match op with
    | IfElse ->
      let gen = emit ~origin:tag ~construct:"if-else" in
      let mid = (sprintf "_IFM%d" tag) in
      let fin = (sprintf "_IFF%d" tag) in
      (compile_cond cond Cpre mid env tag "if-else") @ (compile_expr exp1 env) @ [gen (jump_label fin) ; gen (ILAB (mid))] @ (compile_expr exp2 env) @ [gen (ILAB (fin))] )
  | ELet (id, arg, body, m) -> at m @@ fun () ->
    let tag = m.tag in
    let label = (sprintf "_LET%d" tag) in
    let env' = (analyse_let id arg body label env) in
    (compile_expr body env')
  | ESeq (exps, _) ->
    List.fold_left (fun res exp -> res @ (compile_expr exp env)) [] exps
  | EExpect _ -> []

let compile_body (e : expr) : emitted list =
  compile_expr (tag_expr (Rename.uniquify e)) empty_env


let prelude = "
;redcode-94b
"

let epilogue = [INSTR (IDAT, RN, RNone, RNone)]

(* pMARS 0.9.4 hangs on a source line of 256 characters or more (measured: a 245-character label
   in an instruction line hung it, 200 did not). *)
let max_line = 256

let compile_prog (e : expr) : string =
  let instrs = List.map (fun (x : emitted) -> x.instr) (compile_body e) in
  let text = (prelude) ^ (pp_instrs instrs) ^ (pp_instrs epilogue) in
  List.iteri (fun i line ->
    let n = String.length line in
    if n >= max_line then
      error (sprintf "redcode line %d has %d characters; pMARS hangs on lines of %d or more" (i + 1) n max_line))
    (String.split_on_char '\n' text) ;
  text
