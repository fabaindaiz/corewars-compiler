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
    | None -> let s = Rename.original s in error (sprintf "(store %s): %s is not a variable of an enclosing let" s s) )
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

(* JMZ/JMN/DJN test or decrement one field of their B-target, and their A operand is only where
   to jump: the field the tested variable is stored in decides (.A or .B). An immediate #x is the
   instruction's own B-number, and anything else is tested on its B-field (the ICWS'94 default). *)
let jump_modifier (carg2 : carg) (env : env) : rmod =
  if immediate carg2 then RB
  else match carg_to_opmod carg2 env with
    | TA -> RA
    | TB | TNum | TRef -> RB

(* With no modifier written, the variables' fields decide (opmod_to_rmod); where they do not, the
   ICWS'94 default for the opcode, as pMARS would give the same redcode written by hand. *)
let compile_mod (carg1 : carg) (carg2 : carg) (imod : imod) (opcode : opcode) (env : env) : rmod =
  let mod1 = (carg_to_opmod carg1 env) in
  let mod2 = (carg_to_opmod carg2 env) in
  match imod with
  | MDef when (opcode = IJMZ || opcode = IJMN || opcode = IDJN) -> jump_modifier carg2 env
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
  | Cond1 (op, imod, a2) ->
    let a1 = ALab (MDir, label) in
    let opcode = (compile_cond1 op mode) in
    let carg1, rarg1 = (compile_arg a1 env) in
    let carg2, rarg2 = (compile_arg a2 env) in
    let rmod = compile_mod carg1 carg2 imod opcode env in
    (* A (store x) in a condition makes the condition's cell x's place: label it, as a primitive does. *)
    List.map emit (compile_label a2 env)
    @ [emit ~stores:(stores_of a1 a2) (INSTR (opcode, rmod, rarg1, rarg2))]
  | Cond2 (op, imod, a1, a2) ->
    let opcode, a1, a2, always_skip = (compile_cond2 op mode a1 a2) in
    (* GT is emitted as SLT y, x: a modifier the user wrote for x and y reads the swapped operands. *)
    let imod = match op, imod with
      | Cgt, MAB -> MBA
      | Cgt, MBA -> MAB
      | (Ceq | Cne | Cgt | Clt), m -> m in
    let rmod, rarg1, rarg2 = (compile_args a1 a2 imod opcode env) in
    let skip = if always_skip then [emit (INSTR (ISNE, RAB, RRef (RImm, 0), RRef (RImm, 1)))] else [] in
    List.map emit (compile_label a1 env @ compile_label a2 env)
    @ [emit ~stores:(stores_of a1 a2) (INSTR (opcode, rmod, rarg1, rarg2))] @ skip @ [emit (jump_label label)]


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

(* The optional transformations the policy chooses between (Optimize.choose); no_opts emits the
   code every construct had before any of them existed. *)
type options = { rotate_unary : bool; rotate_binary : bool }

let no_opts = { rotate_unary = false; rotate_binary = false }

let rec compile_expr (opts : options) (e : meta eexpr) (env : env) : emitted list =
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
      [gen (ILAB (ini))] @ (compile_expr opts exp env) @ [gen (jump_label ini)]
    | If ->
      let gen = emit ~origin:tag ~construct:"if" in
      let fin = (sprintf "_IF%d" tag) in
      (compile_cond cond Cpre fin env tag "if") @ (compile_expr opts exp env) @ [gen (ILAB (fin))]
    | While ->
      let gen = emit ~origin:tag ~construct:"while" in
      let ini = (sprintf "_WHI%d" tag) in
      let fin = (sprintf "_WHF%d" tag) in
      (* Rotated, the test sits after the body and one JMP enters it: while c e is if c (do-while c e).
         A unary test then loops in one instruction instead of two; DZ has no single post-test. *)
      let rotate = match cond with
        | Cond1 ((Cjz | Cjn), _, _) -> opts.rotate_unary
        | Cond2 _ -> opts.rotate_binary
        | Cond1 ((Cdz | Cdn), _, _) | Cond0 -> false in
      if rotate then
        let test = (sprintf "_WHC%d" tag) in
        let body = (compile_expr opts exp env) in
        [gen (jump_label test) ; gen (ILAB (ini))] @ body @ [gen (ILAB (test))]
        @ (compile_cond cond Cpos ini env tag "while") @ [gen (ILAB (fin))]
      else
      [gen (ILAB (ini))] @ (compile_cond cond Cpre fin env tag "while") @ (compile_expr opts exp env) @ [gen (jump_label ini) ; gen (ILAB (fin))]
    | DoWhile ->
      let gen = emit ~origin:tag ~construct:"do-while" in
      let ini = (sprintf "_DWH%d" tag) in
      [gen (ILAB (ini))] @ (compile_expr opts exp env) @ (compile_cond cond Cpos ini env tag "do-while") )
  | EFlow2 (op, cond, exp1, exp2, m) -> at m @@ fun () ->
    let tag = m.tag in
    (match op with
    | IfElse ->
      let gen = emit ~origin:tag ~construct:"if-else" in
      let mid = (sprintf "_IFM%d" tag) in
      let fin = (sprintf "_IFF%d" tag) in
      (compile_cond cond Cpre mid env tag "if-else") @ (compile_expr opts exp1 env) @ [gen (jump_label fin) ; gen (ILAB (mid))] @ (compile_expr opts exp2 env) @ [gen (ILAB (fin))] )
  | ELet (id, arg, body, m) -> at m @@ fun () ->
    let tag = m.tag in
    let label = (sprintf "_LET%d" tag) in
    let env' = (analyse_let id arg body label env) in
    (compile_expr opts body env')
  | ESeq (exps, _) ->
    List.fold_left (fun res exp -> res @ (compile_expr opts exp env)) [] exps
  | EExpect _ -> []

(* A generated jump whose target cell holds a generated JMP goes straight to that JMP's target: the
   same cells, one cycle less each time it is taken (an if at the end of a repeat cost the scanner
   archetype 3 cycles per empty cell, against 2 by hand). Only jumps the compiler generated are
   rewritten, and only through JMPs it generated: a JMP the user wrote, or one carrying a user's
   label, may have its target changed at run time. A JMP is passed only when the cell before it
   falls into it (or the one before that skips into it): the jumps that reach it otherwise all go
   past it, and it would be left dead, still closing its loop. A chain is followed until it repeats
   a label. *)
let thread_jumps (body : emitted list) : emitted list =
  (* Each instruction with the labels on its cell, in order. *)
  let cells = Array.of_list (List.rev (snd (List.fold_left (fun (pending, acc) (e : emitted) ->
      match e.instr with
      | ILAB l -> (l :: pending, acc)
      | ICOM _ -> (pending, acc)
      | INSTR _ -> ([], (e, pending) :: acc)) ([], []) body))) in
  let at = Hashtbl.create 16 in
  Array.iteri (fun i (_, labels) -> List.iter (fun l -> Hashtbl.replace at l i) labels) cells ;
  let op i = match (fst cells.(i)).instr with INSTR (o, _, _, _) -> Some o | ILAB _ | ICOM _ -> None in
  let continues i = match op i with
    | Some (IJMP | IDAT) | None -> false
    | Some (ISPL | INOP | IMOV | IADD | ISUB | IMUL | IDIV | IMOD | IJMZ | IJMN | IDJN | ISEQ | ISNE
           | ISLT | ICMP | ILDP | ISTP) -> true in
  let skips i = match op i with
    | Some (ISEQ | ISNE | ISLT | ICMP) -> true
    | Some (IDAT | ISPL | IJMP | INOP | IMOV | IADD | ISUB | IMUL | IDIV | IMOD | IJMZ | IJMN | IDJN
           | ILDP | ISTP) | None -> false in
  let falls_into i = i = 0 || continues (i - 1) || (i >= 2 && skips (i - 2)) in
  let user_label l = not (String.starts_with ~prefix:"_" l) in
  let generated_jmp l = match Hashtbl.find_opt at l with
    | Some i ->
      (match cells.(i) with
      | ({ instr = INSTR (IJMP, _, RLab (RDir, l'), _); construct = Some _; _ }, labels)
        when falls_into i && not (List.exists user_label labels) -> Some l'
      | ({ instr = INSTR _ | ILAB _ | ICOM _; _ }, _) -> None)
    | None -> None in
  let rec final seen l = match generated_jmp l with
    | Some l' when not (List.mem l' seen) -> final (l' :: seen) l'
    | Some _ | None -> l in
  List.map (fun (e : emitted) -> match e with
    | { instr = INSTR ((IJMP | IJMZ | IJMN | IDJN) as op, md, RLab (RDir, l), b); construct = Some _; _ } ->
      { e with instr = INSTR (op, md, RLab (RDir, final [l] l), b) }
    | { instr = INSTR _ | ILAB _ | ICOM _; _ } -> e) body

let compile_body ?(opts = no_opts) (e : expr) : emitted list =
  thread_jumps (compile_expr opts (tag_expr (Rename.uniquify e)) empty_env)


let prelude = "
;redcode-94b
"

(* What pMARS fills empty core with (DAT.F $0, $0, pmars.c in the vendored source): a program that
   runs past its end dies here, a label at the end has a cell, and a scanner sees empty core. *)
let epilogue = [INSTR (IDAT, RN, RRef (RDir, 0), RRef (RDir, 0))]

(* pMARS 0.9.4 hangs on a source line of 256 characters or more (measured: a 245-character label
   in an instruction line hung it, 200 did not). *)
let max_line = 256

let compile_prog ?opts (e : expr) : string =
  let instrs = List.map (fun (x : emitted) -> x.instr) (compile_body ?opts e) in
  let text = (prelude) ^ (pp_instrs instrs) ^ (pp_instrs epilogue) in
  List.iteri (fun i line ->
    let n = String.length line in
    if n >= max_line then
      error (sprintf "redcode line %d has %d characters; pMARS hangs on lines of %d or more" (i + 1) n max_line))
    (String.split_on_char '\n' text) ;
  text
