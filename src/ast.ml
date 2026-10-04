(** AST **)


type place =
| PA
| PB


type imode =
| MINone
| MIInc
| MIDec

type mode =
| MImm          (* Immediate *)
| MDir          (* Direct *)
| MInd of imode (* Indirect *)

type arg =
| ANone
| AStore of string
| ANum of int
| AId of string
| ARef of mode * int
| ALab of mode * string
| AExp of mode option * Red.rexpr (* an expression; its mode is decided by Consts.resolve when not written *)


type cond1 =
| Cjz
| Cjn
| Cdz
| Cdn

type cond2 =
| Ceq
| Cne
| Cgt
| Clt

type imod =
| MDef
| MN
| MA
| MB
| MAB
| MBA
| MI
| MX
| MF

(* A condition's modifier: MDef unless the user wrote one, as in (NE I a b) or (JZ F x). *)
type cond =
| Cond0
| Cond1 of cond1 * imod * arg
| Cond2 of cond2 * imod * arg * arg

type prim2 =
| Dat
| Jmp
| Spl
| Nop
| Mov
| Add
| Sub
| Mul
| Div
| Mod
| Jmz
| Jmn
| Djn
| Seq
| Sne
| Slt
| Stp
| Ldp


(* A repeat's arg is the B operand of its JMP back: not a target, but evaluated every iteration
   (data, a (store x), or a pointer that moves). *)
type flow1 =
| Repeat of arg
| If
| While
| DoWhile

type flow2 =
| IfElse


(* What the programmer expects of the compiled warrior: checked at compile time against the
   metrics, or exported as behaviour probes (alive, dead, cell). *)
type cmp =
| Eq
| Le

type expectation =
| XLength of cmp * int
| XCycles of cmp * int
| XOverhead of cmp * int
| XBoot of cmp * int
| XStep of int
| XCoversCore
| XAlive of int
| XDead of int
| XCell of int * string * int  (* address, text, after N instructions *)


(* Where a node starts in the source: line and column, both from 1. *)
type loc = { line : int; col : int }

(* The one compile error: what the user wrote wrong, and where when known. An impossible state
   inside the compiler is not this: it is [Failure] (failwith), reported as an internal error. *)
exception Error of loc option * string

let error (msg : string) : 'a = raise (Error (None, msg))

type tag = int

(* The AST, annotated: the parser builds [loc eexpr] and tagging turns it into [meta eexpr]. *)
type 'a eexpr =
| EComment of string
| ELabel of string * 'a
| EPrim2 of prim2 * imod * arg * arg * 'a
| EFlow1 of flow1 * cond * 'a eexpr * 'a
| EFlow2 of flow2 * cond * 'a eexpr * 'a eexpr * 'a
| ELet of string * arg * 'a eexpr * 'a
| ESeq of 'a eexpr list * 'a
| EExpect of expectation * 'a

type expr = loc eexpr

type meta = { tag : tag; loc : loc }

(* A source file: an optional (program ...) header around one body expression. *)
(* consts: the (const name value) items of the header, in order: emitted as EQU lines. *)
type source = { optimize : string list option; expects : expectation list; consts : (string * Red.rexpr) list; body : expr }

(* Every node gets a tag in pre-order from 1; labels are named after tags, so this numbering is
   part of the output and must not change. A comment takes none itself, but as an element of a seq
   it advances the count like any element; an expectation takes none anywhere. *)
let rec tag_expr_help (e : expr) (cur : tag) : (meta eexpr * tag) =
  match e with
  | EComment (s) ->
    EComment (s), cur
  | ELabel (s, loc) ->
    (ELabel (s, { tag = cur; loc }), cur + 1)
  | EPrim2 (op, m, a1, a2, loc) ->
    (EPrim2 (op, m, a1, a2, { tag = cur; loc }), cur + 1)
  | EFlow1 (op, cond, expr, loc) ->
    let (tag_expr, next_tag) = tag_expr_help expr (cur + 1) in
    (EFlow1 (op, cond, tag_expr, { tag = cur; loc }), next_tag)
  | EFlow2 (op, cond, expr1, expr2, loc) ->
    let (tag_expr1, next_tag1) = tag_expr_help expr1 (cur + 1) in
    let (tag_expr2, next_tag2) = tag_expr_help expr2 next_tag1 in
    (EFlow2 (op, cond, tag_expr1, tag_expr2, { tag = cur; loc }), next_tag2)
  | ELet (x, a, expr, loc) ->
    let (tag_expr, next_tag) = tag_expr_help expr (cur + 1) in
    (ELet (x, a, tag_expr, { tag = cur; loc }), next_tag)
  | ESeq (exprs, loc) ->
    let rec tag_seq (exprs : expr list) (cur : tag) : meta eexpr list * tag =
      (match exprs with
      | EExpect (x, loc) :: tail ->
        (* An expectation emits nothing and takes no tag of its own, so adding one never
           renumbers the labels generated after it. *)
        let (tag_tail, next_tag) = tag_seq tail cur in
        EExpect (x, { tag = cur; loc }) :: tag_tail, next_tag
      | head :: tail ->
        let (tag_head, next_tag1) = tag_expr_help head (cur + 1) in
        let (tag_tail, next_tag2) = tag_seq tail next_tag1 in
        [tag_head] @ tag_tail, next_tag2
      | [] -> [], cur ) in
    let (tag_e, next_tag) = tag_seq exprs (cur + 1) in
    (ESeq (tag_e, { tag = cur; loc }), next_tag)
  | EExpect (x, loc) ->
    (EExpect (x, { tag = cur; loc }), cur)

let tag_expr (e : expr) : meta eexpr =
  let (tagged, _) = tag_expr_help e 1 in tagged


(* Pretty printing - used by testing framework *)
let string_of_arg(a : arg) : string =
  match a with
  | ANone -> "0"
  | AStore s -> s
  | ANum n -> Int.to_string n
  | AId s -> s
  | ARef (_, n) -> Int.to_string n
  | ALab (_, s) -> s
  | AExp (_, e) -> Red.pp_rexpr e
