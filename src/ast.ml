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

type cond =
| Cond0
| Cond1 of cond1 * arg
| Cond2 of cond2 * arg * arg


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


type flow1 =
| Repeat
| If
| While
| DoWhile

type flow2 =
| IfElse


type expr =
| Comment of string
| Label of string
| Prim2 of prim2 * imod * arg * arg
| Flow1 of flow1 * cond * expr
| Flow2 of flow2 * cond * expr * expr
| Let of string * arg * expr
| Seq of expr list

type 'a eexpr =
| EComment of string
| ELabel of string * 'a
| EPrim2 of prim2 * imod * arg * arg * 'a
| EFlow1 of flow1 * cond * 'a eexpr * 'a
| EFlow2 of flow2 * cond * 'a eexpr * 'a eexpr * 'a
| ELet of string * arg * 'a eexpr * 'a
| ESeq of 'a eexpr list * 'a


type tag = int

let rec tag_expr_help (e : expr) (cur : tag) : (tag eexpr * tag) =
  match e with
  | Comment (s) ->
    EComment (s), cur
  | Label (s) ->
    let (next_tag) = (cur + 1) in
    (ELabel (s, cur), next_tag)
  | Prim2 (op, m, a1, a2) ->
    let (next_tag) = (cur + 1) in
    (EPrim2 (op, m, a1, a2, cur), next_tag)
  | Flow1 (op, cond, expr) ->
    let (tag_expr, next_tag) = tag_expr_help expr (cur + 1) in
    (EFlow1 (op, cond, tag_expr, cur), next_tag)
  | Flow2 (op, cond, expr1, expr2) ->
    let (tag_expr1, next_tag1) = tag_expr_help expr1 (cur + 1) in
    let (tag_expr2, next_tag2) = tag_expr_help expr2 next_tag1 in
    (EFlow2 (op, cond, tag_expr1, tag_expr2, cur), next_tag2)
  | Let (x, a, expr) ->
    let (tag_expr, next_tag) = tag_expr_help expr (cur + 1) in
    (ELet (x, a, tag_expr, cur), next_tag)
  | Seq (exprs) ->
    let rec tag_seq (exprs : expr list) (cur : tag) : tag eexpr list * tag =
      (match exprs with
      | head :: tail ->
        let (tag_head, next_tag1) = tag_expr_help head (cur + 1) in
        let (tag_tail, next_tag2) = tag_seq tail next_tag1 in
        [tag_head] @ tag_tail, next_tag2
      | [] -> [], cur ) in
    let (tag_e, next_tag) = tag_seq exprs (cur + 1) in
    (ESeq (tag_e, cur), next_tag)

let tag_expr (e : expr) : tag eexpr =
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
