(** AST **)


type place =
| PA
| PB


type mode =
| MDir
| MInd
| MDec
| MInc

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
| Cdn

type cond2 =
| Ceq
| Cne
| Cgt
| Clt

type cond =
| Cond1 of cond1 * arg
| Cond2 of cond2 * arg * arg


type prim2 =
| Dat
| Mov
| Add
| Sub
| Mul
| Div
| Mod
| Jmp
| Spl

type cont1 =
| If
| While
| Dowhile


type expr =
| Nop
| Label of string
| Prim2 of prim2 * arg * arg
| Cont1 of cont1 * cond * expr
| Let of string * arg * expr
| Repeat of expr
| Seq of expr list

type 'a eexpr =
| ENop of 'a
| ELabel of string * 'a
| EPrim2 of prim2 * arg * arg * 'a
| ECont1 of cont1 * cond * 'a eexpr * 'a
| ELet of string * arg * 'a eexpr * 'a
| ERepeat of 'a eexpr * 'a
| ESeq of 'a eexpr list * 'a


type tag = int


let rec tag_expr_help (e : expr) (cur : tag) : (tag eexpr * tag) =
  match e with
  | Nop ->
    let (next_tag) = (cur + 1) in
    (ENop (cur), next_tag)
  | Label (s) ->
    let (next_tag) = (cur + 1) in
    (ELabel (s, cur), next_tag)
  | Prim2 (op, a1, a2) ->
    let (next_tag) = (cur + 1) in
    (EPrim2 (op, a1, a2, cur), next_tag)
  | Cont1 (op, c, expr) ->
    let (tag_expr, next_tag) = tag_expr_help expr (cur + 1) in
    (ECont1 (op, c, tag_expr, cur), next_tag)
  | Let (x, a, expr) ->
    let (tag_expr, next_tag) = tag_expr_help expr (cur + 1) in
    (ELet (x, a, tag_expr, cur), next_tag)
  | Repeat (expr) ->
    let (tag_expr, next_tag) = tag_expr_help expr (cur + 1) in
    (ERepeat (tag_expr, cur), next_tag)
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
