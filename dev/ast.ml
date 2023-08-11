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
