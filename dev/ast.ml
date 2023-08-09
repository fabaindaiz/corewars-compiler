(** AST **)


type place =
| A
| B

type mode =
| ADir
| AInd
| ADec
| AInc

type arg =
| ANone
| Num of int
| Id of string
| Ref of mode * int
| Lab of mode * string
| Place of string

type cond =
| Cjz of arg
| Cjn of arg
| Cdn of arg
| Ceq of arg * arg
| Cne of arg * arg
| Cgt of arg * arg
| Clt of arg * arg

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

type expr =
| Label of string
| Prim2 of prim2 * arg * arg
| Nop
| Let of string * place *arg * expr
| Seq of expr list
| Repeat of expr
| If of cond * expr
| While of cond * expr
| Dowhile of cond * expr
