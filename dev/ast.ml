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

type expr =
| Dat of arg * arg
| Label of string
| Point of string
| Mov of arg * arg
| Add of arg * arg
| Sub of arg * arg
| Mul of arg * arg
| Div of arg * arg
| Mod of arg * arg
| Jmp of arg * arg
| Spl of arg * arg
| Nop
| Let of string * place *arg * expr
| Seq of expr list
| Repeat of expr
| If of cond * expr
| While of cond * expr
| Dowhile of cond * expr
