(** AST **)


type mode =
| Dir
| Ind

type arg =
| Num of int
| Id of string
| Ref of mode * int
| Lab of mode * string

type act =
| ANot
| ADec
| AInc

type aarg =
| Not of arg
| Dec of arg
| Inc of arg

type cond =
| Cjz of aarg
| Cjn of aarg
| Cdn of aarg
| Ceq of aarg * aarg
| Cne of aarg * aarg
| Cgt of aarg * aarg
| Clt of aarg * aarg

type expr =
(*| Dat of aarg * aarg*)
| Mov of aarg * aarg
| Sub of aarg * aarg
| Let of string * aarg * expr
| Repeat of expr
| Seq of expr list
| If of cond * expr
| While of cond * expr
| Dowhile of cond * expr
| Label of string
