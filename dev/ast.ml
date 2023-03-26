(** AST **)
open Printf


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

type expr =
(*| Dat of aarg * aarg*)
| Mov of aarg * aarg
| Let of string * aarg * expr
| Repeat of expr
