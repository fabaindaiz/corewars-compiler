(** AST **)
open Printf

type mode =
| Ind
| Pre
| Pos

type arg =
| Num of int
| Id of string


type expr =
| Mov of arg * arg