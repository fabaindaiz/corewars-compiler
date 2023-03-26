(** Compiler **)
open Printf
open Red
open Ast


let compile_arg (arg : arg) : rarg =
  match arg with
  | Num (n) -> Const (n)

let compile_expr (e : expr) : instruction list =
  match e with
  | Mov (e1, e2) -> 
    let a1 = (compile_arg e1) in
    let a2 = (compile_arg e2) in
    [IMOV ("", MI, a1, a2)]


let prelude = sprintf "
;redcode
"

let compile_prog (e : expr) : string =
  let instrs = (compile_expr e) in
  (prelude) ^ (pp_instrs instrs)