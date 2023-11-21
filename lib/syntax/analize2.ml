
let compile_mode (m : mode) (p : place) : Red.rmode =
  match m, p with
  | Imm, _ -> Imm
  | Dir, _ -> Dir
  | Ind, A -> AInd
  | Ind, B -> BInd
  | Dec, A -> ADec
  | Dec, B -> BDec
  | Inc, A -> AInc
  | Inc, B -> BInc


type carg =
| Ref of mode * int    (* number variable *)
| Lab of mode * string (* label variable *)
| Var of mode * string (* direct reference *)
| Pnt of mode * string (* indirect reference *)

let resolve_arg (m : mode) (s: string) : Red.carg =
  None

let arg_to_carg (a: arg) : carg =
  match a with
  | None -> Ref (Imm, 0)
  | Num n -> Ref (Imm, n)
  | Ref (m, n) -> Ref (m, n)
  | Id (s) -> resolve_arg Dir s
  | Lab (m, s) -> resolve_arg m s


let carg_to_rarg (c: carg) : Red.rarg =
  match c with
  | Ref (m, n) -> Red.Ref (compile_mode m B, n)
  | Lab (m, s) -> Red.Lab (compile_mode m B, s)
  | Var (m, s) -> Red.Var (compile_mode m A, s)
  | Pnt (m, s) -> Red.Pnt (compile_mode m A, s)