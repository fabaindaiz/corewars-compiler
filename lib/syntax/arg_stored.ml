type t =
  | Ref of mode * int    (* number variable *)
  | Lab of mode * string (* label variable *)
  | Var of mode * place * string (* direct reference *)
  | Pnt of mode * place * place * string (* indirect reference *)

and mode =
  | Imm
  | Dir
  | Ind
  | Dec
  | Inc

and place =
  | A
  | B

let type_of = function
  | _ -> Type.Arg

let to_string_place =
  function
  | A -> "A"
  | B -> "B"

let to_string_mode =
  function
  | Imm -> "Imm"
  | Dir -> "Dir"
  | Ind -> "Ind"
  | Dec -> "Dec"
  | Inc -> "Inc"

let to_string =
  function
  | Ref (m, n) -> Printf.sprintf "Ref (%s, %d)" (to_string_mode m) n
  | Lab (m, s) -> Printf.sprintf "Lab (%s, %s)" (to_string_mode m) s
  | Var (m, p, s) -> Printf.sprintf "Var (%s, %s, %s)" (to_string_mode m) (to_string_place p) s
  | Pnt (m, p1, p2, s) -> Printf.sprintf "Pnt (%s, %s, %s, %s)" (to_string_mode m) (to_string_place p1) (to_string_place p2) s

let pp ftm =
  let open Format in
  function
  | Ref (m, n) -> fprintf ftm "Ref (%s, %d)" (to_string_mode m) n
  | Lab (m, s) -> fprintf ftm "Lab (%s, %s)" (to_string_mode m) s
  | Var (m, p, s) -> fprintf ftm "Var (%s, %s, %s)" (to_string_mode m) (to_string_place p) s
  | Pnt (m, p1, p2, s) -> fprintf ftm "Pnt (%s, %s, %s, %s)" (to_string_mode m) (to_string_place p1) (to_string_place p2) s
