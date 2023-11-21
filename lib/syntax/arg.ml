type t =
  | None
  | Store of string
  | Num of int
  | Id of string
  | Ref of mode * int
  | Lab of mode * string

and mode =
  | Imm
  | Dir
  | Ind
  | Dec
  | Inc

let type_of = function
  | _ -> Type.Arg

let to_string_mode =
  function
  | Imm -> "Imm"
  | Dir -> "Dir"
  | Ind -> "Ind"
  | Dec -> "Dec"
  | Inc -> "Inc"

let to_string =
  function
  | None -> "None"
  | Store s -> "Store " ^ s
  | Num n -> "Num " ^ string_of_int n
  | Id s -> "Id " ^ s
  | Ref (m, n) -> "Ref (" ^ to_string_mode m ^ ", " ^ string_of_int n ^ ")"
  | Lab (m, s) -> "Lab (" ^ to_string_mode m ^ ", " ^ s ^ ")"

let pp ftm =
  let open Format in
  function
  | None -> fprintf ftm "None"
  | Store s -> fprintf ftm "Store %s" s
  | Num n -> fprintf ftm "Num %d" n
  | Id s -> fprintf ftm "Id %s" s
  | Ref (m, n) -> fprintf ftm "Ref (%s, %d)" (to_string_mode m) n
  | Lab (m, s) -> fprintf ftm "Lab (%s, %s)" (to_string_mode m) s
