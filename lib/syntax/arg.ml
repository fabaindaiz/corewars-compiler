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
| Inc
| Dec

let type_of = function
  | _ -> Type.Arg

let to_string_mode = function
  | Imm -> "Imm "
  | Dir -> "Dir "
  | Ind -> "Ind "
  | Inc -> "Inc "
  | Dec -> "Dec "

let to_string = function
  | None -> "None"
  | Store s -> "Store " ^ s
  | Num n -> "Num " ^ (string_of_int n)
  | Id s -> "Id " ^ s
  | Ref (m, n) -> "Ref " ^ (to_string_mode m) ^ (string_of_int n)
  | Lab (m, s) -> "Lab " ^ (to_string_mode m) ^ s

let pp fmt const = Format.fprintf fmt "%s" (to_string const)
