(* Lib *)


type aenv = (string * string) list

let empty_aenv : aenv = []

let extend_aenv (x : string) (label : string) (aenv : aenv) : aenv =
  ((x, label) :: aenv)
