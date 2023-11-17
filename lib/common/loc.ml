type t = { pos_start : Lexing.position; pos_end : Lexing.position }

let loc_of_positions (pos_start : Lexing.position) (pos_end : Lexing.position) =
  { pos_start; pos_end }

let loc_of_lexbuf lexbuf =
  let pos_start, pos_end = Sedlexing.lexing_positions lexbuf in
  let loc = loc_of_positions pos_start pos_end in
  loc

let pp fmt { pos_start; pos_end } =
  let source = pos_start.pos_fname in
  let start_line = pos_start.pos_lnum in
  let start_ = pos_start.pos_cnum - pos_start.pos_bol in
  let end_ = pos_end.pos_cnum - pos_end.pos_bol in
  Format.fprintf fmt "file %s, line %d, characters %d-%d" source start_line
    start_ end_
