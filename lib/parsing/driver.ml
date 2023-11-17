open! Common

let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.file

let try_parse lexbuf parser =
  try
    let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
    parser lexer
  with Parser.Error state ->
    let loc = Loc.loc_of_lexbuf lexbuf in
    let msg = ParserMessages.message state in
    Error.error_with_loc (Syntax (ParsingError msg)) loc

let parse_file filename =
  let in_channel = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel in_channel in
  Sedlexing.set_filename lexbuf filename;
  try_parse lexbuf parser

let parse_stdin () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  Sedlexing.set_filename lexbuf "/dev/stdin";
  try_parse lexbuf parser

let parse_string str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  try_parse lexbuf parser
