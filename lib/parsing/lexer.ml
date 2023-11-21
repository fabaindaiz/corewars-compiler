open Common
open Parser

let lexeme = Sedlexing.Utf8.lexeme
let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let lower = [%sedlex.regexp? 'a' .. 'z']
let upper = [%sedlex.regexp? 'A' .. 'Z']
let alpha = [%sedlex.regexp? lower | upper]
let id0 = [%sedlex.regexp? alpha | '_']
let id = [%sedlex.regexp? id0, Star (alpha | digit | '_')]
let whitespace = [%sedlex.regexp? Plus ('\t' | ' ')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let lparen = [%sedlex.regexp? '(']
let rparen = [%sedlex.regexp? ')']
let dot = [%sedlex.regexp? '.']
let plus = [%sedlex.regexp? '+']
let minus = [%sedlex.regexp? '-']
let times = [%sedlex.regexp? '*']
let equal = [%sedlex.regexp? '=']
let and_ = [%sedlex.regexp? "and"]
let barbar = [%sedlex.regexp? "||"]
let ampersandampersand = [%sedlex.regexp? "&&"]
let or_ = [%sedlex.regexp? "or"]
let not = [%sedlex.regexp? '!']
let less = [%sedlex.regexp? '<']
let arrow = [%sedlex.regexp? "->"]
let if_ = [%sedlex.regexp? "if"]
let then_ = [%sedlex.regexp? "then"]
let else_ = [%sedlex.regexp? "else"]
let fun_ = [%sedlex.regexp? "fun"]
let let_ = [%sedlex.regexp? "let"]
let rec_ = [%sedlex.regexp? "rec"]
let in_ = [%sedlex.regexp? "in"]
let true_ = [%sedlex.regexp? "true"]
let false_ = [%sedlex.regexp? "false"]
let comma = [%sedlex.regexp? ',']
let dot = [%sedlex.regexp? '.']
let colon = [%sedlex.regexp? ':']
let coloncolon = [%sedlex.regexp? "::"]
let bool = [%sedlex.regexp? "bool"]
let int = [%sedlex.regexp? "int"]
let dat = [%sedlex.regexp? "DAT"]
let jmp = [%sedlex.regexp? "JMP"]
let spl = [%sedlex.regexp? "SPL"]
let nop = [%sedlex.regexp? "NOP"]
let mov = [%sedlex.regexp? "MOV"]
let add = [%sedlex.regexp? "ADD"]
let sub = [%sedlex.regexp? "SUB"]
let mul = [%sedlex.regexp? "MUL"]
let div = [%sedlex.regexp? "DIV"]
let mod_ = [%sedlex.regexp? "MOD"]
let jmz = [%sedlex.regexp? "JMZ"]
let jmn = [%sedlex.regexp? "JMN"]
let djn = [%sedlex.regexp? "DJN"]
let seq = [%sedlex.regexp? "SEQ"]
let sne = [%sedlex.regexp? "SNE"]
let slt = [%sedlex.regexp? "SLT"]
let stp = [%sedlex.regexp? "STP"]
let ldp = [%sedlex.regexp? "LDP"]
let def = [%sedlex.regexp? "DEF"]

let rec token lexbuf =
  match%sedlex lexbuf with
  | lparen -> LPAREN
  | rparen -> RPAREN
  | plus -> PLUS
  | minus -> MINUS
  | times -> TIMES
  | equal -> EQUAL
  | and_ -> AND
  | barbar -> BARBAR
  | ampersandampersand -> AMPERSANDAMPERSAND
  | not -> NOT
  | less -> LESS
  | arrow -> ARROW
  | bool -> TBOOL
  | int -> TINT
  | if_ -> IF
  | then_ -> THEN
  | else_ -> ELSE
  | fun_ -> FUN
  | let_ -> LET
  | rec_ -> REC
  | in_ -> IN
  | true_ -> TRUE
  | false_ -> FALSE
  | comma -> COMMA
  | dot -> DOT
  | colon -> COLON
  | coloncolon -> COLONCOLON
  | dat -> DAT
  | jmp -> JMP
  | spl -> SPL
  | nop -> NOP
  | mov -> MOV
  | add -> ADD
  | sub -> SUB
  | mul -> MUL
  | div -> DIV
  | mod_ -> MOD
  | jmz -> JMZ
  | jmn -> JMN
  | djn -> DJN
  | seq -> SEQ
  | sne -> SNE
  | slt -> SLT
  | stp -> STP
  | ldp -> LDP
  | id -> ID (Sedlexing.Utf8.lexeme lexbuf)
  | number -> INT (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
  | whitespace -> token lexbuf
  | newline ->
      Sedlexing.new_line lexbuf;
      token lexbuf
  | eof -> EOF
  | any ->
      let loc = Loc.loc_of_lexbuf lexbuf in
      let str = Sedlexing.Utf8.lexeme lexbuf in
      Error.error_with_loc (Syntax (UnexpectedToken str)) loc
  | _ -> failwith "unreachable"
