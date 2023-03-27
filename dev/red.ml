(** RED **)
open Printf


(* addressing modes *)
type rmode =
| MImm (* immediate *)
| MDir (* direct *)
| AInd (* A-field indirect *)
| BInd (* B-field indirect *)
| APre (* A-field indirect with predecrement *)
| BPre (* B-field indirect with predecrement *)
| APos (* A-field indirect with postincrement *)
| BPos (* B-field indirect with postincrement *)

(* red arguments for opcodes *)
type rarg =
| RNum of int
| RId of string
| RRef of rmode * int
| RLab of rmode * string

(* instruction modifiers *)
type rmod = 
| MA
| MB
| MAB
| MBA
| MF
| MX
| MI

(* red opcode *)
type instruction =
| IDAT of string * rarg * rarg (* data *)
| IMOV of string * rmod * rarg * rarg (* move *)
| IADD of string * rmod * rarg * rarg (* add *)
| ISUB of string * rmod * rarg * rarg (* subtract *)
| IMUL of string * rmod * rarg * rarg (* multiply *)
| IDIV of string * rmod * rarg * rarg (* divide *)
| IMOD of string * rmod * rarg * rarg (* modulus *)
| IJMP of string * rarg (* jump *)
| IJMZ of string * rmod * rarg * rarg (* jump if zero *)
| IJMN of string * rmod * rarg * rarg (* jump if not zero *)
| IDJN of string * rmod * rarg * rarg (* decrement and jump if not zero *)
| ICMP of string * rmod * rarg * rarg (* skip if equal *)
| ISEQ of string * rmod * rarg * rarg (* skip if equal *)
| ISNE of string * rmod * rarg * rarg (* skip if not equal *)
| ISLT of string * rmod * rarg * rarg (* skip if lower than *)
| ISPL of string * rarg (* split *)
| ILDP of string * rmod * rarg * rarg (* load from p-space *)
| ISTP of string * rmod * rarg * rarg (* save to p-space *)
| INOP of string (* no operation *)
| ILabel of string


(* addressing modes to string *)
let pp_mode (mode : rmode) : string =
  match mode with
  | MImm -> "#"
  | MDir -> "$"
  | AInd -> "*"
  | BInd -> "@"
  | APre -> "{"
  | BPre -> "<"
  | APos -> "}"
  | BPos -> ">"

(* rarguments for instruction to string *)
let pp_rarg (rarg : rarg) : string =
  match rarg with
  | RNum (n)    -> sprintf "#%-6s"  (Int.to_string n)
  | RId  (l)    -> sprintf " %-6s"  (l)
  | RRef (m, n) -> sprintf "%s%-6s" (pp_mode m) (Int.to_string n)
  | RLab (m, l) -> sprintf "%s%-6s" (pp_mode m) (l)

(* instruction modifiers to string *)
let pp_rmod (rmod : rmod) : string = 
  match rmod with
  | MA  -> ".A "
  | MB  -> ".B "
  | MAB -> ".AB"
  | MBA -> ".BA"
  | MF  -> ".F "
  | MX  -> ".X "
  | MI  -> ".I "

(* red opcode to string *)
let pp_instr (opcode : instruction) : string =
  match opcode with
  | IDAT (l, e1, e2)    -> sprintf "%-6s DAT    %s, %s" (l)             (pp_rarg e1) (pp_rarg e2)
  | IMOV (l, m, e1, e2) -> sprintf "%-6s MOV%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IADD (l, m, e1, e2) -> sprintf "%-6s ADD%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISUB (l, m, e1, e2) -> sprintf "%-6s SUB%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IMUL (l, m, e1, e2) -> sprintf "%-6s MUL%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IDIV (l, m, e1, e2) -> sprintf "%-6s DIV%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IMOD (l, m, e1, e2) -> sprintf "%-6s MOD%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IJMP (l, e1)        -> sprintf "%-6s JMP    %s"     (l)             (pp_rarg e1)
  | IJMZ (l, m, e1, e2) -> sprintf "%-6s JMZ%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IJMN (l, m, e1, e2) -> sprintf "%-6s JMN%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IDJN (l, m, e1, e2) -> sprintf "%-6s DJN%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ICMP (l, m, e1, e2) -> sprintf "%-6s CMP%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISEQ (l, m, e1, e2) -> sprintf "%-6s SEQ%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISNE (l, m, e1, e2) -> sprintf "%-6s SNE%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISLT (l, m, e1, e2) -> sprintf "%-6s SLT%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISPL (l, e1)        -> sprintf "%-6s SPL    %s"     (l)             (pp_rarg e1)
  | ILDP (l, m, e1, e2) -> sprintf "%-6s LDP%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISTP (l, m, e1, e2) -> sprintf "%-6s STP%s %s, %s"  (l) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | INOP (l)            -> sprintf "%-6s NOP"           (l)
  | ILabel (l)          -> sprintf "%-6s"               (l)

(* red instruction list to string *)
let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\n" ^ (pp_instr i)) "" instrs
