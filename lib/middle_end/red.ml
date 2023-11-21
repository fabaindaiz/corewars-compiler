(** RED **)
open Printf


(* addressing modes *)
type rmode =
| RImm  (* immediate *)
| RDir  (* direct *)
| RAInd (* A-field indirect *)
| RBInd (* B-field indirect *)
| RADec (* A-field indirect with predecrement *)
| RBDec (* B-field indirect with predecrement *)
| RAInc (* A-field indirect with postincrement *)
| RBInc (* B-field indirect with postincrement *)

(* addressing modes to string *)
let pp_mode (rmode : rmode) : string =
  match rmode with
  | RImm -> "#"
  | RDir -> "$"
  | RAInd -> "*"
  | RBInd -> "@"
  | RADec -> "{"
  | RBDec -> "<"
  | RAInc -> "}"
  | RBInc -> ">"


(* red arguments for opcodes *)
type rarg =
| RNone                  (* none arg *)
| RRef of rmode * int    (* number arg *)
| RLab of rmode * string (* string arg *)

(* rarguments for instruction to string *)
let pp_rarg (rarg : rarg) : string =
  match rarg with
  | RNone       -> sprintf "#%-6s"  (Int.to_string 0)
  | RRef (m, n) -> sprintf "%s%-6s" (pp_mode m) (Int.to_string n)
  | RLab (m, l) -> sprintf "%s%-6s" (pp_mode m) (l)


(* instruction modifiers *)
type rmod =
| RN  (* no mod *)
| RA  (* A to A *)
| RB  (* B to B *)
| RAB (* A to B *)
| RBA (* B to A *)
| RF  (* AB to AB *)
| RX  (* AB to BA *)
| RI  (* instr to instr *)

(* instruction modifiers to string *)
let pp_rmod (rmod : rmod) : string = 
  match rmod with
  | RN  -> "   "
  | RA  -> ".A "
  | RB  -> ".B "
  | RAB -> ".AB"
  | RBA -> ".BA"
  | RF  -> ".F "
  | RX  -> ".X "
  | RI  -> ".I "


type opcode =    
| IDAT (* data *)
| ISPL (* split *)
| IJMP (* jump *)
| INOP (* no operation *)
| IMOV (* move *)
| IADD (* add *)
| ISUB (* subtract *)
| IMUL (* multiply *)
| IDIV (* divide *)
| IMOD (* modulus *)   
| IJMZ (* jump if zero *)
| IJMN (* jump if not zero *)
| IDJN (* decrement and jump if not zero *)
| ICMP (* skip if equal *)
| ISEQ (* skip if equal *)
| ISNE (* skip if not equal *)
| ISLT (* skip if lower than *)
| ILDP (* load from p-space *)
| ISTP (* save to p-space *)

let pp_opcode (op : opcode) : string =
  match op with
  | IDAT -> "DAT"
  | ISPL -> "SPL"
  | IJMP -> "JMP"
  | INOP -> "NOP"
  | IMOV -> "MOV"
  | IADD -> "ADD"
  | ISUB -> "SUB"
  | IMUL -> "MUL"
  | IDIV -> "DIV"
  | IMOD -> "MOD"  
  | IJMZ -> "JMZ"
  | IJMN -> "JMN"
  | IDJN -> "DJN"
  | ICMP -> "CMP"
  | ISEQ -> "SEQ"
  | ISNE -> "SNE"
  | ISLT -> "SLT"
  | ILDP -> "LDP"
  | ISTP -> "STP"


(* red opcode *)
type instruction =
| ICOM of string        (* comment *)
| ILAB of string        (* label *)
| INSTR of opcode * rmod * rarg * rarg

(* red opcode to string *)
let pp_instruction (opcode : instruction) : string =
  match opcode with
  | ICOM (s)             -> sprintf ";%-6s" (s)
  | ILAB (l)             -> sprintf "%-6s" (l)
  | INSTR (op, m, e1, e2) -> sprintf "  %s%s %s, %s" (pp_opcode op) (pp_rmod m) (pp_rarg e1) (pp_rarg e2)


(* red instruction list to string *)
let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\r\n" ^ (pp_instruction i)) "" instrs
