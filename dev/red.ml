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
  | RA  -> ".A "
  | RB  -> ".B "
  | RAB -> ".AB"
  | RBA -> ".BA"
  | RF  -> ".F "
  | RX  -> ".X "
  | RI  -> ".I "


(* red opcode *)
type instruction =
| ILabel of string
| IDAT of rarg * rarg        (* data *)
| IMOV of rmod * rarg * rarg (* move *)
| IADD of rmod * rarg * rarg (* add *)
| ISUB of rmod * rarg * rarg (* subtract *)
| IMUL of rmod * rarg * rarg (* multiply *)
| IDIV of rmod * rarg * rarg (* divide *)
| IMOD of rmod * rarg * rarg (* modulus *)
| ISPL of rarg * rarg        (* split *)
| IJMP of rarg * rarg        (* jump *)
| IJMZ of rmod * rarg * rarg (* jump if zero *)
| IJMN of rmod * rarg * rarg (* jump if not zero *)
| IDJN of rmod * rarg * rarg (* decrement and jump if not zero *)
| ICMP of rmod * rarg * rarg (* skip if equal *)
| ISEQ of rmod * rarg * rarg (* skip if equal *)
| ISNE of rmod * rarg * rarg (* skip if not equal *)
| ISLT of rmod * rarg * rarg (* skip if lower than *)
| ILDP of rmod * rarg * rarg (* load from p-space *)
| ISTP of rmod * rarg * rarg (* save to p-space *)
| INOP of rarg * rarg        (* no operation *)

(* red opcode to string *)
let pp_instr (opcode : instruction) : string =
  match opcode with
  | ILabel (l)       -> sprintf "%-6s" (l)
  | IDAT (e1, e2)    -> sprintf "  DAT    %s, %s"            (pp_rarg e1) (pp_rarg e2)
  | IMOV (m, e1, e2) -> sprintf "  MOV%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IADD (m, e1, e2) -> sprintf "  ADD%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISUB (m, e1, e2) -> sprintf "  SUB%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IMUL (m, e1, e2) -> sprintf "  MUL%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IDIV (m, e1, e2) -> sprintf "  DIV%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IMOD (m, e1, e2) -> sprintf "  MOD%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISPL (e1, e2)    -> sprintf "  SPL    %s, %s"            (pp_rarg e1) (pp_rarg e2)
  | IJMP (e1, e2)    -> sprintf "  JMP    %s, %s"            (pp_rarg e1) (pp_rarg e2)
  | IJMZ (m, e1, e2) -> sprintf "  JMZ%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IJMN (m, e1, e2) -> sprintf "  JMN%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | IDJN (m, e1, e2) -> sprintf "  DJN%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ICMP (m, e1, e2) -> sprintf "  CMP%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISEQ (m, e1, e2) -> sprintf "  SEQ%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISNE (m, e1, e2) -> sprintf "  SNE%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISLT (m, e1, e2) -> sprintf "  SLT%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ILDP (m, e1, e2) -> sprintf "  LDP%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | ISTP (m, e1, e2) -> sprintf "  STP%s %s, %s" (pp_rmod m) (pp_rarg e1) (pp_rarg e2)
  | INOP (e1, e2)    -> sprintf "  NOP    %s, %s"            (pp_rarg e1) (pp_rarg e2)


(* red instruction list to string *)
let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\r\n" ^ (pp_instr i)) "" instrs
