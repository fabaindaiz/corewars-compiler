(** RED **)
open Printf


(* addressing modes *)
type mode =
| Imm (* immediate *)
| Dir (* direct *)
| AInd (* A-field indirect *)
| BInd (* B-field indirect *)
| APre (* A-field indirect with predecrement *)
| BPre (* B-field indirect with predecrement *)
| APos (* A-field indirect with postincrement *)
| BPos (* B-field indirect with postincrement *)

(* arguments for opcodes *)
type arg =
| Const of int
| Ref of mode * int
| Label of mode * string

(* instruction modifiers *)
type mods = 
| A
| B
| AB
| BA
| F
| X
| I

(* red opcode *)
type opcode =
| DAT of arg * arg (* data *)
| MOV of mods * arg * arg (* move *)
| ADD of mods * arg * arg (* add *)
| SUB of mods * arg * arg (* subtract *)
| MUL of mods * arg * arg (* multiply *)
| DIV of mods * arg * arg (* divide *)
| MOD of mods * arg * arg (* modulus *)
| JMP of arg (* jump *)
| JMZ of mods * arg * arg (* jump if zero *)
| JMN of mods * arg * arg (* jump if not zero *)
| DJN of mods * arg * arg (* decrement and jump if not zero *)
| CMP of mods * arg * arg (* skip if equal *)
| SEQ of mods * arg * arg (* skip if equal *)
| SNE of mods * arg * arg (* skip if not equal *)
| SLT of mods * arg * arg (* skip if lower than *)
| SPL of arg (* split *)
| LDP of mods * arg * arg (* load from p-space *)
| STP of mods * arg * arg (* save to p-space *)
| NOP (* no operation *)

(* red instruction *)
type instruction =
| Instr of string * opcode


(* addressing modes to string *)
let pp_mode (mode : mode) : string =
  match mode with
  | Imm  -> "#"
  | Dir  -> "$"
  | AInd -> "*"
  | BInd -> "@"
  | APre -> "{"
  | BPre -> "<"
  | APos -> "}"
  | BPos -> ">"

(* arguments for instruction to string *)
let pp_arg (arg : arg) : string =
  match arg with
  | Const (n)    -> sprintf " %-6s"  (Int.to_string n)
  | Ref (m, n)   -> sprintf "%s%-6s" (pp_mode m) (Int.to_string n)
  | Label (m, l) -> sprintf "%s%-6s" (pp_mode m) (l)

(* instruction modifiers to string *)
let pp_mods (mods : mods) : string = 
  match mods with
  | A  -> ".A "
  | B  -> ".B "
  | AB -> ".AB"
  | BA -> ".BA"
  | F  -> ".F "
  | X  -> ".X "
  | I  -> ".I "

(* red opcode to string *)
let pp_opcode (opcode : opcode) : string =
  match opcode with
  | DAT (e1, e2)    -> sprintf "DAT    %s, %s"            (pp_arg e1) (pp_arg e2)
  | MOV (m, e1, e2) -> sprintf "MOV%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | ADD (m, e1, e2) -> sprintf "ADD%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | SUB (m, e1, e2) -> sprintf "SUB%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | MUL (m, e1, e2) -> sprintf "MUL%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | DIV (m, e1, e2) -> sprintf "DIV%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | MOD (m, e1, e2) -> sprintf "MOD%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | JMP (e1)        -> sprintf "JMP    %s"                (pp_arg e1)
  | JMZ (m, e1, e2) -> sprintf "JMZ%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | JMN (m, e1, e2) -> sprintf "JMN%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | DJN (m, e1, e2) -> sprintf "DJN%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | CMP (m, e1, e2) -> sprintf "CMP%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | SEQ (m, e1, e2) -> sprintf "SEQ%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | SNE (m, e1, e2) -> sprintf "SNE%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | SLT (m, e1, e2) -> sprintf "SLT%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | SPL (e1)        -> sprintf "SPL    %s"                (pp_arg e1)
  | LDP (m, e1, e2) -> sprintf "LDP%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | STP (m, e1, e2) -> sprintf "STP%s %s, %s" (pp_mods m) (pp_arg e1) (pp_arg e2)
  | NOP ->                     "NOP"

(* red instruction to string *)
let pp_instr (instruction : instruction) : string =
  match instruction with
  | Instr (l, op) -> sprintf "%-6s %s" (l) (pp_opcode op)

(* red instruction list to string *)
let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\n" ^ (pp_instr i)) "" instrs
