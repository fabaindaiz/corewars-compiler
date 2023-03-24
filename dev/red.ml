(** RED **)
open Printf


type mode =
| Imm (* immediate *)
| Dir (* direct *)
| AInd (* A-field indirect *)
| BInd (* B-field indirect *)
| APre (* A-field indirect with predecrement *)
| BPre (* B-field indirect with predecrement *)
| APos (* A-field indirect with postincrement *)
| BPos (* B-field indirect with postincrement *)

type arg =
| Const of int64
| Ref of mode * int64
| Label of mode * string

type mods = 
| A
| B
| AB
| BA
| F
| X
| I

(* red instructions *)
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

type instructions =
| Instr of string * opcode