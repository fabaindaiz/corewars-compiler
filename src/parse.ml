(** Parser **)
open CCSexp
open Printf
open Ast

(* Locations: the located reader records where each node of a parsed s-expression starts, keyed
   by the node itself (physical identity), so the parser below keeps matching plain sexps and an
   error can still say where. A sexp built in code has no location. *)
module Phys = Hashtbl.Make (struct
  type t = CCSexp.t
  let equal = ( == )
  let hash = Hashtbl.hash
end)

let locations : loc Phys.t = Phys.create 256

let loc_of (s : sexp) : loc option = Phys.find_opt locations s

let nowhere : loc = { line = 0; col = 0 }

let here (s : sexp) : loc = Option.value (loc_of s) ~default:nowhere

module Located = CCSexp.Make (struct
  type t = CCSexp.t
  type nonrec loc = loc
  let atom s = `Atom s
  let list l = `List l
  let match_ t ~atom ~list = match t with `Atom s -> atom s | `List l -> list l
  (* CCSexp gives (line, column from 0); an atom's position is its first character, a list's is
     just past its "(", which is the paren's column counted from 1. *)
  let make_loc = Some (fun (line, col) _ _ -> { line; col })
  let atom_with_loc ~loc s = let t = `Atom s in Phys.replace locations t { loc with col = loc.col + 1 } ; t
  let list_with_loc ~loc l = let t = `List l in Phys.replace locations t loc ; t
end)

(* A user error about [sexp], located when the reader knows where it is. *)
let fail (sexp : sexp) (msg : string) : 'a = raise (Error (loc_of sexp, msg))

(* Names starting with "_" belong to the labels the compiler generates (_LET1, _WHI9, ...), so a
   user name can never collide with one. *)
let user_name (sexp : sexp) (s : string) : string =
  if String.length s > 0 && s.[0] = '_' then
    fail sexp (sprintf "`%s`: names starting with `_` are reserved for the compiler" s)
  else s

(* pMARS reads these as opcodes or pseudo-opcodes in any case (asm.c), never as labels. *)
let pmars_keywords = ["MOV"; "ADD"; "SUB"; "MUL"; "DIV"; "MOD"; "JMZ"; "JMN"; "DJN"; "CMP"; "SLT"; "SPL";
                      "DAT"; "JMP"; "SEQ"; "SNE"; "NOP"; "LDP"; "STP"; "ORG"; "END"; "PIN"; "EQU"; "FOR"; "ROF"]

let label_name (sexp : sexp) (s : string) : string =
  let s = user_name sexp s in
  if List.mem (String.uppercase_ascii s) pmars_keywords then
    fail sexp (sprintf "`%s` is a pMARS keyword and cannot be a label" s)
  else s


let parse_mode (sexp : sexp) : mode =
  match sexp with
  | `Atom "Imm" | `Atom "#" -> MImm
  | `Atom "Dir" | `Atom "$" -> MDir
  | `Atom "Ind" | `Atom "@" -> MInd (MINone)
  | `Atom "Dec" | `Atom "<" -> MInd (MIDec)
  | `Atom "Inc" | `Atom ">" -> MInd (MIInc)
  | _ -> fail sexp (sprintf "Not a valid mode: %s" (to_string sexp))

let parse_arg (sexp : sexp) : arg =
  match sexp with
  | `Atom "none" -> ANone
  | `List [`Atom "store"; `Atom s] | `List [`Atom "!"; `Atom s] -> AStore (user_name sexp s)
  | `Atom s ->
    (match Int64.of_string_opt s with
    | Some n -> ANum (Int64.to_int n)
    | None -> AId (user_name sexp s) )
  | `List [m; (`Atom s as a)] ->
    (match Int64.of_string_opt s with
    | Some n -> ARef ((parse_mode m), (Int64.to_int n))
    | None -> ALab ((parse_mode m), user_name a s) )
  | _ -> fail sexp (sprintf "Not a valid arg: %s" (to_string sexp))


let parse_imod (sexp : sexp) : imod =
  match sexp with
  | `Atom "A" -> MA
  | `Atom "B" -> MB
  | `Atom "AB" -> MAB
  | `Atom "BA" -> MBA
  | `Atom "I" -> MI
  | `Atom "F" -> MF
  | `Atom "X" -> MX
  | _ -> fail sexp (sprintf "Not a valid imod: %s" (to_string sexp))

(* The operator decides the arity, so an optional modifier after it is never ambiguous: (JZ F x) is
   unary with a modifier, (EQ x y) binary without one. *)
let cond1_op (cop : sexp) : cond1 option =
  match cop with
  | `Atom "JZ" -> Some Cjz
  | `Atom "JN" -> Some Cjn
  | `Atom "DZ" -> Some Cdz
  | `Atom "DN" -> Some Cdn
  | _ -> None

let cond2_op (cop : sexp) : cond2 option =
  match cop with
  | `Atom "EQ" -> Some Ceq
  | `Atom "NE" -> Some Cne
  | `Atom "GT" -> Some Cgt
  | `Atom "LT" -> Some Clt
  | _ -> None

let parse_cond (sexp : sexp) : cond =
  let bad kind = fail sexp (sprintf "Not a valid %s cond: %s" kind (to_string sexp)) in
  match sexp with
  | `List [cop; a] ->
    (match cond1_op cop with Some op -> Cond1 (op, MDef, parse_arg a) | None -> bad "unary")
  | `List [cop; x; y] ->
    (match cond1_op cop, cond2_op cop with
    | Some op, _ -> Cond1 (op, parse_imod x, parse_arg y)
    | None, Some op -> Cond2 (op, MDef, parse_arg x, parse_arg y)
    | None, None -> bad "binary")
  | `List [cop; m; a1; a2] ->
    (match cond2_op cop with Some op -> Cond2 (op, parse_imod m, parse_arg a1, parse_arg a2) | None -> bad "binary")
  | _ -> fail sexp (sprintf "Not a valid cond: %s" (to_string sexp))

let parse_int (sexp : sexp) : int =
  match sexp with
  | `Atom s ->
    (match int_of_string_opt s with
    | Some n -> n
    | None -> fail sexp (sprintf "Not a number: %s" s))
  | `List _ -> fail sexp (sprintf "Not a number: %s" (to_string sexp))

let parse_expectation (sexp : sexp) : expectation =
  let measured name n = match name with
    | "length" -> Some (fun c -> XLength (c, n)) | "cycles" -> Some (fun c -> XCycles (c, n))
    | "overhead" -> Some (fun c -> XOverhead (c, n)) | "boot" -> Some (fun c -> XBoot (c, n))
    | _ -> None in
  let bad () = fail sexp (sprintf "Not a valid expectation: %s" (to_string sexp)) in
  (* A probe runs N instructions in cdb (`skip N-1`): N = 0 has nothing to run. *)
  let count n = let k = parse_int n in
    if k < 1 then fail sexp (sprintf "Not a valid expectation: %s (N must be at least 1)" (to_string sexp))
    else k in
  match sexp with
  | `List [`Atom m; `Atom "<="; n] ->
    (match measured m (parse_int n) with Some f -> f Le | None -> bad ())
  | `List [`Atom "step"; k] -> XStep (parse_int k)
  | `List [`Atom "alive"; n] -> XAlive (count n)
  | `List [`Atom "dead"; n] -> XDead (count n)
  | `List [`Atom m; n] -> (match measured m (parse_int n) with Some f -> f Eq | None -> bad ())
  | `List [`Atom "covers-core"] -> XCoversCore
  | `List [`Atom "cell"; addr; `Atom text; n] -> XCell (parse_int addr, text, count n)
  | _ -> bad ()

let rec parse_exp (sexp : sexp) : expr =
  let loc = here sexp in
  match sexp with
  | `List (`Atom "com" :: exps) -> EComment (List.fold_left (fun res s -> res ^ " " ^ (String.escaped (to_string s))) "" exps)
  | `List (`Atom "seq" :: exps) -> ESeq (List.map parse_exp exps, loc)
  | `List [`Atom "label"; `Atom s] -> ELabel (label_name sexp s, loc)
  | `List [eop] ->
    (match eop with
    | `Atom "DAT" -> EPrim2 (Dat, MN, ANone, ANone, loc)
    | `Atom "NOP" -> EPrim2 (Nop, MN, ANone, ANone, loc)
    | _ -> fail sexp (sprintf "Not a valid expr: %s" (to_string sexp)) )
  | `List [eop; e] ->
    (match eop with
    | `Atom "DAT" -> EPrim2 (Dat, MN, ANone, parse_arg e, loc)
    | `Atom "JMP" -> EPrim2 (Jmp, MN, parse_arg e, ANone, loc)
    | `Atom "SPL" -> EPrim2 (Spl, MN, parse_arg e, ANone, loc)
    | `Atom "NOP" -> EPrim2 (Nop, MN, parse_arg e, ANone, loc)
    | `Atom "repeat" -> EFlow1 (Repeat, Cond0, parse_exp e, loc)
    | `Atom "expect" -> EExpect (parse_expectation e, loc)
    | _ -> fail sexp (sprintf "Not a valid unary expr: %s" (to_string sexp)) )
  | `List [eop; e1; e2] ->
    (match eop with 
    | `Atom "DAT" -> EPrim2 (Dat, MN, parse_arg e1, parse_arg e2, loc)
    | `Atom "JMP" -> EPrim2 (Jmp, MN, parse_arg e1, parse_arg e2, loc)
    | `Atom "SPL" -> EPrim2 (Spl, MN, parse_arg e1, parse_arg e2, loc)
    | `Atom "NOP" -> EPrim2 (Nop, MN, parse_arg e1, parse_arg e2, loc)
    | `Atom "MOV" -> EPrim2 (Mov, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "ADD" -> EPrim2 (Add, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "SUB" -> EPrim2 (Sub, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "MUL" -> EPrim2 (Mul, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "DIV" -> EPrim2 (Div, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "MOD" -> EPrim2 (Mod, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "JMZ" -> EPrim2 (Jmz, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "JMN" -> EPrim2 (Jmn, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "DJN" -> EPrim2 (Djn, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "SEQ" -> EPrim2 (Seq, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "SNE" -> EPrim2 (Sne, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "SLT" -> EPrim2 (Slt, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "STP" -> EPrim2 (Stp, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "LDP" -> EPrim2 (Ldp, MDef, parse_arg e1, parse_arg e2, loc)
    | `Atom "if" -> EFlow1 (If, parse_cond e1, parse_exp e2, loc)
    | `Atom "while" -> EFlow1 (While, parse_cond e1, parse_exp e2, loc)
    | `Atom "do-while" -> EFlow1 (DoWhile, parse_cond e1, parse_exp e2, loc)
    | `Atom "let" ->
      (match e1 with
      | `List [`Atom id; e] -> ELet (user_name e1 id, parse_arg e, parse_exp e2, loc)
      | _ -> fail e1 (sprintf "Not a valid let assignment: %s" (to_string e1)) )
    | _ -> fail sexp (sprintf "Not a valid binary expr: %s" (to_string sexp)) )
  | `List [eop; e1; e2; e3] ->
    (match eop with
    | `Atom "MOV" -> EPrim2 (Mov, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "ADD" -> EPrim2 (Add, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "SUB" -> EPrim2 (Sub, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "MUL" -> EPrim2 (Mul, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "DIV" -> EPrim2 (Div, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "MOD" -> EPrim2 (Mod, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "JMZ" -> EPrim2 (Jmz, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "JMN" -> EPrim2 (Jmn, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "DJN" -> EPrim2 (Djn, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "SEQ" -> EPrim2 (Seq, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "SNE" -> EPrim2 (Sne, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "SLT" -> EPrim2 (Slt, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "STP" -> EPrim2 (Stp, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "LDP" -> EPrim2 (Ldp, parse_imod e1, parse_arg e2, parse_arg e3, loc)
    | `Atom "if" -> EFlow2 (IfElse, parse_cond e1, parse_exp e2, parse_exp e3, loc)
    | _ -> fail sexp (sprintf "Not a valid ternary expr: %s" (to_string sexp)) )
  | _ -> fail sexp (sprintf "Not a valid expr: %s" (to_string sexp))


(* A source: a plain expression, or (program (optimize o ...) (expect e) ... body) *)
let parse_source (sexp : sexp) : source =
  match sexp with
  | `List (`Atom "program" :: items) ->
    let optimize = ref None and expects = ref [] and bodies = ref [] in
    List.iter (fun item -> match item with
      | `List [`Atom "optimize"] -> fail item "an (optimize ...) needs at least one objective"
      | `List (`Atom "optimize" :: os) ->
        optimize := Some (List.map (fun o -> match o with
          | `Atom s -> s
          | `List _ -> fail o (sprintf "Not an objective: %s" (to_string o))) os)
      | `List [`Atom "expect"; e] -> expects := parse_expectation e :: !expects
      | `Atom _ | `List _ -> bodies := parse_exp item :: !bodies) items ;
    (match !bodies with
    | [body] -> { optimize = !optimize; expects = List.rev !expects; body }
    | [] | _ :: _ :: _ -> fail sexp "a (program ...) needs exactly one body expression")
  | `Atom _ | `List _ -> { optimize = None; expects = []; body = parse_exp sexp }

(* parse a program from a file *)
let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
   match Located.parse_file filename with
   | Ok s -> s
   | Error msg -> error (sprintf "Unable to parse file %s: %s" filename msg)
 
(* parse a program from a string *)
let sexp_from_string (src : string) : CCSexp.sexp =
  match Located.parse_string src with
  | Ok s -> s
  | Error msg -> error msg
