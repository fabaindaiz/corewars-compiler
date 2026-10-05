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

(* The atoms the macro expander invents (a template's labels and let binders renamed per
   expansion), by physical identity: only these may carry a name starting with "_". *)
let generated : unit Phys.t = Phys.create 64

(* Names starting with "_" belong to the labels the compiler generates (_LET1, _WHI9, ...), so a
   user name can never collide with one. [atom] is the name's own node, when [sexp] is the form
   around it. *)
let user_name ?atom (sexp : sexp) (s : string) : string =
  if String.length s > 0 && s.[0] = '_' && not (Phys.mem generated (Option.value atom ~default:sexp)) then
    fail sexp (sprintf "`%s`: names starting with `_` are reserved for the compiler" s)
  else s

(* pMARS reads these as opcodes or pseudo-opcodes in any case (asm.c), never as labels. *)
let pmars_keywords = ["MOV"; "ADD"; "SUB"; "MUL"; "DIV"; "MOD"; "JMZ"; "JMN"; "DJN"; "CMP"; "SLT"; "SPL";
                      "DAT"; "JMP"; "SEQ"; "SNE"; "NOP"; "LDP"; "STP"; "ORG"; "END"; "PIN"; "EQU"; "FOR"; "ROF"]

let label_name ?atom (sexp : sexp) (s : string) : string =
  let s = user_name ?atom sexp s in
  (* a name the expander made is a user's label renamed: already checked, its "_" reserved *)
  if Option.fold atom ~none:false ~some:(Phys.mem generated) then s
  else if List.mem s pmars_predefined then
    fail sexp (sprintf "`%s` is a pMARS predefined symbol and cannot be a label" s)
  else if not (valid_label s) then fail sexp (invalid_label s)
  else if List.mem (String.uppercase_ascii s) pmars_keywords then
    fail sexp (sprintf "`%s` is a pMARS keyword and cannot be a label" s)
  else s


let parse_mode (sexp : sexp) : mode =
  match sexp with
  | `Atom "Imm" | `Atom "#" -> MImm
  | `Atom "Dir" | `Atom "$" -> MDir
  | `Atom "Ind" | `Atom "@" -> MInd (MINone)
  | `Atom "Dec" | `Atom "<" -> MInd (MIDec)
  | `Atom "Inc" | `Atom ">" -> MInd (MIInc)
  | `Atom "AInd" | `Atom "*" -> MIndA (MINone)
  | `Atom "ADec" | `Atom "{" -> MIndA (MIDec)
  | `Atom "AInc" | `Atom "}" -> MIndA (MIInc)
  | _ -> fail sexp (sprintf "Not a valid mode: %s" (to_string sexp))

(* An expression for pMARS: numbers, names (labels, constants) and + - * / %, two operands each. *)
let rec parse_rexpr (sexp : sexp) : Red.rexpr =
  match sexp with
  | `Atom s ->
    (match Int64.of_string_opt s with
    | Some n -> Red.XNum (Int64.to_int n)
    | None ->
      let s = user_name sexp s in
      (* a name pMARS reads: a label, a constant, or one of its predefined symbols *)
      if valid_label s || Phys.mem generated sexp || List.mem s pmars_predefined then Red.XName s
      else fail sexp (invalid_label s))
  | `List [`Atom ("+" | "-" | "*" | "/" | "%" as op); a; b] -> Red.XBin (op.[0], parse_rexpr a, parse_rexpr b)
  | `List _ -> fail sexp (sprintf "Not a valid expression: %s" (to_string sexp))

let is_operator (sexp : sexp) : bool =
  match sexp with
  | `Atom ("+" | "-" | "*" | "/" | "%") -> true
  | `Atom _ | `List _ -> false

let parse_arg (sexp : sexp) : arg =
  match sexp with
  | `Atom "none" -> ANone
  | `List [op; _; _] when is_operator op -> AExp (None, parse_rexpr sexp)
  | `List [m; (`List _ as e)] -> AExp (Some (parse_mode m), parse_rexpr e)
  | `List [`Atom ("store" | "!"); (`Atom s as a)] -> AStore (user_name ~atom:a sexp s)
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
  (* The wrong number of arguments for a known operator is named after the operator's kind. *)
  let kind cop = if cond1_op cop <> None then "unary" else "binary" in
  match sexp with
  | `List [cop; a] ->
    (match cond1_op cop with Some op -> Cond1 (op, MDef, parse_arg a) | None -> bad (kind cop))
  | `List [cop; x; y] ->
    (match cond1_op cop, cond2_op cop with
    | Some op, _ -> Cond1 (op, parse_imod x, parse_arg y)
    | None, Some op -> Cond2 (op, MDef, parse_arg x, parse_arg y)
    | None, None -> bad "binary")
  | `List [cop; m; a1; a2] ->
    (match cond2_op cop with Some op -> Cond2 (op, parse_imod m, parse_arg a1, parse_arg a2) | None -> bad (kind cop))
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
  | `List [`Atom "label"; (`Atom s as a)] -> ELabel (label_name ~atom:a sexp s, loc)
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
    | `Atom "repeat" -> EFlow1 (Repeat ANone, Cond0, parse_exp e, loc)
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
    | `Atom "repeat" -> EFlow1 (Repeat (parse_arg e2), Cond0, parse_exp e1, loc)
    | `Atom "if" -> EFlow1 (If, parse_cond e1, parse_exp e2, loc)
    | `Atom "while" -> EFlow1 (While, parse_cond e1, parse_exp e2, loc)
    | `Atom "do-while" -> EFlow1 (DoWhile, parse_cond e1, parse_exp e2, loc)
    | `Atom "let" ->
      (match e1 with
      | `List [(`Atom id as a); e] -> ELet (user_name ~atom:a e1 id, parse_arg e, parse_exp e2, loc)
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


(* The macro layer (docs/specs/2026-10-04-macros-design.md): typed templates, expanded at the
   s-expression level before anything else, and (for k lo hi body) over constants. *)
type kind = KNum | KLab | KVar | KCode

type template = { name : string; params : (string * kind) list; body : sexp; index : int; at : sexp }

let string_of_kind (k : kind) : string =
  match k with KNum -> "Num" | KLab -> "Lab" | KVar -> "Var" | KCode -> "Code"

(* Words RED gives a meaning to: a template, a parameter or a for variable named like one would
   change what the expanded program says. *)
let red_words = ["seq"; "let"; "label"; "com"; "store"; "none"; "repeat"; "if"; "while"; "do-while";
                 "expect"; "for"; "define"; "program"; "A"; "B"; "AB"; "BA"; "F"; "X"; "I";
                 "Imm"; "Dir"; "Ind"; "Dec"; "Inc"; "AInd"; "ADec"; "AInc"; "JZ"; "JN"; "DZ"; "DN";
                 "EQ"; "NE"; "GT"; "LT"; "length"; "cycles"; "overhead"; "boot"; "step"; "covers-core";
                 "alive"; "dead"; "cell"]

(* A template called at the top of the program's body would be read as the header item of its name *)
let header_words = ["start"; "hill"; "name"; "author"; "strategy"; "optimize"; "const"]

let macro_name (sexp : sexp) (s : string) : string =
  let s = label_name sexp s in
  if List.mem s red_words then fail sexp (sprintf "`%s` is a RED word and cannot name a template or parameter" s) ;
  s

let parse_template (index : int) (item : sexp) : template =
  match item with
  | `List [`Atom "define"; `List (`Atom name :: params); body] ->
    let param p = match p with
      | `List [`Atom x; `Atom k] ->
        let kind = match k with
          | "Num" -> KNum | "Lab" -> KLab | "Var" -> KVar | "Code" -> KCode
          | _ -> fail p (sprintf "`%s` is not a kind: one of Num, Lab, Var, Code" k) in
        (macro_name p x, kind)
      | `Atom _ | `List _ -> fail p (sprintf "Not a parameter: %s (write (name Kind))" (to_string p)) in
    let params = List.map param params in
    let names = List.map fst params in
    if List.length (List.sort_uniq Stdlib.compare names) <> List.length names then
      fail item (sprintf "`%s` names a parameter twice" name) ;
    let name = macro_name item name in
    if List.mem name header_words then fail item (sprintf "`%s` is a RED word and cannot name a template" name) ;
    { name; params; body; index; at = item }
  | `Atom _ | `List _ -> fail item (sprintf "Not a template: %s (write (define (name (param Kind) ...) body))" (to_string item))

(* Every atom in [s] that [env] names is replaced; a new list carries [loc], the place of the call
   or for that made it, so an error inside an expansion says where that is. *)
let rec subst (env : (string * sexp) list) (loc : loc option) (s : sexp) : sexp =
  match s with
  | `Atom a -> (match List.assoc_opt a env with Some r -> r | None -> s)
  | `List l ->
    let t = `List (List.map (subst env loc) l) in
    Option.iter (Phys.replace locations t) loc ;
    t

(* The labels and let binders a template body defines: renamed at each expansion. *)
let rec defined (s : sexp) : string list =
  match s with
  | `List [`Atom "label"; `Atom l] -> [l]
  | `List [`Atom "let"; `List [`Atom x; init]; body] -> x :: defined init @ defined body
  | `List l -> List.concat_map defined l
  | `Atom _ -> []

(* The variables of the (for k ...) in [s]: renamed at each expansion too, so a for replaces only
   its own variable, never a name an argument brought in. *)
let rec for_vars (s : sexp) : string list =
  match s with
  | `List [`Atom "for"; `Atom k; _; _; body] -> k :: for_vars body
  | `List l -> List.concat_map for_vars l
  | `Atom _ -> []

let rec labels_in (s : sexp) : string list =
  match s with
  | `List [`Atom "label"; `Atom l] -> [l]
  | `List l -> List.concat_map labels_in l
  | `Atom _ -> []

(* A template's let and for names renamed within their own scope: the binder and its body, never the
   let's initial value or a use outside, which may name a label of the program. *)
let rec scoped (env : (string * sexp) list) (active : string list) (loc : loc option) (s : sexp) : sexp =
  let node items = let t = `List items in Option.iter (Phys.replace locations t) loc ; t in
  let inner x = if List.mem_assoc x env then x :: active else List.filter (fun a -> a <> x) active in
  let binder x xa = if List.mem_assoc x env then List.assoc x env else xa in
  match s with
  | `Atom a -> if List.mem a active then List.assoc a env else s
  | `List [(`Atom "let" as l); `List [(`Atom x as xa); init]; body] ->
    node [l; node [binder x xa; scoped env active loc init]; scoped env (inner x) loc body]
  | `List [(`Atom "for" as f); (`Atom k as ka); lo; hi; body] ->
    node [f; binder k ka; scoped env active loc lo; scoped env active loc hi; scoped env (inner k) loc body]
  | `List l -> node (List.map (scoped env active loc) l)

(* The templates a body calls: every list's head, except a let's binding, whose head is its name *)
let rec heads (s : sexp) : string list =
  match s with
  | `List [`Atom "let"; `List [_; init]; body] -> "let" :: heads init @ heads body
  | `List (`Atom h :: rest) -> h :: List.concat_map heads rest
  | `List l -> List.concat_map heads l
  | `Atom _ -> []

(* [consts] evaluates for bounds; [templates] in definition order. A call may reach only templates
   defined before its own, so every expansion ends. *)
let expand (consts : (string * Red.rexpr) list) (templates : template list) (program : sexp) : sexp =
  List.iter (fun t ->
    match List.find_opt (fun h -> List.exists (fun u -> u.name = h && u.index >= t.index) templates) (heads t.body) with
    | Some c -> fail t.at (sprintf "`%s` calls `%s`, which is not defined before it" t.name c)
    | None -> ()) templates ;
  let count = ref 0 and steps = ref 0 in
  (* no hill holds more than 200 instructions: an expansion far past that is a mistake, stopped
     before it costs seconds *)
  let step (s : sexp) =
    incr steps ;
    if !steps > 10000 then fail s "the program expands to more than 10000 template calls and for iterations" in
  let bound (b : sexp) : int =
    match (try Consts.value consts (parse_rexpr b) with Error _ -> None) with
    | Some n -> n
    | None -> fail b (sprintf "a (for ...) bound must be a number known when compiling: %s" (to_string b)) in
  let rec go (scope : string list) (s : sexp) : sexp =
    match s with
    | `Atom _ -> s
    | `List [(`Atom "let" as l); (`List [(`Atom x as xa); init] as binding); body] ->
      (* the binder's own node is kept: a renamed one is known by its identity (generated) *)
      let b = `List [xa; go scope init] in
      Option.iter (Phys.replace locations b) (loc_of binding) ;
      let t = `List [l; b; go (x :: scope) body] in
      Option.iter (Phys.replace locations t) (loc_of s) ;
      t
    | `List [`Atom "for"; (`Atom k as ka); lo; hi; body] ->
      let k = if Phys.mem generated ka then k else macro_name s k in
      (* the body's k are all replaced by numbers: a binder of that name inside would lose its uses *)
      if List.mem k (defined body @ for_vars body) then
        fail s (let o = Rename.original k in
                sprintf "`%s` is the variable of a (for %s ...): a let, label or for inside it cannot take its name" o o) ;
      let lo = bound lo and hi = bound hi in
      (* hi - lo overflows to a negative number on bounds near the integer limits *)
      if hi >= lo && (hi - lo >= 1000 || hi - lo < 0) then fail s "a (for ...) repeats at most 1000 times" ;
      let iterations = if hi < lo then [] else List.init (hi - lo + 1) (fun i -> lo + i) in
      let t = `List (`Atom "seq" :: List.map (fun i -> step s ; go scope (subst [(k, `Atom (string_of_int i))] (loc_of s) body)) iterations) in
      Option.iter (Phys.replace locations t) (loc_of s) ;
      t
    | `List (`Atom h :: args) when List.exists (fun t -> t.name = h) templates ->
      let t = List.find (fun t -> t.name = h) templates in
      let n = List.length t.params in
      if List.length args <> n then
        fail s (sprintf "`%s` takes %d argument%s, given %d" h n (if n = 1 then "" else "s") (List.length args)) ;
      List.iter2 (fun (p, kind) a ->
        let bad why = fail a (sprintf "`%s`'s %s is a %s: %s" h p (string_of_kind kind) why) in
        match kind, a with
        | KNum, `Atom x when List.mem x scope -> bad (sprintf "`%s` is a let variable (pass it as a Var)" x)
        | KNum, `Atom _ -> ()
        | KNum, `List [op; _; _] when is_operator op -> ()
        | KNum, `List _ -> bad (sprintf "a number, constant, label or expression, not %s" (to_string a))
        | KLab, `Atom x when (valid_label x || Phys.mem generated a) && not (List.mem x scope) -> ()
        | KLab, `Atom x -> bad (sprintf "a label, not %s" (Rename.original x))
        | KLab, `List _ -> bad (sprintf "a label, not %s" (to_string a))
        | KVar, `Atom x when List.mem x scope -> ()
        | KVar, (`Atom _ | `List _) -> bad (sprintf "`%s` is not a let variable here" (to_string a))
        | KCode, `List _ -> ()
        | KCode, `Atom _ -> bad (sprintf "a RED expression, not %s" (to_string a))) t.params args ;
      incr count ;
      step s ;
      (* labels and let binders of the body renamed first, then the arguments put in: a name passed
         in is never renamed and never captured *)
      let own names = List.filter (fun d -> not (List.mem_assoc d t.params)) (List.sort_uniq Stdlib.compare names) in
      let renames = List.map (fun d ->
          let a = `Atom (sprintf "_X%d_%s" !count d) in
          Phys.replace generated a () ;
          Option.iter (Phys.replace locations a) (loc_of s) ;
          (d, a)) in
      let binders = renames (own (List.filter (fun d -> not (List.mem d (labels_in t.body))) (defined t.body) @ for_vars t.body)) in
      let body = scoped binders [] (loc_of s) t.body in
      go scope (subst (renames (own (labels_in t.body)) @ List.combine (List.map fst t.params) args) (loc_of s) body)
    | `List l ->
      let t = `List (List.map (go scope) l) in
      Option.iter (Phys.replace locations t) (loc_of s) ;
      t in
  go [] program

(* A source: a plain expression, or (program (optimize o ...) (expect e) ... body) *)
let parse_source (sexp : sexp) : source =
  match sexp with
  | `List (`Atom "program" :: items) ->
    let optimize = ref None and expects = ref [] and consts = ref [] and bodies = ref [] in
    let hill = ref None and meta = ref [] and start = ref None and templates = ref [] in
    let words item ws = String.concat " " (List.map (fun w -> match w with
      | `Atom s -> s
      | `List _ -> fail item (sprintf "Not a word: %s" (to_string w))) ws) in
    List.iter (fun item -> match item with
      | `List [`Atom "optimize"] -> fail item "an (optimize ...) needs at least one objective"
      | `List (`Atom "optimize" :: os) ->
        optimize := Some (List.map (fun o -> match o with
          | `Atom s -> s
          | `List _ -> fail o (sprintf "Not an objective: %s" (to_string o))) os)
      | `List [`Atom "expect"; e] -> expects := parse_expectation e :: !expects
      | `List [`Atom "start"; `Atom l] ->
        if !start <> None then fail item "(start ...) is given twice" ;
        start := Some (label_name item l)
      | `List [`Atom "hill"; `Atom h] ->
        if !hill <> None then fail item "(hill ...) is given twice" ;
        hill := Some h
      | `List (`Atom ("name" | "author" | "strategy" as k) :: ws) ->
        let a = if k = "author" then "an" else "a" in
        let text = words item ws in
        (* each item is one comment line in the output: an empty one says nothing, and a line break
           would put its words outside the comment, as redcode *)
        if String.trim text = "" then fail item (sprintf "%s (%s ...) needs words" a k) ;
        if String.exists (fun c -> Char.code c < 32) text then
          fail item (sprintf "%s (%s ...) is one line: no line breaks or control characters" a k) ;
        if k <> "strategy" && List.mem_assoc k !meta then fail item (sprintf "(%s ...) is given twice" k) ;
        meta := (k, text) :: !meta
      | `List [`Atom "const"; `Atom n; v] ->
        let n = label_name item n in
        if List.mem_assoc n !consts then fail item (sprintf "constant `%s` is defined twice" n) ;
        let v = parse_rexpr v in
        if Consts.divides_by_zero (List.rev !consts) v then
          fail item "a division by zero in an expression: pMARS rejects the warrior" ;
        (* EQU substitutes text: a value naming a label would mean a different cell at every use. *)
        let rec names (x : Red.rexpr) = match x with
          | Red.XNum _ -> [] | Red.XName s -> [s] | Red.XBin (_, a, b) -> names a @ names b in
        (match List.find_opt (fun s -> not (List.mem_assoc s !consts)) (names v) with
        | Some s -> fail item (sprintf "a constant is a number: `%s` is not a constant defined before `%s`" s n)
        | None -> consts := (n, v) :: !consts)
      | `List (`Atom "define" :: _) ->
        let t = parse_template (List.length !templates) item in
        if List.exists (fun u -> u.name = t.name) !templates then fail item (sprintf "template `%s` is defined twice" t.name) ;
        templates := t :: !templates
      | `Atom _ | `List _ -> bodies := item :: !bodies) items ;
    (* the body is expanded once the whole header is known: its constants bound the for loops *)
    (match !bodies with
    | [body] ->
      let body = parse_exp (expand (List.rev !consts) (List.rev !templates) body) in
      { optimize = !optimize; expects = List.rev !expects; consts = List.rev !consts;
        hill = !hill; meta = List.rev !meta; start = !start; body }
    | [] | _ :: _ :: _ -> fail sexp "a (program ...) needs exactly one body expression")
  | `Atom _ | `List _ ->
    { optimize = None; expects = []; consts = []; hill = None; meta = []; start = None; body = parse_exp (expand [] [] sexp) }

(* parse a program from a file *)
let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
   Phys.reset locations ;
   Phys.reset generated ;
   match Located.parse_file filename with
   | Ok s -> s
   | Error msg -> error (sprintf "Unable to parse file %s: %s" filename msg)
 
(* parse a program from a string *)
(* CCSexp reports "parse error at L:C: ..." with a 0-based column: said as file:line:col like every
   other error, 1-based. *)
let sexp_from_string (src : string) : CCSexp.sexp =
  Phys.reset locations ;
  Phys.reset generated ;
  match Located.parse_string src with
  | Ok s -> s
  | Error msg ->
    (match Scanf.sscanf_opt msg "parse error at %d:%d: %s@\n" (fun l c rest -> (l, c, rest)) with
    | Some (line, col, rest) -> raise (Error (Some { line; col = col + 1 }, rest))
    | None -> error msg)
