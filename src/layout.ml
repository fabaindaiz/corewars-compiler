(** Layout: the emitted program as positioned cells, with successors and loops **)
open Red
open Ast

(* A view for analysis only: redcode is still printed from the instruction list, with labels.
   Offsets are resolved the way pMARS resolves them, relative to the cell that holds them. *)

type field = FA | FB

type role = Code | Epilogue

type operand = { mode : rmode; value : int; label : string option }

type cell = {
  pos : int;
  op : opcode;
  md : rmod;
  a : operand;
  b : operand;
  labels : string list;
  vars : (string * field) list;
  origin : tag option;
  construct : string option;
  role : role;
}

type diagnostic =
| Undefined_label of string
| Duplicate_label of string * int * int
| Long_line of int

type edge =
| Next of int
| Jump of int
| Skip of int
| Dynamic

(* owner: the construct whose loop-head label the loop closes on (its tag and name), when there is
   one; a loop of user code has none. *)
type loop = { header : int; body : int list; back_edges : (int * int) list; owner : (tag * string) option }

(* The labels a loop construct puts on its head: repeat, while (unrotated: its test; rotated: the
   test after the body), do-while. *)
let loop_heads = ["_REP"; "_WHI"; "_WHC"; "_DWH"]

type program = {
  cells : cell array;
  succ : edge list array;
  loops : loop list;
  diagnostics : diagnostic list;
  coresize : int;
  entry : int;  (* the cell execution starts at: the (start label), else the first *)
}

let default_coresize = 8000

let max_line = Compile.max_line

let norm (coresize : int) (n : int) : int = ((n mod coresize) + coresize) mod coresize

let indirect (m : rmode) : bool =
  match m with
  | RAInd | RBInd | RADec | RBDec | RAInc | RBInc -> true
  | RImm | RDir -> false

let field_of_place (p : place) : field = match p with PA -> FA | PB -> FB


(* Positions: an ILAB attaches to the next instruction, the trailing ones to the epilogue. *)
type raw = { r_instr : opcode * rmod * rarg * rarg; r_labels : string list; r_vars : (string * field) list;
             r_origin : tag option; r_construct : string option; r_role : role; r_long : bool }

let place_cells (body : Compile.emitted list) : raw list =
  let long s = String.length s >= max_line in
  let step (acc, pending, pending_long) ((x : Compile.emitted), role) =
    match x.instr with
    | ICOM _ -> (acc, pending, pending_long)
    | ILAB l -> (acc, l :: pending, pending_long || long (pp_instruction x.instr))
    | INSTR (op, md, a, b) ->
      let r = { r_instr = (op, md, a, b); r_labels = List.rev pending;
                r_vars = List.map (fun (v, p) -> (v, field_of_place p)) x.stores;
                r_origin = x.origin; r_construct = x.construct; r_role = role;
                r_long = pending_long || long (pp_instruction x.instr) } in
      (r :: acc, [], false) in
  let epilogue = List.map (fun i -> (Compile.emit i, Epilogue)) Compile.epilogue in
  let all = List.map (fun x -> (x, Code)) body @ epilogue in
  let acc, _, _ = List.fold_left step ([], [], false) all in
  List.rev acc


let build ?(coresize = default_coresize) ?(consts = []) ?start (body : Compile.emitted list) : program =
  let raws = Array.of_list (place_cells body) in
  let n = Array.length raws in
  let table = Hashtbl.create 16 in
  let diags = ref [] in
  let add d = if not (List.mem d !diags) then diags := d :: !diags in
  Array.iteri (fun pos r ->
    List.iter (fun l -> match Hashtbl.find_opt table l with
      | Some first -> add (Duplicate_label (l, first, pos))
      | None -> Hashtbl.add table l pos) r.r_labels ;
    if r.r_long then add (Long_line pos)) raws ;
  (* An expression is evaluated as pMARS does: a label is its offset from the cell that holds it, a
     constant its value; a division by zero, which pMARS rejects, is 0 here. *)
  let rec eval pos (x : rexpr) : int =
    match x with
    | XNum n -> n
    | XName s ->
      (match List.assoc_opt s consts, Hashtbl.find_opt table s with
      | Some v, _ -> eval pos v
      | None, Some t -> t - pos
      | None, None ->
        (* pMARS's predefined symbols: CORESIZE is known here; the others depend on the hill's
           settings and count as 0, unreported. *)
        if s = "CORESIZE" then coresize
        else if List.mem s pmars_predefined then 0
        else (add (Undefined_label s) ; 0))
    | XBin (op, a, b) ->
      let a = eval pos a and b = eval pos b in
      (match op with
      | '+' -> a + b
      | '-' -> a - b
      | '*' -> a * b
      | '/' -> if b = 0 then 0 else a / b
      | '%' -> if b = 0 then 0 else a mod b
      | _ -> failwith (Printf.sprintf "Layout.eval: no operator %c" op)) in
  let operand pos (a : rarg) : operand =
    match a with
    | RExp (m, x) -> { mode = m; value = norm coresize (eval pos x); label = None }
    | RNone -> { mode = RImm; value = 0; label = None }
    | RRef (m, k) -> { mode = m; value = norm coresize k; label = None }
    | RLab (m, l) ->
      (match Hashtbl.find_opt table l with
      | Some t -> { mode = m; value = norm coresize (t - pos); label = Some l }
      | None -> add (Undefined_label l) ; { mode = m; value = 0; label = Some l }) in
  let cells = Array.mapi (fun pos r ->
    let op, md, a, b = r.r_instr in
    { pos; op; md; a = operand pos a; b = operand pos b; labels = r.r_labels; vars = r.r_vars;
      origin = r.r_origin; construct = r.r_construct; role = r.r_role }) raws in
  let succ = Array.map (fun c ->
    let next = if c.pos + 1 < n then [Next (c.pos + 1)] else [] in
    let skip = if c.pos + 2 < n then [Skip (c.pos + 2)] else [] in
    (* A jump whose target is indirect, unresolved or outside the warrior goes where the static
       view cannot follow. An immediate target is the instruction itself (ICWS'94). *)
    let target () =
      let resolved = match c.a.label with Some l -> Hashtbl.mem table l | None -> true in
      if indirect c.a.mode || not resolved then Dynamic
      else
        let t = if c.a.mode = RImm then c.pos else norm coresize (c.pos + c.a.value) in
        if t < n then Jump t else Dynamic in
    match c.op with
    | IDAT -> []
    | IJMP -> [target ()]
    | ISPL -> next @ [target ()]
    | IJMZ | IJMN | IDJN -> target () :: next
    | ISEQ | ISNE | ISLT | ICMP when c.a.mode = RImm && c.b.mode = RImm && (c.md = RAB || c.md = RN) ->
      (* Both operands immediate: the instruction compares its own A-number with its own B-number,
         a constant, so only one successor is possible (SNE #0, #1 always skips). *)
      let skips = match c.op with
        | ISEQ | ICMP -> c.a.value = c.b.value
        | ISNE -> c.a.value <> c.b.value
        | ISLT -> c.a.value < c.b.value
        | IDAT | ISPL | IJMP | INOP | IMOV | IADD | ISUB | IMUL | IDIV | IMOD | IJMZ | IJMN | IDJN
        | ILDP | ISTP -> false in
      if skips then skip else next
    | ISEQ | ISNE | ISLT | ICMP -> next @ skip
    | IMOV | IADD | ISUB | IMUL | IDIV | IMOD | INOP | ILDP | ISTP -> next) cells in
  let index e = match e with Next i | Jump i | Skip i -> Some i | Dynamic -> None in
  (* Back edges by depth-first search from the entry; each loop's body is its header plus every
     cell that reaches a back edge's source without passing the header. *)
  let visited = Array.make n false and on_stack = Array.make n false in
  let backs = ref [] in
  let rec dfs i =
    visited.(i) <- true ; on_stack.(i) <- true ;
    List.iter (fun e -> match index e with
      | Some t when on_stack.(t) -> backs := (i, t) :: !backs
      | Some t when not visited.(t) -> dfs t
      | Some _ | None -> ()) succ.(i) ;
    on_stack.(i) <- false in
  let entry = match start with
    | Some l -> (match Hashtbl.find_opt table l with Some t -> t | None -> add (Undefined_label l) ; 0)
    | None -> 0 in
  if n > 0 then dfs entry ;
  let preds = Array.make n [] in
  Array.iteri (fun i es -> List.iter (fun e -> match index e with
    | Some t -> preds.(t) <- i :: preds.(t) | None -> ()) es) succ ;
  let natural (s, h) =
    let inside = Array.make n false in
    inside.(h) <- true ;
    let rec up i = if not inside.(i) then (inside.(i) <- true ; List.iter up preds.(i)) in
    up s ;
    List.filter (fun i -> inside.(i)) (List.init n Fun.id) in
  (* Which loop a back edge closes. A jump to a loop-head label closes that construct's loop: loops
     that share a header (a do-while whose body starts with a repeat: _DWH and _REP on one cell)
     stay separate, and a threaded jump (Compile.thread_jumps) closes the loop whose head it now
     names. Any other back edge into a cell holding a loop head closes that loop: a rotated while's
     body falls into its test, or jumps there through an if-else's end label. Elsewhere (user
     loops) the label jumped to, or for a jump through a number the construct that emitted it. *)
  let heads = Hashtbl.create 8 in
  List.iter (fun (x : Compile.emitted) -> match x.instr, x.origin, x.construct with
    | ILAB l, Some t, Some k when List.exists (fun p -> String.starts_with ~prefix:p l) loop_heads ->
      Hashtbl.replace heads l (t, k)
    | (ILAB _ | ICOM _ | INSTR _), _, _ -> ()) body ;
  let closing (s, h) =
    let jumped = match cells.(s).a.label with
      | Some l when List.mem (Jump h) succ.(s) -> Some l
      | Some _ | None -> None in
    match Option.bind jumped (Hashtbl.find_opt heads) with
    | Some o -> `Owner o
    | None ->
      (match List.find_map (Hashtbl.find_opt heads) cells.(h).labels, jumped with
      | Some o, _ -> `Owner o
      | None, Some l -> `Label l
      | None, None -> `Origin cells.(s).origin) in
  let keys = List.sort_uniq compare (List.map (fun (s, h) -> (h, closing (s, h))) !backs) in
  let loops = List.map (fun (h, k) ->
    let edges = List.sort compare (List.filter (fun (s, t) -> t = h && closing (s, t) = k) !backs) in
    let body = List.sort_uniq compare (List.concat_map natural edges) in
    let owner = match k with `Owner o -> Some o | `Label _ | `Origin _ -> None in
    { header = h; body; back_edges = edges; owner }) keys in
  { cells; succ; loops; diagnostics = List.rev !diags; coresize; entry }

let reachable (p : program) : bool array =
  let n = Array.length p.cells in
  let seen = Array.make n false in
  let rec go i = if i < n && not seen.(i) then begin
    seen.(i) <- true ;
    List.iter (fun e -> match e with Next t | Jump t | Skip t -> go t | Dynamic -> ()) p.succ.(i) end in
  go p.entry ; seen

let of_expr ?coresize (e : expr) : program = build ?coresize (Compile.compile_body e)
