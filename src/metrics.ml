(** Metrics: what the emitted program costs, measured on its layout **)
open Printf
open Red
open Ast
open Layout

(* A cycle is one instruction executed by one process: with n processes each advances once every
   n cycles. Every figure here is per process and static: self-modifying code is not followed. *)

type range = { min : int; max : int }

type loop_metrics = {
  loop : Layout.loop;
  label : string option;  (* the header cell's first label *)
  node : tag option;
  construct : string option;
  cycles : range;
  overhead : range;
  own : range;            (* control instructions of the loop's own construct, per iteration *)
  test : opcode option;   (* the loop construct's own test, if it has one *)
  exit : int option;
}

type prediction =
| Step of { loop : int; node : tag option; cell : int; field : Layout.field; k : int; period : int;
            cover_cycles : int; full : bool; own_hit : (int * int * int) option }
| Counter of { loop : int; node : tag option; n : int; loop_cycles : int; dies_after : int option }

type t = {
  length : int;
  code : int;
  data : int;
  epilogue : int;
  unreachable : int;
  dead : (int * tag option) list;  (* the unreachable cells, with the node that emitted each *)
  nonzero : int;
  nonblank : int;
  boot : range option;
  spl_sites : int;
  dynamic_jumps : int;
  div_by_zero : int list;
  loops : loop_metrics list;
  predictions : prediction list;
  diagnostics : Layout.diagnostic list;
  coresize : int;
}

let range_of (xs : int list) : range option =
  match xs with
  | [] -> None
  | x :: rest -> Some (List.fold_left (fun r y -> { min = Int.min r.min y; max = Int.max r.max y }) { min = x; max = x } rest)

let targets (es : edge list) : int list =
  List.filter_map (fun e -> match e with Next t | Jump t | Skip t -> Some t | Dynamic -> None) es

let control (op : opcode) : bool =
  match op with
  | IJMP | IJMZ | IJMN | IDJN | ISEQ | ISNE | ISLT | ICMP -> true
  | IDAT | ISPL | INOP | IMOV | IADD | ISUB | IMUL | IDIV | IMOD | ILDP | ISTP -> false

let back_edges (p : program) : (int * int) list = List.concat_map (fun (l : Layout.loop) -> l.back_edges) p.loops

(* Removing the depth-first back edges leaves an acyclic graph, so the shortest and longest paths
   are one pass in topological order: linear, where enumerating paths is exponential in the number
   of branches. [weight] counts a cell; [terminal] cells are reached but not left (the start is left
   when [expand_start]). *)
let dag_ranges (p : program) ~(allowed : int -> bool) ~(terminal : int -> bool) ~(weight : int -> int)
    ?(expand_start = true) (start : int) : range option array =
  let n = Array.length p.cells in
  let backs = back_edges p in
  let next i = List.filter (fun t -> allowed t && not (List.mem (i, t) backs)) (targets p.succ.(i)) in
  let leaves i = if i = start then expand_start else not (terminal i) in
  let state = Array.make n 0 and order = ref [] in
  let rec visit i =
    if state.(i) = 0 then begin
      state.(i) <- 1 ;
      if leaves i then List.iter (fun t -> if state.(t) <> 1 then visit t) (next i) ;
      state.(i) <- 2 ; order := i :: !order end in
  visit start ;
  let best = Array.make n None in
  best.(start) <- Some { min = weight start; max = weight start } ;
  List.iter (fun i -> match best.(i) with
    | Some r when leaves i ->
      List.iter (fun t -> let w = weight t in
        best.(t) <- Some (match best.(t) with
          | None -> { min = r.min + w; max = r.max + w }
          | Some b -> { min = Int.min b.min (r.min + w); max = Int.max b.max (r.max + w) })) (next i)
    | Some _ | None -> ()) !order ;
  best

(* The back edge that closes a loop is its last one: a construct's own jump back ends its body, and a
   threaded jump from an inner construct (Compile.thread_jumps) sits before it. *)
let closing_source (l : Layout.loop) : int = List.fold_left (fun m (s, _) -> max m s) min_int l.back_edges

let measure_loop (p : program) (l : Layout.loop) : loop_metrics =
  let in_body i = List.mem i l.body in
  let source = closing_source l in
  let c = p.cells.(source) in
  (* The construct the loop belongs to: the owner of the loop head it closes on, else (user code)
     the cell that closes it. *)
  let origin, construct = match l.owner with
    | Some (t, k) -> Some t, Some k
    | None -> c.origin, c.construct in
  (* A lap: from the header to a source of this loop's back edges; an inner loop counts one pass. *)
  let lap weight =
    let best = dag_ranges p ~allowed:in_body ~terminal:(fun _ -> false) ~weight l.header in
    range_of (List.concat_map (fun (s, _) -> match best.(s) with Some r -> [r.min; r.max] | None -> []) l.back_edges) in
  let cycles = lap (fun _ -> 1) in
  let overhead = lap (fun i -> if p.cells.(i).construct <> None && control p.cells.(i).op then 1 else 0) in
  (* Leaving: from the header to the first cell outside the loop, counting the loop construct's
     own exit jump, which sits outside the natural body. *)
  let own i = p.cells.(i).origin = origin && p.cells.(i).construct <> None && construct <> None in
  let inside i = in_body i || own i in
  let reach = dag_ranges p ~allowed:inside ~terminal:(fun _ -> false) ~weight:(fun _ -> 1) l.header in
  let exits = List.filter_map (fun i -> match reach.(i) with
    | Some r when List.exists (fun t -> not (inside t)) (targets p.succ.(i)) -> Some r.min
    | Some _ | None -> None) (List.init (Array.length p.cells) Fun.id) in
  let exit = Option.map (fun r -> r.min) (range_of exits) in
  let zero = { min = 0; max = 0 } in
  let label = match p.cells.(l.header).labels with x :: _ -> Some x | [] -> None in
  let own_control = lap (fun i -> if own i && control p.cells.(i).op then 1 else 0) in
  let test = List.find_map (fun i -> match p.cells.(i).op with
    | (IJMZ | IJMN | IDJN | ISEQ | ISNE | ISLT | ICMP) as op when own i -> Some op
    | IJMZ | IJMN | IDJN | ISEQ | ISNE | ISLT | ICMP | IDAT | ISPL | IJMP | INOP | IMOV | IADD | ISUB
    | IMUL | IDIV | IMOD | ILDP | ISTP -> None) (List.sort_uniq compare (l.header :: l.body)) in
  { loop = l; label; node = origin; construct;
    cycles = Option.value cycles ~default:zero; overhead = Option.value overhead ~default:zero;
    own = Option.value own_control ~default:zero; test; exit }

(* An immediate A operand makes the instruction itself the A-value (ICWS'94), so its own numbers
   are the divisors: .A/.AB divide by its A-number, .B/.BA by its B-number, .F/.X/.I by both. *)
let divides_by_zero (c : cell) : bool =
  let self_zero = c.a.mode = RImm && (match c.md with
    | RB | RBA -> c.b.value = 0
    | RN | RA | RAB -> c.a.value = 0
    | RF | RX | RI -> c.a.value = 0 || c.b.value = 0) in
  (c.op = IDIV || c.op = IMOD) && self_zero

let blank (c : cell) : bool =
  c.op = IDAT && (c.md = RN || c.md = RF)
  && c.a.mode = RDir && c.a.value = 0 && c.b.mode = RDir && c.b.value = 0

(* Predictions: heuristics for two patterns, reported as predictions and absent when nothing
   matches. *)
let writes (op : opcode) : bool =
  match op with
  | IMOV | IADD | ISUB | IMUL | IDIV | IMOD -> true
  | IDAT | ISPL | IJMP | INOP | IJMZ | IJMN | IDJN | ICMP | ISEQ | ISNE | ISLT | ILDP | ISTP -> false

let base_field (m : rmode) : Layout.field option =
  match m with
  | RBInd | RBInc | RBDec -> Some FB
  | RAInd | RAInc | RADec -> Some FA
  | RImm | RDir -> None

let rec gcd (a : int) (b : int) : int = if b = 0 then a else gcd b (a mod b)

(* The inverse of a modulo m (a and m coprime), by the extended Euclid. *)
let inverse (a : int) (m : int) : int =
  let rec go r0 r1 s0 s1 = if r1 = 0 then s0 else go r1 (r0 mod r1) s1 (s0 - (r0 / r1) * s1) in
  if m = 1 then 0 else ((go a m 1 0 mod m) + m) mod m

let step_of (p : program) (lm : loop_metrics) ~(cell : int) ~(field : Layout.field) ~(k : int) ~(written : bool) : prediction =
  let k = norm p.coresize k in
  let g = gcd p.coresize k in
  let period = p.coresize / g in
  (* The first cell of its own loop, other than its own, the pointer reaches from its initial value:
     after j iterations it points at cell + value + j*k, so x is reached when g divides
     x - cell - value, at j = (x - cell - value)/g * (k/g)^-1 modulo period. A write through it there
     hits the warrior's running code (the step-3 dwarf at iteration 2666). Its own cell is left out,
     where a B-field pointer reads 0 (a scanner sees it empty), and so is anything after it: a write
     there can overwrite the pointer itself (the core-clear's bomb resets it to 0 and it never gets
     further), so its value is not known past that iteration. *)
  let value = match field with FA -> p.cells.(cell).a.value | FB -> p.cells.(cell).b.value in
  let reached x =
    let d = norm p.coresize (x - cell - value) in
    if d mod g <> 0 then None else Some ((d / g) * inverse (k / g) period mod period) in
  let own = match reached cell with Some 0 | None -> period | Some j -> j in
  (* only a pointer something writes through can hit: a paper reads its own cells on purpose *)
  let hits = if not written then [] else List.filter_map (fun x ->
      if x = cell then None
      else match reached x with Some j when j < own -> Some (j, x) | Some _ | None -> None) lm.loop.body in
  let own_hit = match List.sort compare hits with
    | (j, x) :: _ -> Some (x, j, j * lm.cycles.max)
    | [] -> None in
  Step { loop = lm.loop.header; node = lm.node; cell; field; k; period;
         cover_cycles = period * lm.cycles.max; full = (g = 1); own_hit }

let steps (p : program) (lm : loop_metrics) : prediction list =
  let n = Array.length p.cells in
  let body = lm.loop.body in
  (* On every lap: no source of this loop's back edges is reachable from the header without it. *)
  let every i =
    let avoiding = dag_ranges p ~allowed:(fun t -> List.mem t body && t <> i) ~terminal:(fun _ -> false)
        ~weight:(fun _ -> 1) lm.loop.header in
    i = lm.loop.header || List.for_all (fun (s, _) -> s = i || avoiding.(s) = None) lm.loop.back_edges in
  let target (c : cell) = norm p.coresize (c.pos + c.b.value) in
  (* A field is a pointer when an operand in the loop goes through it (either operand, any
     instruction), or when it is the B-field of a write's own cell. *)
  let destination t f =
    List.exists (fun j -> let w = p.cells.(j) in
      (writes w.op && j = t && f = FB && w.b.mode = RDir)
      || List.exists (fun (o : operand) ->
          base_field o.mode = Some f && norm p.coresize (w.pos + o.value) = t) [w.a; w.b]) body in
  (* Every change to a pointer field in the loop: an ADD/SUB of a constant to it, or a write through
     it that moves it (< or >). *)
  let by_add = List.filter_map (fun i -> let c = p.cells.(i) in
      let field = match c.md with
        | RA | RBA -> Some FA | RB | RAB -> Some FB | RN | RF | RX | RI -> None in
      (* An immediate A operand makes the instruction itself the A-value (ICWS'94): .A and .AB add
         its A-number (the #k), .B and .BA its own B-number. *)
      let amount = match c.md with
        | RA | RAB -> c.a.value | RB | RBA -> c.b.value | RN | RF | RX | RI -> 0 in
      match field with
      | Some f when (c.op = IADD || c.op = ISUB) && c.a.mode = RImm && c.b.mode = RDir
                    && norm p.coresize amount <> 0 && target c < n && destination (target c) f ->
        Some ((target c, f), (if c.op = IADD then amount else - amount), i)
      | Some _ | None -> None) body in
  (* A < or > (or { or }) moves its pointer whenever the operand is evaluated, on either operand of
     any instruction. *)
  let by_mode = List.concat_map (fun j -> let w = p.cells.(j) in
      List.filter_map (fun (o : operand) ->
        let k = match o.mode with
          | RBInc | RAInc -> Some 1 | RBDec | RADec -> Some (-1)
          | RImm | RDir | RAInd | RBInd -> None in
        let t = norm p.coresize (w.pos + o.value) in
        match k, base_field o.mode with
        | Some k, Some f when t < n -> Some ((t, f), k, j)
        | (Some _ | None), (Some _ | None) -> None) [w.a; w.b]) body in
  (* A pointer's step is its net change over one lap: the sum of its changes, when each happens once
     on every lap. A change on some laps only, or inside an inner loop whose trip count is not known
     here, leaves the step unknown, and nothing is predicted rather than a wrong number. *)
  let inner = List.concat_map (fun (l' : Layout.loop) ->
      if l'.header <> lm.loop.header && List.mem l'.header body && List.for_all (fun i -> List.mem i body) l'.body
      then l'.body else []) p.loops in
  let changes = by_add @ by_mode in
  (* Any other write straight into a pointer's cell (MOV 7 p, MUL ...) changes it by an amount not
     known here. *)
  let overwritten (cell, _) = List.exists (fun j -> let w = p.cells.(j) in
      writes w.op && w.b.mode = RDir && target w = cell
      && not (List.exists (fun (_, _, i) -> i = j) by_add)) body in
  let pointers = List.fold_left (fun acc (ptr, _, _) -> if List.mem ptr acc then acc else acc @ [ptr]) [] changes in
  List.filter_map (fun ((cell, field) as ptr) ->
    let mine = List.filter (fun (q, _, _) -> q = ptr) changes in
    let k = List.fold_left (fun acc (_, k, _) -> acc + k) 0 mine in
    if List.for_all (fun (_, _, i) -> every i && not (List.mem i inner)) mine && not (overwritten ptr)
       && norm p.coresize k <> 0
    then
      (* written through: the B operand of an instruction that writes, going through this field *)
      let written = List.exists (fun j -> let w = p.cells.(j) in
          writes w.op && base_field w.b.mode = Some field && norm p.coresize (w.pos + w.b.value) = cell) body in
      Some (step_of p lm ~cell ~field ~k ~written)
    else None) pointers

let counter (p : program) (entry : range option) (lm : loop_metrics) : prediction option =
  let s = p.cells.(closing_source lm.loop) in
  let n_of_field (c : cell) = match s.md with
    | RA | RBA -> Some c.a.value | RB | RAB | RN -> Some c.b.value | RF | RX | RI -> None in
  let initial =
    if s.op <> IDJN || not (List.mem (Jump lm.loop.header) p.succ.(s.pos)) then None
    else if s.b.mode = RImm then n_of_field s
    else if s.b.mode = RDir then
      let t = norm p.coresize (s.pos + s.b.value) in
      (* a counter another instruction writes (Mice's MOV #7 before each pass) starts from a value
         not known here *)
      let rewritten = Array.exists (fun (w : cell) ->
          w.pos <> s.pos && writes w.op && w.b.mode = RDir && norm p.coresize (w.pos + w.b.value) = t) p.cells in
      if t < Array.length p.cells && not rewritten then n_of_field p.cells.(t) else None
    else None in
  Option.map (fun n ->
    (* DJN decrements first: a counter at 0 wraps and runs CORESIZE iterations. *)
    let n = if n = 0 then p.coresize else n in
    let loop_cycles = n * lm.cycles.max in
    let falls_on_dat = s.pos + 1 < Array.length p.cells && p.cells.(s.pos + 1).op = IDAT in
    (* Only a loop entered without passing another loop has a known start, and only one with no
       inner loop has a known length: a lap counts an inner loop as a single pass. *)
    let nested = List.exists (fun (o : Layout.loop) ->
        o <> lm.loop && List.for_all (fun i -> List.mem i lm.loop.body) o.body) p.loops in
    let dies_after = match entry with
      | Some r when falls_on_dat && not nested -> Some (r.max + loop_cycles + 1)
      | Some _ | None -> None in
    Counter { loop = lm.loop.header; node = lm.node; n; loop_cycles; dies_after })
    initial

let measure (p : program) : t =
  let cells = Array.to_list p.cells in
  let seen = reachable p in
  (* A cell never executed is data when it holds a variable, when it is a DAT the program names (a
     bomber's (label bomb) (DAT 0 0); a generated label names no data), or when an executed
     instruction reads or writes it, through a label or a number (a bomb that is an SPL, a cell
     (MOV x (Dir -1)) writes); anything else never executed is dead code. *)
  let named (c : cell) = List.exists user_label c.labels in
  let n = Array.length p.cells in
  let referenced = Array.make n false in
  (* An indirect operand also reaches the cell its pointer names, by the pointer's field as loaded
     (self-modification is not followed). *)
  let mark t = if t < n then referenced.(t) <- true in
  (* A jump's A operand is where control goes, not data it reads. *)
  let data_operands (c : cell) = match c.op with
    | IJMP | IJMZ | IJMN | IDJN | ISPL -> [c.b]
    | IDAT | INOP | IMOV | IADD | ISUB | IMUL | IDIV | IMOD | ICMP | ISEQ | ISNE | ISLT | ILDP | ISTP -> [c.a; c.b] in
  Array.iter (fun (c : cell) -> if seen.(c.pos) then
    List.iter (fun (o : operand) -> if o.mode <> RImm then begin
      let base = norm p.coresize (c.pos + o.value) in
      mark base ;
      if base < n then match base_field o.mode with
        | Some FA -> mark (norm p.coresize (base + p.cells.(base).a.value))
        | Some FB -> mark (norm p.coresize (base + p.cells.(base).b.value))
        | None -> ()
    end) (data_operands c)) p.cells ;
  let is_data (c : cell) = c.vars <> [] || (c.op = IDAT && named c) || referenced.(c.pos) in
  let count f = List.length (List.filter f cells) in
  let headers = List.sort_uniq compare (List.map (fun (l : Layout.loop) -> l.header) p.loops) in
  let is_header i = List.mem i headers in
  (* Cycles before each loop starts, over paths that pass no other loop's header. *)
  let entries =
    if headers = [] then Array.make (Array.length p.cells) None
    else dag_ranges p ~allowed:(fun _ -> true) ~terminal:is_header ~expand_start:(not (is_header p.entry))
        ~weight:(fun i -> if is_header i then 0 else 1) p.entry in
  let boot = range_of (List.concat_map (fun h -> match entries.(h) with
    | Some r -> [r.min; r.max] | None -> []) headers) in
  { length = Array.length p.cells;
    code = count (fun c -> c.role = Code);
    epilogue = count (fun c -> c.role = Epilogue);
    data = count (fun c -> c.role = Code && not seen.(c.pos) && is_data c);
    unreachable = count (fun c -> c.role = Code && not seen.(c.pos) && not (is_data c));
    dead = List.filter_map (fun c ->
        if c.role = Code && not seen.(c.pos) && not (is_data c) then Some (c.pos, c.origin) else None) cells;
    nonzero = count (fun c -> c.a.value <> 0 || c.b.value <> 0);
    nonblank = count (fun c -> not (blank c));
    boot;
    spl_sites = count (fun c -> c.op = ISPL);
    dynamic_jumps = count (fun c -> List.mem Dynamic p.succ.(c.pos));
    div_by_zero = List.map (fun c -> c.pos) (List.filter divides_by_zero cells);
    loops = List.map (measure_loop p) p.loops;
    predictions = [];
    diagnostics = p.diagnostics;
    coresize = p.coresize }
  |> fun m -> { m with predictions =
      List.concat_map (fun lm -> steps p lm @ Option.to_list (counter p entries.(lm.loop.header) lm)) m.loops }


(* The policy: objectives in priority order, compared lexicographically. Speed before size by
   default: one more instruction per loop iteration cost about 20 benchmark points where eight more
   cells cost about 4 (docs/specs/2026-10-03-cost-model-design.md). *)
type objective = Speed | Size | Stealth | Boot

type policy = objective list

let default_policy : policy = [Speed; Size]

let objective_of_string (s : string) : objective option =
  match s with
  | "speed" -> Some Speed | "size" -> Some Size | "stealth" -> Some Stealth | "boot" -> Some Boot
  | _ -> None

let string_of_objective (o : objective) : string =
  match o with Speed -> "speed" | Size -> "size" | Stealth -> "stealth" | Boot -> "boot"

let compare (policy : policy) (x : t) (y : t) : int =
  let worst m = List.fold_left (fun acc l -> Int.max acc l.cycles.max) 0 m.loops in
  let total m = List.fold_left (fun acc l -> acc + l.cycles.max) 0 m.loops in
  let boot m = match m.boot with Some r -> r.max | None -> max_int in
  let by o = match o with
    | Speed -> let c = Int.compare (worst x) (worst y) in if c <> 0 then c else Int.compare (total x) (total y)
    | Size -> Int.compare x.length y.length
    | Stealth -> Int.compare x.nonblank y.nonblank
    | Boot -> Int.compare (boot x) (boot y) in
  List.fold_left (fun acc o -> if acc <> 0 then acc else by o) 0 policy


(* Reports *)
(* The counter that bounds a loop, if one was predicted: its iterations. *)
let counter_of (m : t) (l : loop_metrics) : int option =
  List.find_map (fun pr -> match pr with
    | Counter c when c.loop = l.loop.header && c.node = l.node -> Some c.n
    | Counter _ | Step _ -> None) m.predictions

let show_diagnostic (d : Layout.diagnostic) : string =
  match d with
  | Undefined_label l -> sprintf "undefined label `%s` (pMARS rejects the warrior)" l
  | Duplicate_label (l, a, b) -> sprintf "label `%s` defined at cells %d and %d (pMARS keeps the first)" l a b
  | Long_line i -> sprintf "cell %d prints a line of 256 characters or more (pMARS hangs on it)" i

let show_range (r : range) : string =
  if r.min = r.max then string_of_int r.min else sprintf "%d..%d" r.min r.max

(* Which pointer a step is: two pointers of one loop can share a step (a paper's source and target). *)
let whose (cell : int) (field : Layout.field) : string =
  sprintf " (pointer in cell %d, %s-field)" cell (match field with FA -> "A" | FB -> "B")

(* A step is stored modulo CORESIZE; shown signed, so a predecrement reads -1, not 7999. *)
let signed (m : t) (k : int) : int = if k > m.coresize / 2 then k - m.coresize else k

let to_text ~(maxlength : int) (m : t) : string =
  let b = Buffer.create 256 in
  let add = Buffer.add_string b in
  (* Reachability follows no dynamic jump, so with any the counts are a static lower bound. *)
  let static = if m.dynamic_jumps > 0 then sprintf " (ignoring %d dynamic jumps)" m.dynamic_jumps else "" in
  add (sprintf "length %d/%d   code %d  data %d  epilogue %d  unreachable %d%s   nonzero %d  nonblank %d   boot %s  spl %d\n"
         m.length maxlength m.code m.data m.epilogue m.unreachable static m.nonzero m.nonblank
         (match m.boot with Some r -> show_range r | None -> "—") m.spl_sites) ;
  if m.dynamic_jumps > 0 then add (sprintf "dynamic jumps: %d\n" m.dynamic_jumps) ;
  List.iter (fun i -> add (sprintf "kills: cell %d divides by 0\n" i)) m.div_by_zero ;
  List.iter (fun d -> add (sprintf "diagnostic: %s\n" (show_diagnostic d))) m.diagnostics ;
  if m.loops = [] then add "loops: none\n" ;
  List.iter (fun l ->
    let first = List.hd l.loop.body and last = List.nth l.loop.body (List.length l.loop.body - 1) in
    add (sprintf "loop %d..%d (%s, %s, node %s)   cycles/iter %s   overhead %s   exit %s\n"
           first last (Option.value l.label ~default:"—") (Option.value l.construct ~default:"user code")
           (match l.node with Some t -> string_of_int t | None -> "—")
           (show_range l.cycles) (show_range l.overhead)
           (match l.exit with Some e -> string_of_int e | None -> "—")) ;
    (* A pointer only covers what the loop lets it: a counter or an exit can stop it first. *)
    let bounded = counter_of m l in
    let if_runs = if l.exit <> None && bounded = None then ", if the loop runs that long" else "" in
    let mine (loop, node) = loop = l.loop.header && node = l.node in
    List.iter (fun pr -> match pr with
      | Step s when mine (s.loop, s.node) && (match bounded with Some n -> n < s.period | None -> false) ->
        add (sprintf "  predicted: step %d%s → visits %d cells before the counter ends\n" (signed m s.k) (whose s.cell s.field)
               (Option.value bounded ~default:0))
      | Step s when mine (s.loop, s.node) && s.full ->
        add (sprintf "  predicted: step %d%s → period %d iterations, covers core in %d cycles%s\n"
               (signed m s.k) (whose s.cell s.field) s.period s.cover_cycles if_runs)
      | Step s when mine (s.loop, s.node) ->
        add (sprintf "  predicted: step %d%s → period %d iterations, does not visit every cell (%d cycles per period)%s\n"
               (signed m s.k) (whose s.cell s.field) s.period s.cover_cycles if_runs)
      | Step _ | Counter _ -> ()) m.predictions ;
    List.iter (fun pr -> match pr with
      | Step { own_hit = Some (x, j, c); loop; node; _ } when mine (loop, node) ->
        add (sprintf "  predicted: reaches cell %d of its own loop after %d iterations (%d cycles)\n" x j c)
      | Step _ | Counter _ -> ()) m.predictions ;
    List.iter (fun pr -> match pr with
      | Counter c when mine (c.loop, c.node) ->
        let nested = List.exists (fun o -> o != l && List.for_all (fun i -> List.mem i l.loop.body) o.loop.body) m.loops in
        add (sprintf "  predicted: counter %d → %d cycles in the loop%s%s\n" c.n c.loop_cycles
               (if nested then " (inner loops counted once)" else "")
               (match c.dies_after with Some d -> sprintf "; dies after %d instructions" d | None -> ""))
      | Step _ | Counter _ -> ()) m.predictions) m.loops ;
  Buffer.contents b

(* A JSON string: quote, backslash and control characters escaped; other bytes (UTF-8) as they are. *)
let json_string (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"' ;
  String.iter (fun c -> match c with
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | '\t' -> Buffer.add_string b "\\t"
    | '\r' -> Buffer.add_string b "\\r"
    | c when Char.code c < 0x20 -> Buffer.add_string b (sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char b c) s ;
  Buffer.add_char b '"' ;
  Buffer.contents b

let to_json ?policy ?optimizations (m : t) : string =
  let range r = sprintf "{\"min\":%d,\"max\":%d}" r.min r.max in
  let opt f = function Some x -> f x | None -> "null" in
  let str = json_string in
  let ints xs = "[" ^ String.concat "," (List.map string_of_int xs) ^ "]" in
  let field f = match f with FA -> "\"A\"" | FB -> "\"B\"" in
  let prediction pr = match pr with
    | Step s -> sprintf "{\"kind\":\"step\",\"loop\":%d,\"cell\":%d,\"field\":%s,\"k\":%d,\"period\":%d,\"cover_cycles\":%d,\"full\":%b,\"own_hit\":%s}"
                  s.loop s.cell (field s.field) s.k s.period s.cover_cycles s.full
                  (match s.own_hit with
                   | Some (x, j, c) -> sprintf "{\"cell\":%d,\"iterations\":%d,\"cycles\":%d}" x j c
                   | None -> "null")
    | Counter c -> sprintf "{\"kind\":\"counter\",\"loop\":%d,\"n\":%d,\"loop_cycles\":%d,\"dies_after\":%s}"
                     c.loop c.n c.loop_cycles (opt string_of_int c.dies_after) in
  let diagnostics = "[" ^ String.concat "," (List.map (fun d -> str (show_diagnostic d)) m.diagnostics) ^ "]" in
  let loop l = sprintf "{\"header\":%d,\"label\":%s,\"body\":%s,\"node\":%s,\"construct\":%s,\"cycles\":%s,\"overhead\":%s,\"exit\":%s}"
      l.loop.header (opt str l.label) (ints l.loop.body) (opt string_of_int l.node) (opt str l.construct)
      (range l.cycles) (range l.overhead) (opt string_of_int l.exit) in
  sprintf "{\"length\":%d,\"code\":%d,\"data\":%d,\"epilogue\":%d,\"unreachable\":%d,\"nonzero\":%d,\"nonblank\":%d,\"boot\":%s,\"spl_sites\":%d,\"dynamic_jumps\":%d,\"div_by_zero\":%s,\"loops\":[%s],\"predictions\":[%s],\"diagnostics\":%s,\"coresize\":%d%s}"
    m.length m.code m.data m.epilogue m.unreachable m.nonzero m.nonblank (opt range m.boot)
    m.spl_sites m.dynamic_jumps (ints m.div_by_zero) (String.concat "," (List.map loop m.loops)) (String.concat "," (List.map prediction m.predictions)) diagnostics m.coresize
    (match policy with
     | Some ps -> sprintf ",\"policy\":[%s]" (String.concat "," (List.map (fun o -> str (string_of_objective o)) ps))
     | None -> "")
    ^ (match optimizations with
       | Some os -> sprintf ",\"optimizations\":[%s]" (String.concat "," (List.map str os))
       | None -> "")
