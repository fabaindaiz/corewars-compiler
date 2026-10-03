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
  exit : int option;
}

type prediction =
| Step of { loop : int; node : tag option; cell : int; field : Layout.field; k : int; period : int;
            cover_cycles : int; full : bool }
| Counter of { loop : int; node : tag option; n : int; loop_cycles : int; dies_after : int option }

type t = {
  length : int;
  code : int;
  data : int;
  epilogue : int;
  unreachable : int;
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

let measure_loop (p : program) (l : Layout.loop) : loop_metrics =
  let in_body i = List.mem i l.body in
  let source = fst (List.hd l.back_edges) in
  let c = p.cells.(source) in
  (* A lap: from the header to a source of this loop's back edges; an inner loop counts one pass. *)
  let lap weight =
    let best = dag_ranges p ~allowed:in_body ~terminal:(fun _ -> false) ~weight l.header in
    range_of (List.concat_map (fun (s, _) -> match best.(s) with Some r -> [r.min; r.max] | None -> []) l.back_edges) in
  let cycles = lap (fun _ -> 1) in
  let overhead = lap (fun i -> if p.cells.(i).construct <> None && control p.cells.(i).op then 1 else 0) in
  (* Leaving: from the header to the first cell outside the loop, counting the loop construct's
     own exit jump, which sits outside the natural body. *)
  let own i = p.cells.(i).origin = c.origin && p.cells.(i).construct <> None && c.construct <> None in
  let inside i = in_body i || own i in
  let reach = dag_ranges p ~allowed:inside ~terminal:(fun _ -> false) ~weight:(fun _ -> 1) l.header in
  let exits = List.filter_map (fun i -> match reach.(i) with
    | Some r when List.exists (fun t -> not (inside t)) (targets p.succ.(i)) -> Some r.min
    | Some _ | None -> None) (List.init (Array.length p.cells) Fun.id) in
  let exit = Option.map (fun r -> r.min) (range_of exits) in
  let zero = { min = 0; max = 0 } in
  let label = match p.cells.(l.header).labels with x :: _ -> Some x | [] -> None in
  { loop = l; label; node = c.origin; construct = c.construct;
    cycles = Option.value cycles ~default:zero; overhead = Option.value overhead ~default:zero; exit }

let divides_by_zero (c : cell) : bool =
  let self_zero = c.a.mode = RImm && (match c.md with
    | RB | RBA -> c.b.value = 0
    | RN | RA | RAB | RF | RX | RI -> c.a.value = 0) in
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

let step_of (p : program) (lm : loop_metrics) ~(cell : int) ~(field : Layout.field) ~(k : int) : prediction =
  let k = norm p.coresize k in
  let period = p.coresize / gcd p.coresize k in
  Step { loop = lm.loop.header; node = lm.node; cell; field; k; period;
         cover_cycles = period * lm.cycles.max; full = (gcd p.coresize k = 1) }

let steps (p : program) (lm : loop_metrics) : prediction list =
  let n = Array.length p.cells in
  let body = lm.loop.body in
  (* On every lap: no source of this loop's back edges is reachable from the header without it. *)
  let every i =
    let avoiding = dag_ranges p ~allowed:(fun t -> List.mem t body && t <> i) ~terminal:(fun _ -> false)
        ~weight:(fun _ -> 1) lm.loop.header in
    i = lm.loop.header || List.for_all (fun (s, _) -> s = i || avoiding.(s) = None) lm.loop.back_edges in
  let target (c : cell) = norm p.coresize (c.pos + c.b.value) in
  (* A field is a destination when a write in the loop goes to it: the cell's own B operand, or a
     write whose B operand is indirect through it. *)
  let destination t f =
    List.exists (fun j -> let w = p.cells.(j) in
      writes w.op && ((j = t && f = FB && w.b.mode = RDir)
                      || (base_field w.b.mode = Some f && target w = t))) body in
  let by_add = List.filter_map (fun i -> let c = p.cells.(i) in
      let field = match c.md with
        | RA | RBA -> Some FA | RB | RAB -> Some FB | RN | RF | RX | RI -> None in
      (* An immediate A operand makes the instruction itself the A-value (ICWS'94): .A and .AB add
         its A-number (the #k), .B and .BA its own B-number. *)
      let amount = match c.md with
        | RA | RAB -> c.a.value | RB | RBA -> c.b.value | RN | RF | RX | RI -> 0 in
      match field with
      | Some f when (c.op = IADD || c.op = ISUB) && c.a.mode = RImm && c.b.mode = RDir && every i
                    && norm p.coresize amount <> 0 && target c < n && destination (target c) f ->
        let k = if c.op = IADD then amount else - amount in
        Some (step_of p lm ~cell:(target c) ~field:f ~k)
      | Some _ | None -> None) body in
  let by_mode = List.filter_map (fun j -> let w = p.cells.(j) in
      let k = match w.b.mode with
        | RBInc | RAInc -> Some 1 | RBDec | RADec -> Some (-1)
        | RImm | RDir | RAInd | RBInd -> None in
      match k, base_field w.b.mode with
      | Some k, Some f when writes w.op && every j && target w < n ->
        Some (step_of p lm ~cell:(target w) ~field:f ~k)
      | (Some _ | None), (Some _ | None) -> None) body in
  by_add @ by_mode

let counter (p : program) (entry : range option) (lm : loop_metrics) : prediction option =
  let s = p.cells.(fst (List.hd lm.loop.back_edges)) in
  let n_of_field (c : cell) = match s.md with
    | RA | RBA -> Some c.a.value | RB | RAB | RN -> Some c.b.value | RF | RX | RI -> None in
  let initial =
    if s.op <> IDJN || not (List.mem (Jump lm.loop.header) p.succ.(s.pos)) then None
    else if s.b.mode = RImm then n_of_field s
    else if s.b.mode = RDir then
      let t = norm p.coresize (s.pos + s.b.value) in
      if t < Array.length p.cells then n_of_field p.cells.(t) else None
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
  let count f = List.length (List.filter f cells) in
  let headers = List.sort_uniq compare (List.map (fun (l : Layout.loop) -> l.header) p.loops) in
  let is_header i = List.mem i headers in
  (* Cycles before each loop starts, over paths that pass no other loop's header. *)
  let entries =
    if headers = [] then Array.make (Array.length p.cells) None
    else dag_ranges p ~allowed:(fun _ -> true) ~terminal:is_header ~expand_start:(not (is_header 0))
        ~weight:(fun i -> if is_header i then 0 else 1) 0 in
  let boot = range_of (List.concat_map (fun h -> match entries.(h) with
    | Some r -> [r.min; r.max] | None -> []) headers) in
  { length = Array.length p.cells;
    code = count (fun c -> c.role = Code);
    epilogue = count (fun c -> c.role = Epilogue);
    data = count (fun c -> c.role = Code && not seen.(c.pos) && c.vars <> []);
    unreachable = count (fun c -> c.role = Code && not seen.(c.pos) && c.vars = []);
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

(* A step is stored modulo CORESIZE; shown signed, so a predecrement reads -1, not 7999. *)
let signed (m : t) (k : int) : int = if k > m.coresize / 2 then k - m.coresize else k

let to_text ~(maxlength : int) (m : t) : string =
  let b = Buffer.create 256 in
  let add = Buffer.add_string b in
  add (sprintf "length %d/%d   code %d  data %d  epilogue %d  unreachable %d   nonzero %d  nonblank %d   boot %s  spl %d\n"
         m.length maxlength m.code m.data m.epilogue m.unreachable m.nonzero m.nonblank
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
        add (sprintf "  predicted: step %d → visits %d cells before the counter ends\n" (signed m s.k)
               (Option.value bounded ~default:0))
      | Step s when mine (s.loop, s.node) && s.full ->
        add (sprintf "  predicted: step %d → period %d iterations, covers core in %d cycles%s\n"
               (signed m s.k) s.period s.cover_cycles if_runs)
      | Step s when mine (s.loop, s.node) ->
        add (sprintf "  predicted: step %d → period %d iterations, does not visit every cell (%d cycles per period)%s\n"
               (signed m s.k) s.period s.cover_cycles if_runs)
      | Counter c when mine (c.loop, c.node) ->
        let nested = List.exists (fun o -> o != l && List.for_all (fun i -> List.mem i l.loop.body) o.loop.body) m.loops in
        add (sprintf "  predicted: counter %d → %d cycles in the loop%s%s\n" c.n c.loop_cycles
               (if nested then " (inner loops counted once)" else "")
               (match c.dies_after with Some d -> sprintf "; dies after %d instructions" d | None -> ""))
      | Step _ | Counter _ -> ()) m.predictions) m.loops ;
  Buffer.contents b

let to_json (m : t) : string =
  let range r = sprintf "{\"min\":%d,\"max\":%d}" r.min r.max in
  let opt f = function Some x -> f x | None -> "null" in
  let str s = sprintf "\"%s\"" (String.escaped s) in
  let ints xs = "[" ^ String.concat "," (List.map string_of_int xs) ^ "]" in
  let field f = match f with FA -> "\"A\"" | FB -> "\"B\"" in
  let prediction pr = match pr with
    | Step s -> sprintf "{\"kind\":\"step\",\"loop\":%d,\"cell\":%d,\"field\":%s,\"k\":%d,\"period\":%d,\"cover_cycles\":%d,\"full\":%b}"
                  s.loop s.cell (field s.field) s.k s.period s.cover_cycles s.full
    | Counter c -> sprintf "{\"kind\":\"counter\",\"loop\":%d,\"n\":%d,\"loop_cycles\":%d,\"dies_after\":%s}"
                     c.loop c.n c.loop_cycles (opt string_of_int c.dies_after) in
  let diagnostics = "[" ^ String.concat "," (List.map (fun d -> str (show_diagnostic d)) m.diagnostics) ^ "]" in
  let loop l = sprintf "{\"header\":%d,\"label\":%s,\"body\":%s,\"node\":%s,\"construct\":%s,\"cycles\":%s,\"overhead\":%s,\"exit\":%s}"
      l.loop.header (opt str l.label) (ints l.loop.body) (opt string_of_int l.node) (opt str l.construct)
      (range l.cycles) (range l.overhead) (opt string_of_int l.exit) in
  sprintf "{\"length\":%d,\"code\":%d,\"data\":%d,\"epilogue\":%d,\"unreachable\":%d,\"nonzero\":%d,\"nonblank\":%d,\"boot\":%s,\"spl_sites\":%d,\"dynamic_jumps\":%d,\"div_by_zero\":%s,\"loops\":[%s],\"predictions\":[%s],\"diagnostics\":%s}"
    m.length m.code m.data m.epilogue m.unreachable m.nonzero m.nonblank (opt range m.boot)
    m.spl_sites m.dynamic_jumps (ints m.div_by_zero) (String.concat "," (List.map loop m.loops)) (String.concat "," (List.map prediction m.predictions)) diagnostics
