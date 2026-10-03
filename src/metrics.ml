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
| Step of { loop : int; cell : int; field : Layout.field; k : int; period : int; cover_cycles : int; full : bool }
| Counter of { loop : int; n : int; loop_cycles : int; dies_after : int option }

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

(* Simple paths from [start] while [inside] holds, each cell at most once; [stop t] ends a path
   at successor t and records it. Warriors are at most a few hundred cells with few branches. *)
let paths (p : program) ~(inside : int -> bool) ~(stop : int -> bool) (start : int) : int list list =
  let found = ref [] in
  let rec go i trail =
    let trail = i :: trail in
    List.iter (fun t ->
      if stop t then found := List.rev trail :: !found
      else if inside t && not (List.mem t trail) then go t trail) (targets p.succ.(i)) in
  go start [] ; !found

let measure_loop (p : program) (l : Layout.loop) : loop_metrics =
  let in_body i = List.mem i l.body in
  let source = fst (List.hd l.back_edges) in
  let c = p.cells.(source) in
  let laps = paths p ~inside:(fun i -> in_body i && i <> l.header) ~stop:(fun t -> t = l.header) l.header in
  let count f = List.map (fun path -> List.length (List.filter f path)) laps in
  let cycles = range_of (count (fun _ -> true)) in
  let overhead = range_of (count (fun i -> p.cells.(i).construct <> None && control p.cells.(i).op)) in
  (* Leaving: from the header to the first cell outside the loop, counting the loop construct's
     own exit jump, which sits outside the natural body. *)
  let own i = p.cells.(i).origin = c.origin && p.cells.(i).construct <> None && c.construct <> None in
  let outs = paths p ~inside:(fun i -> in_body i || own i) ~stop:(fun t -> not (in_body t || own t)) l.header in
  let exit = Option.map (fun r -> r.min) (range_of (List.map List.length outs)) in
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
  Step { loop = lm.loop.header; cell; field; k; period; cover_cycles = period * lm.cycles.max;
         full = (gcd p.coresize k = 1) }

let steps (p : program) (lm : loop_metrics) : prediction list =
  let n = Array.length p.cells in
  let body = lm.loop.body in
  let laps = paths p ~inside:(fun i -> List.mem i body && i <> lm.loop.header)
      ~stop:(fun t -> t = lm.loop.header) lm.loop.header in
  let every i = laps <> [] && List.for_all (List.mem i) laps in
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
      match field with
      | Some f when (c.op = IADD || c.op = ISUB) && c.a.mode = RImm && c.b.mode = RDir && every i
                    && target c < n && destination (target c) f ->
        let k = if c.op = IADD then c.a.value else - c.a.value in
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

let counter (p : program) (boot : range option) (lm : loop_metrics) : prediction option =
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
    let start = match boot with Some r -> r.max | None -> 0 in
    Counter { loop = lm.loop.header; n; loop_cycles;
              dies_after = if falls_on_dat then Some (start + loop_cycles + 1) else None })
    initial

let measure (p : program) : t =
  let cells = Array.to_list p.cells in
  let seen = reachable p in
  let count f = List.length (List.filter f cells) in
  let headers = List.map (fun (l : Layout.loop) -> l.header) p.loops in
  let boot =
    if headers = [] then None
    else if List.mem 0 headers then Some { min = 0; max = 0 }
    else range_of (List.map List.length (paths p ~inside:(fun _ -> true) ~stop:(fun t -> List.mem t headers) 0)) in
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
    coresize = p.coresize }
  |> fun m -> { m with predictions =
      List.concat_map (fun lm -> steps p lm @ Option.to_list (counter p m.boot lm)) m.loops }


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
  if m.loops = [] then add "loops: none\n" ;
  List.iter (fun l ->
    let first = List.hd l.loop.body and last = List.nth l.loop.body (List.length l.loop.body - 1) in
    add (sprintf "loop %d..%d (%s, %s, node %s)   cycles/iter %s   overhead %s   exit %s\n"
           first last (Option.value l.label ~default:"—") (Option.value l.construct ~default:"user code")
           (match l.node with Some t -> string_of_int t | None -> "—")
           (show_range l.cycles) (show_range l.overhead)
           (match l.exit with Some e -> string_of_int e | None -> "—")) ;
    List.iter (fun pr -> match pr with
      | Step s when s.loop = l.loop.header && s.full ->
        add (sprintf "  predicted: step %d → period %d iterations, covers core in %d cycles\n" (signed m s.k) s.period s.cover_cycles)
      | Step s when s.loop = l.loop.header ->
        add (sprintf "  predicted: step %d → period %d iterations, does not visit every cell (%d cycles per period)\n"
               (signed m s.k) s.period s.cover_cycles)
      | Counter c when c.loop = l.loop.header ->
        add (sprintf "  predicted: counter %d → %d cycles in the loop%s\n" c.n c.loop_cycles
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
  let loop l = sprintf "{\"header\":%d,\"label\":%s,\"body\":%s,\"node\":%s,\"construct\":%s,\"cycles\":%s,\"overhead\":%s,\"exit\":%s}"
      l.loop.header (opt str l.label) (ints l.loop.body) (opt string_of_int l.node) (opt str l.construct)
      (range l.cycles) (range l.overhead) (opt string_of_int l.exit) in
  sprintf "{\"length\":%d,\"code\":%d,\"data\":%d,\"epilogue\":%d,\"unreachable\":%d,\"nonzero\":%d,\"nonblank\":%d,\"boot\":%s,\"spl_sites\":%d,\"dynamic_jumps\":%d,\"div_by_zero\":%s,\"loops\":[%s],\"predictions\":[%s]}"
    m.length m.code m.data m.epilogue m.unreachable m.nonzero m.nonblank (opt range m.boot)
    m.spl_sites m.dynamic_jumps (ints m.div_by_zero) (String.concat "," (List.map loop m.loops)) (String.concat "," (List.map prediction m.predictions))
