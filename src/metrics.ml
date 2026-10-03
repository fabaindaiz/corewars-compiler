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
  { loop = l; node = c.origin; construct = c.construct;
    cycles = Option.value cycles ~default:zero; overhead = Option.value overhead ~default:zero; exit }

let divides_by_zero (c : cell) : bool =
  let self_zero = c.a.mode = RImm && (match c.md with
    | RB | RBA -> c.b.value = 0
    | RN | RA | RAB | RF | RX | RI -> c.a.value = 0) in
  (c.op = IDIV || c.op = IMOD) && self_zero

let blank (c : cell) : bool =
  c.op = IDAT && (c.md = RN || c.md = RF)
  && c.a.mode = RDir && c.a.value = 0 && c.b.mode = RDir && c.b.value = 0

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
    predictions = [] }


(* Reports *)
let show_range (r : range) : string =
  if r.min = r.max then string_of_int r.min else sprintf "%d..%d" r.min r.max

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
    add (sprintf "loop %d..%d (%s, node %s)   cycles/iter %s   overhead %s   exit %s\n"
           first last (Option.value l.construct ~default:"user code")
           (match l.node with Some t -> string_of_int t | None -> "—")
           (show_range l.cycles) (show_range l.overhead)
           (match l.exit with Some e -> string_of_int e | None -> "—"))) m.loops ;
  Buffer.contents b

let to_json (m : t) : string =
  let range r = sprintf "{\"min\":%d,\"max\":%d}" r.min r.max in
  let opt f = function Some x -> f x | None -> "null" in
  let str s = sprintf "\"%s\"" (String.escaped s) in
  let ints xs = "[" ^ String.concat "," (List.map string_of_int xs) ^ "]" in
  let loop l = sprintf "{\"header\":%d,\"body\":%s,\"node\":%s,\"construct\":%s,\"cycles\":%s,\"overhead\":%s,\"exit\":%s}"
      l.loop.header (ints l.loop.body) (opt string_of_int l.node) (opt str l.construct)
      (range l.cycles) (range l.overhead) (opt string_of_int l.exit) in
  sprintf "{\"length\":%d,\"code\":%d,\"data\":%d,\"epilogue\":%d,\"unreachable\":%d,\"nonzero\":%d,\"nonblank\":%d,\"boot\":%s,\"spl_sites\":%d,\"dynamic_jumps\":%d,\"div_by_zero\":%s,\"loops\":[%s],\"predictions\":[]}"
    m.length m.code m.data m.epilogue m.unreachable m.nonzero m.nonblank (opt range m.boot)
    m.spl_sites m.dynamic_jumps (ints m.div_by_zero) (String.concat "," (List.map loop m.loops))
