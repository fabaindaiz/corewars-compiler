(** Expect: the programmer's expectations, checked against the metrics or exported as probes **)
open Printf
open Ast
open Metrics

type outcome = Pass | Fail of string

(* Each expectation with the tag of its innermost enclosing loop construct; None outside any. *)
let collect (e : meta eexpr) : (expectation * tag option) list =
  let rec go loop e = match e with
    | EExpect (x, _) -> [(x, loop)]
    | EFlow1 ((Repeat _ | While | DoWhile), _, body, m) -> go (Some m.tag) body
    | EFlow1 (If, _, body, _) | ELet (_, _, body, _) -> go loop body
    | EFlow2 (IfElse, _, b1, b2, _) -> go loop b1 @ go loop b2
    | ESeq (es, _) -> List.concat_map (go loop) es
    | EComment _ | ELabel _ | EPrim2 _ -> [] in
  go None e

let bound (c : cmp) : string = match c with Eq -> "" | Le -> "<= "

let holds (c : cmp) (n : int) (v : int) : bool = match c with Eq -> v = n | Le -> v <= n

let describe (x : expectation) : string =
  match x with
  | XLength (c, n) -> sprintf "expect length %s%d" (bound c) n
  | XCycles (c, n) -> sprintf "expect cycles %s%d" (bound c) n
  | XOverhead (c, n) -> sprintf "expect overhead %s%d" (bound c) n
  | XBoot (c, n) -> sprintf "expect boot %s%d" (bound c) n
  | XStep k -> sprintf "expect step %d" k
  | XCoversCore -> "expect covers-core"
  | XAlive n -> sprintf "expect alive %d" n
  | XDead n -> sprintf "expect dead %d" n
  | XCell (a, text, n) -> sprintf "expect cell %d \"%s\" %d" a text n

(* The loops an expectation speaks about: the one its construct encloses, or all of them. *)
let scope (m : t) (loop : tag option) : (loop_metrics list, string) result =
  match loop with
  | Some t ->
    (match List.filter (fun l -> l.node = Some t) m.loops with
    | [] -> Error "no loop encloses this expectation"
    | ls -> Ok ls)
  | None -> if m.loops = [] then Error "the program has no loop" else Ok m.loops

let slowest (ls : loop_metrics list) : loop_metrics =
  List.fold_left (fun a l -> if l.cycles.max > a.cycles.max then l else a) (List.hd ls) ls

let name (l : loop_metrics) : string =
  sprintf "the %s of node %s" (Option.value l.construct ~default:"loop")
    (match l.node with Some t -> string_of_int t | None -> "?")

(* The fixed-step pointers of these loops: (signed step, period, full, the loop). *)
let steps_in (m : t) (ls : loop_metrics list) : (int * int * bool * loop_metrics) list =
  List.filter_map (fun p -> match p with
    | Step s ->
      Option.map (fun l -> (signed m s.k, s.period, s.full, l))
        (List.find_opt (fun l -> l.loop.header = s.loop && l.node = s.node) ls)
    | Counter _ -> None) m.predictions

(* Whether one pointer visits every cell before its loop can stop. *)
let covers (m : t) ((k, period, full, l) : int * int * bool * loop_metrics) : (unit, string) result =
  if not full then Error (sprintf "the pointer steps by %d and visits %d of %d cells" k period m.coresize)
  else match counter_of m l with
    | Some n when n < period -> Error (sprintf "the loop ends after %d iterations, visiting %d of %d cells" n n m.coresize)
    | Some _ -> Ok ()
    | None -> if l.exit <> None then Error "the loop can exit before visiting every cell" else Ok ()

let check (m : t) ((x, loop) : expectation * tag option) : outcome option =
  let fail detail = Some (Fail (sprintf "%s: %s" (describe x) detail)) in
  let verdict ok detail = if ok then Some Pass else fail detail in
  let in_scope f = match scope m loop with Error e -> fail e | Ok ls -> f ls in
  match x with
  | XLength (c, n) -> verdict (holds c n m.length) (sprintf "the warrior is %d cells" m.length)
  | XBoot (c, n) ->
    (match m.boot with
    | None -> fail "the program has no loop"
    | Some b -> verdict (holds c n b.max) (sprintf "the first loop starts after %d cycles" b.max))
  | XCycles (c, n) -> in_scope (fun ls -> let l = slowest ls in
      verdict (holds c n l.cycles.max) (sprintf "%s executes %d per iteration" (name l) l.cycles.max))
  | XOverhead (c, n) -> in_scope (fun ls ->
      let l = List.fold_left (fun a l -> if l.overhead.max > a.overhead.max then l else a) (List.hd ls) ls in
      verdict (holds c n l.overhead.max) (sprintf "%s spends %d per iteration on control" (name l) l.overhead.max))
  | XStep k -> in_scope (fun ls -> match steps_in m ls with
      | [] -> fail "no fixed-step pointer found"
      | steps when List.exists (fun (s, _, _, _) -> s = k) steps -> Some Pass
      | (s, _, _, _) :: _ -> fail (sprintf "the pointer steps by %d" s))
  | XCoversCore -> in_scope (fun ls -> match steps_in m ls with
      | [] -> fail "no fixed-step pointer found"
      | first :: _ as steps ->
        if List.exists (fun s -> covers m s = Ok ()) steps then Some Pass
        else match covers m first with Error e -> fail e | Ok () -> Some Pass)
  | XAlive _ | XDead _ | XCell _ -> None

(* The execution kinds, as a behaviour spec for tools/behave.py (`redcode:` relative to the spec). *)
(* hill: the hill the warrior names, so tools/behave.py runs it under that hill's settings. *)
let to_beh ?hill ~(redcode : string) (xs : expectation list) : string =
  let probe x = match x with
    | XAlive n -> Some (sprintf "alive %d" n)
    | XDead n -> Some (sprintf "dead %d" n)
    | XCell (a, text, n) -> Some (sprintf "cell %d %d %s" n a text)
    | XLength _ | XCycles _ | XOverhead _ | XBoot _ | XStep _ | XCoversCore -> None in
  String.concat "" (List.map (fun l -> l ^ "\n")
    ("# written by run_compile.exe --emit-beh" :: ("redcode: " ^ redcode)
     :: Option.to_list (Option.map (fun h -> "hill: " ^ h) hill) @ List.filter_map probe xs))
