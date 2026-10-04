(** Optimize: the policy chooses among the optional transformations by measuring each **)
open Compile

(* Every combination of the optional transformations, fewest first: the choice keeps the earliest of
   equally good variants, so a transformation is applied only when it improves the first objective
   of the policy that tells the variants apart (docs/specs/2026-10-04-optimizer-design.md). *)
let candidates : options list =
  let all = List.concat_map (fun rotate_unary ->
      List.map (fun rotate_binary -> { rotate_unary; rotate_binary }) [false; true]) [false; true] in
  let applied o = List.length (List.filter Fun.id [o.rotate_unary; o.rotate_binary]) in
  List.stable_sort (fun a b -> Int.compare (applied a) (applied b)) all

(* The whole program is compiled once per candidate: at most 100 cells each, so eight compiles cost
   nothing next to one pMARS run. *)
let choose (policy : Metrics.policy) (e : Ast.expr) : options * emitted list * Metrics.t =
  let measure o = let body = compile_body ~opts:o e in (o, body, Metrics.measure (Layout.build body)) in
  match List.map measure candidates with
  | [] -> failwith "Optimize.choose: no candidate"
  | first :: rest ->
    List.fold_left (fun ((_, _, mb) as best) ((_, _, m) as c) ->
      if Metrics.compare policy m mb < 0 then c else best) first rest

(* For the report: the transformations applied, by name. *)
let describe (o : options) : string =
  match List.filter_map (fun (on, name) -> if on then Some name else None)
          [(o.rotate_unary, "rotate-unary"); (o.rotate_binary, "rotate-binary")] with
  | [] -> "none"
  | names -> String.concat ", " names

let compile_body (policy : Metrics.policy) (e : Ast.expr) : emitted list =
  let _, body, _ = choose policy e in body

let compile_prog (policy : Metrics.policy) (e : Ast.expr) : string =
  let opts, _, _ = choose policy e in Compile.compile_prog ~opts e
