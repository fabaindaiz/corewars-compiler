(** Optimize: the policy chooses among the optional transformations by measuring each **)
open Compile

(* Every combination of the optional transformations, fewest first: the choice keeps the earliest of
   equally good variants, so a transformation is applied only when it improves the first objective
   of the policy that tells the variants apart (docs/specs/2026-10-04-optimizer-design.md). *)
let candidates : options list =
  let bools = [false; true] in
  let all = List.concat_map (fun rotate_unary -> List.concat_map (fun rotate_binary ->
      List.map (fun peephole -> { rotate_unary; rotate_binary; peephole }) bools) bools) bools in
  let applied o = List.length (List.filter Fun.id [o.rotate_unary; o.rotate_binary; o.peephole]) in
  List.stable_sort (fun a b -> Int.compare (applied a) (applied b)) all

(* The whole program is compiled once per candidate: at most 100 cells each, so eight compiles cost
   nothing next to one pMARS run. *)
let measure_all ?(consts = []) ?coresize ?start (e : Ast.expr) : (options * emitted list * Metrics.t) list =
  List.map (fun o ->
    let body = compile_body ~opts:o ~consts ?coresize e in
    (o, body, Metrics.measure (Layout.build ?coresize ~consts ?start body))) candidates

let pick (policy : Metrics.policy) (variants : (options * emitted list * Metrics.t) list) : options * emitted list * Metrics.t =
  match variants with
  | [] -> failwith "Optimize.pick: no candidate"
  | first :: rest ->
    List.fold_left (fun ((_, _, mb) as best) ((_, _, m) as c) ->
      if Metrics.compare policy m mb < 0 then c else best) first rest

let choose ?consts (policy : Metrics.policy) (e : Ast.expr) : options * emitted list * Metrics.t =
  pick policy (measure_all ?consts e)

(* For the report: the transformations applied, by name. *)
let names (o : options) : string list =
  List.filter_map (fun (on, name) -> if on then Some name else None)
    [(o.rotate_unary, "rotate-unary"); (o.rotate_binary, "rotate-binary"); (o.peephole, "peephole")]

let describe (o : options) : string =
  match names o with
  | [] -> "none"
  | names -> String.concat ", " names

let compile_body ?consts (policy : Metrics.policy) (e : Ast.expr) : emitted list =
  let _, body, _ = choose ?consts policy e in body

let compile_prog ?consts (policy : Metrics.policy) (e : Ast.expr) : string =
  let opts, _, _ = choose ?consts policy e in Compile.compile_prog ~opts ?consts e
