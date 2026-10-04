(** Warnings: a cost the measured program pays that could be removed, said where it comes from **)
open Printf
open Red
open Ast

type warning = { loc : loc option; message : string }

(* Which warnings are given: the ones about the policy's objectives (default), all, or none. *)
type mode = Policy | All | Nothing

(* The fewest control instructions per iteration a loop construct can spend: a repeat its JMP; a
   loop whose test is a single conditional jump, that jump (a while, once rotated); a comparison,
   the test and a jump. A DZ while has no single post-test. *)
let minimum (l : Metrics.loop_metrics) : int option =
  let single = match l.test with
    | Some (IJMZ | IJMN) -> Some true
    | Some IDJN -> Some (l.construct = Some "do-while")
    | Some (ISEQ | ISNE | ISLT | ICMP) -> Some false
    | Some (IDAT | ISPL | IJMP | INOP | IMOV | IADD | ISUB | IMUL | IDIV | IMOD | ILDP | ISTP) | None -> None in
  match l.construct, single with
  | Some "repeat", _ -> Some 1
  | Some ("while" | "do-while"), Some true -> Some 1
  | Some ("while" | "do-while"), Some false -> Some 2
  | (Some _ | None), (Some _ | None) -> None

(* faster: some variant of the program runs faster than the chosen one (Optimize.measure_all), so
   the policy declined it for an objective it ranks above speed. Without one, a loop's extra
   control instruction is the price of the fastest program there is, and not worth saying. *)
let check ~(mode : mode) ~(policy : Metrics.policy) ~(expects : (expectation * tag option) list)
    ~(faster : bool) (m : Metrics.t) (body : meta eexpr) : warning list =
  let where = Ast.locations body in
  let at node = Option.bind node (fun t -> List.assoc_opt t where) in
  let wants o = match mode with All -> true | Nothing -> false | Policy -> List.mem o policy in
  let policy_text = String.concat " > " (List.map Metrics.string_of_objective policy) in
  let overhead =
    if not (wants Metrics.Speed) || not faster then [] else
    List.filter_map (fun (l : Metrics.loop_metrics) -> match minimum l, l.construct with
      | Some least, Some c when l.own.max > least ->
        let message =
          if c = "while" && least = 1 then
            sprintf "this while tests at the top: %d control instructions per iteration, where %d would do with the test after the body (rotation); the policy (%s) kept it"
              l.own.max least policy_text
          else sprintf "this %s spends %d control instructions per iteration; the least it needs is %d" c l.own.max least in
        Some { loc = at l.node; message }
      | (Some _ | None), (Some _ | None) -> None) m.loops in
  (* A loop that states its step, or that it covers the core, has said what it means. *)
  let stated node = List.exists (fun (x, t) -> t = node && match x with
    | XStep _ | XCoversCore -> true
    | XLength _ | XCycles _ | XOverhead _ | XBoot _ | XAlive _ | XDead _ | XCell _ -> false) expects in
  let steps = if mode = Nothing then [] else
    List.filter_map (fun (p : Metrics.prediction) -> match p with
      | Step s when not s.full && not (stated s.node) ->
        Some { loc = at s.node;
               message = sprintf "a pointer here advances %d cells per iteration and visits only %d of %d cells; write (expect (step %d)) if that is meant"
                   s.k s.period m.coresize s.k }
      | Step _ | Counter _ -> None) m.predictions in
  (* Dead code, grouped by the node that emitted it. A jump the static view cannot follow may reach
     it, so nothing is said when there is one. *)
  let dead =
    if not (wants Metrics.Size || wants Metrics.Stealth) || m.dynamic_jumps > 0 then [] else
    let groups = List.fold_left (fun acc (pos, node) -> match acc with
      | (last, n, node') :: rest when node' = node && last = pos - 1 -> (pos, n + 1, node) :: rest
      | _ -> (pos, 1, node) :: acc) [] m.dead in
    List.rev_map (fun (_, n, node) ->
      { loc = at node; message = sprintf "%d cell%s never executed and holding no data (dead code)" n (if n = 1 then "" else "s") }) groups in
  overhead @ steps @ dead
