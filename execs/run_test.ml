open Cored.Ast
open Cored.Parse
open Cored.Compile
open Cored.Red
open Alcotest
open Bbctester.Type
open Bbctester.Main
open Bbctester.Runtime
open Bbctester.Testeable


let arg : arg testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_arg e)) (=)


(* Tests for our [parse] function *)
let test_parse_num () =
  check arg "same num" (parse_arg (`Atom "5")) (ANum 5)


(* Goldens: the SRC and EXPECTED sections of a .bbc file *)
let read_file (path : string) : string =
  In_channel.with_open_bin path In_channel.input_all

let between (text : string) (start : string) (stop : string) : string =
  let find sub from =
    let n = String.length sub in
    let rec go i = if i + n > String.length text then None
      else if String.sub text i n = sub then Some i else go (i + 1) in
    go from in
  match find start 0 with
  | None -> failwith ("no " ^ start)
  | Some i ->
    let b = i + String.length start in
    (match find stop b with Some j -> String.sub text b (j - b) | None -> String.sub text b (String.length text - b))

let golden_src (path : string) : string = between (read_file path) "SRC:\n" "\nEXPECTED:"
let golden_expected (path : string) : string = between (read_file path) "EXPECTED:\n" "\nEND"
let expr_of (path : string) : expr = parse_exp (sexp_from_string (golden_src path))
let example (name : string) : string = "bbctests/examples/" ^ name ^ ".bbc"

let golden_files () : string list =
  List.concat_map (fun dir ->
    Sys.readdir dir |> Array.to_list |> List.sort compare
    |> List.filter (fun f -> Filename.check_suffix f ".bbc")
    |> List.map (Filename.concat dir))
    ["bbctests/examples"; "bbctests/known-bugs"]


(* Tests for annotated emission *)
let rec while_tag (e : tag eexpr) : tag option =
  match e with
  | EFlow1 (While, _, _, t) -> Some t
  | EFlow1 (_, _, b, _) | ELet (_, _, b, _) -> while_tag b
  | EFlow2 (_, _, b1, b2, _) -> (match while_tag b1 with Some t -> Some t | None -> while_tag b2)
  | ESeq (es, _) -> List.find_map while_tag es
  | EComment _ | ELabel _ | EPrim2 _ | EExpect _ -> None

let is_mov (e : emitted) : bool =
  match e.instr with INSTR (IMOV, _, _, _) -> true | INSTR _ | ICOM _ | ILAB _ -> false

let place : place testable =
  testable (fun f p -> Format.pp_print_string f (match p with PA -> "PA" | PB -> "PB")) (=)

let test_emit_prog8_back_jump_is_while () =
  let e = expr_of (example "prog8") in
  let back = List.find (fun (x : emitted) -> match x.instr with
      | INSTR (IJMP, _, RLab (_, l), _) -> String.starts_with ~prefix:"WHI" l
      | INSTR _ | ICOM _ | ILAB _ -> false) (compile_body e) in
  check Alcotest.(option string) "construct" (Some "while") back.construct ;
  check Alcotest.(option int) "origin" (while_tag (tag_expr e)) back.origin

let test_emit_prog8_mov_is_user () =
  let mov = List.find is_mov (compile_body (expr_of (example "prog8"))) in
  check Alcotest.(option string) "construct" None mov.construct

let test_emit_prog1_stores () =
  let mov = List.find is_mov (compile_body (expr_of (example "prog1"))) in
  check Alcotest.(list (pair string place)) "stores" [("x", PB)] mov.stores

let test_emit_text_unchanged () =
  List.iter (fun path ->
    let body = compile_body (expr_of path) in
    let text = prelude ^ pp_instrs (List.map (fun (x : emitted) -> x.instr) body) ^ pp_instrs epilogue in
    check Alcotest.string path (String.trim (golden_expected path)) (String.trim text))
    (golden_files ())


(* Tests for the positioned IR *)
module L = Cored.Layout

let edge : L.edge testable =
  testable (fun f e -> Format.pp_print_string f (match e with
    | L.Next i -> Printf.sprintf "Next %d" i | L.Jump i -> Printf.sprintf "Jump %d" i
    | L.Skip i -> Printf.sprintf "Skip %d" i | L.Dynamic -> "Dynamic")) (=)

let field : L.field testable =
  testable (fun f x -> Format.pp_print_string f (match x with L.FA -> "FA" | L.FB -> "FB")) (=)

let diagnostic : L.diagnostic testable =
  testable (fun f d -> Format.pp_print_string f (match d with
    | L.Undefined_label l -> "Undefined_label " ^ l
    | L.Duplicate_label (l, a, b) -> Printf.sprintf "Duplicate_label (%s, %d, %d)" l a b
    | L.Long_line i -> Printf.sprintf "Long_line %d" i)) (=)

let layout_of_src (src : string) : L.program = L.of_expr (parse_exp (sexp_from_string src))
let layout_of (path : string) : L.program = L.of_expr (expr_of path)

let test_layout_prog8_cells () =
  let p = layout_of (example "prog8") in
  check Alcotest.int "cells" 7 (Array.length p.cells) ;
  check Alcotest.(list string) "labels" ["LET1"; "LET2"] p.cells.(1).labels ;
  check Alcotest.(list (pair string field)) "vars" [("x", L.FA); ("y", L.FB)] p.cells.(1).vars ;
  check Alcotest.bool "epilogue" true (p.cells.(6).role = L.Epilogue)

let test_layout_prog8_succ () =
  let p = layout_of (example "prog8") in
  check Alcotest.(list edge) "SLT" [L.Next 3; L.Skip 4] p.succ.(2) ;
  check Alcotest.(list edge) "JMP to end" [L.Jump 6] p.succ.(3) ;
  check Alcotest.(list edge) "JMP back" [L.Jump 2] p.succ.(5) ;
  check Alcotest.(list edge) "epilogue" [] p.succ.(6)

let test_layout_prog8_loop () =
  let p = layout_of (example "prog8") in
  check Alcotest.int "one loop" 1 (List.length p.loops) ;
  let l = List.hd p.loops in
  check Alcotest.int "header" 2 l.header ;
  check Alcotest.(list int) "body" [2; 4; 5] l.body ;
  check Alcotest.(list (pair int int)) "back edges" [(5, 2)] l.back_edges

let test_layout_offsets_normalised () =
  let p = layout_of (example "prog1") in
  check Alcotest.int "value" 7998 p.cells.(2).a.value ;
  check Alcotest.(option string) "label" (Some "LET1") p.cells.(2).a.label

let test_layout_prog4_dynamic () =
  let p = layout_of (example "prog4") in
  let jumps = List.filter (fun i -> p.cells.(i).op = IJMP) (List.init (Array.length p.cells) Fun.id) in
  check Alcotest.int "four jumps" 4 (List.length jumps) ;
  List.iter (fun i -> check Alcotest.(list edge) (string_of_int i) [L.Dynamic] p.succ.(i)) jumps

let test_layout_undefined_label () =
  let p = layout_of_src "(JMP nowhere)" in
  check Alcotest.(list diagnostic) "diagnostics" [L.Undefined_label "nowhere"] p.diagnostics ;
  check Alcotest.(list edge) "jump" [L.Dynamic] p.succ.(0)

let test_layout_long_line () =
  let l = String.make 250 'a' in
  let p = layout_of_src (Printf.sprintf "(seq (label %s) (JMP %s))" l l) in
  check Alcotest.bool "Long_line 0" true (List.mem (L.Long_line 0) p.diagnostics)

let test_layout_duplicate_label () =
  let p = L.of_expr (expr_of "bbctests/known-bugs/label_collision.bbc") in
  check Alcotest.(list diagnostic) "diagnostics" [L.Duplicate_label ("LET1", 1, 2)] p.diagnostics


(* Tests for the metrics *)
module M = Cored.Metrics

let range : M.range testable =
  testable (fun f (r : M.range) -> Format.fprintf f "{%d;%d}" r.min r.max) (=)

let r (a : int) (b : int) : M.range = { M.min = a; max = b }

let contains (s : string) (sub : string) : bool =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  go 0

let metrics_of (path : string) : M.t = M.measure (layout_of path)

let test_metrics_prog1 () =
  let m = metrics_of (example "prog1") in
  check Alcotest.(list int) "length code epilogue data nonzero nonblank"
    [4; 3; 1; 0; 3; 4] [m.length; m.code; m.epilogue; m.data; m.nonzero; m.nonblank] ;
  check Alcotest.(option range) "boot" (Some (r 0 0)) m.boot ;
  check Alcotest.int "one loop" 1 (List.length m.loops) ;
  let l = List.hd m.loops in
  check range "cycles" (r 3 3) l.cycles ;
  check range "overhead" (r 0 0) l.overhead ;
  check Alcotest.(option int) "exit" None l.exit

let test_metrics_prog7 () =
  let m = metrics_of (example "prog7") in
  check Alcotest.(option range) "boot" (Some (r 1 1)) m.boot ;
  let l = List.hd m.loops in
  check Alcotest.(list int) "cells" [2; 3] l.loop.body ;
  check range "cycles" (r 2 2) l.cycles ;
  check range "overhead" (r 1 1) l.overhead ;
  check Alcotest.(option string) "construct" (Some "do-while") l.construct

let test_metrics_prog8 () =
  let m = metrics_of (example "prog8") in
  check Alcotest.(option range) "boot" (Some (r 1 1)) m.boot ;
  let l = List.hd m.loops in
  check range "cycles" (r 3 3) l.cycles ;
  check range "overhead" (r 2 2) l.overhead ;
  check Alcotest.(option int) "exit" (Some 2) l.exit

let test_metrics_prog0_no_loops () =
  let m = metrics_of (example "prog0") in
  check Alcotest.int "loops" 0 (List.length m.loops) ;
  check Alcotest.(option range) "boot" None m.boot ;
  check Alcotest.bool "loops: none" true (contains (M.to_text ~maxlength:100 m) "loops: none")

let test_metrics_prog4_dynamic () =
  check Alcotest.int "dynamic jumps" 4 (metrics_of (example "prog4")).dynamic_jumps

let test_metrics_div_by_zero () =
  let m = M.measure (layout_of_src "(seq (DIV 0 (Dir 1)) (DAT 1 1))") in
  check Alcotest.(list int) "cells" [0] m.div_by_zero ;
  check Alcotest.bool "report" true (contains (M.to_text ~maxlength:100 m) "kills: cell 0 divides by 0")

let test_metrics_json_keys () =
  let j = M.to_json (metrics_of (example "prog1")) in
  check Alcotest.bool "length" true (contains j "\"length\":4") ;
  check Alcotest.bool "cycles" true (contains j "\"cycles\":{\"min\":3,\"max\":3}")


(* Tests for the predictions *)
let steps (m : M.t) : (int * int * int * bool) list =
  List.filter_map (fun p -> match p with
    | M.Step s -> Some (s.k, s.period, s.cover_cycles, s.full)
    | M.Counter _ -> None) m.predictions

let counters (m : M.t) : (int * int * int option) list =
  List.filter_map (fun p -> match p with
    | M.Counter c -> Some (c.n, c.loop_cycles, c.dies_after)
    | M.Step _ -> None) m.predictions

let step_t = Alcotest.(list (pair (pair int int) (pair int bool)))
let as_pairs = List.map (fun (k, p, c, f) -> ((k, p), (c, f)))

let test_predict_prog1_step () =
  check step_t "step" (as_pairs [(4, 2000, 6000, false)]) (as_pairs (steps (metrics_of (example "prog1"))))

let test_predict_prog7_counter () =
  check Alcotest.(list (triple int int (option int))) "counter" [(100, 200, Some 202)]
    (counters (metrics_of (example "prog7")))

let test_predict_prog7_step () =
  check step_t "step" (as_pairs [(1, 8000, 16000, true)]) (as_pairs (steps (metrics_of (example "prog7"))))

let test_predict_counter_zero () =
  let m = M.measure (layout_of_src "(do-while (DN 0) (NOP))") in
  check Alcotest.(list int) "n" [8000] (List.map (fun (n, _, _) -> n) (counters m))

let test_predict_matches_behaviour () =
  let spec = read_file "behtests/prog7_dowhile_dn.beh" in
  let dead = List.find_map (fun l -> Scanf.sscanf_opt l "dead %d" Fun.id) (String.split_on_char '\n' spec) in
  check Alcotest.(list (option int)) "dies_after = dead N" [dead]
    (List.map (fun (_, _, d) -> d) (counters (metrics_of (example "prog7"))))

let test_predict_partial_cover () =
  let m = M.measure (layout_of_src
    "(let (p 0) (seq (JMP (Dir 2)) (DAT (store p) 0) (repeat (seq (ADD 2 p) (MOV 0 (Ind p))))))") in
  check step_t "step" (as_pairs [(2, 4000, 12000, false)]) (as_pairs (steps m))


(* Tests for the policy and the program header *)
let objective : M.objective testable =
  testable (fun f o -> Format.pp_print_string f (match o with
    | M.Speed -> "speed" | M.Size -> "size" | M.Stealth -> "stealth" | M.Boot -> "boot")) (=)

let expectation : expectation testable =
  testable (fun f _ -> Format.pp_print_string f "<expectation>") (=)

let test_policy_default () =
  check Alcotest.(list objective) "default" [M.Speed; M.Size] M.default_policy

let test_policy_compare_speed_first () =
  let base = metrics_of (example "prog1") in
  let mk cycles length =
    { base with M.length; loops = [{ (List.hd base.loops) with cycles = r cycles cycles }] } in
  let fast_long = mk 3 8 and slow_short = mk 4 4 in
  check Alcotest.bool "speed first" true (M.compare M.default_policy fast_long slow_short < 0) ;
  check Alcotest.bool "size first" true (M.compare [M.Size; M.Speed] fast_long slow_short > 0)

let test_parse_source_plain () =
  let src = parse_source (sexp_from_string (golden_src (example "prog1"))) in
  check Alcotest.(option (list string)) "optimize" None src.optimize ;
  check Alcotest.(list expectation) "expects" [] src.expects

let test_parse_source_header () =
  let src = parse_source (sexp_from_string "(program (optimize size) (expect (length <= 8)) (MOV 0 1))") in
  check Alcotest.(option (list string)) "optimize" (Some ["size"]) src.optimize ;
  check Alcotest.(list expectation) "expects" [XLength (Le, 8)] src.expects

let test_objective_unknown () =
  check Alcotest.(option objective) "fast" None (M.objective_of_string "fast")


(* Tests for the expectations *)
module X = Cored.Expect

let outcome : X.outcome option testable =
  testable (fun f o -> Format.pp_print_string f (match o with
    | None -> "None" | Some X.Pass -> "Pass" | Some (X.Fail m) -> "Fail " ^ m)) (=)

let test_expect_length_pass () =
  check outcome "pass" (Some X.Pass) (X.check (metrics_of (example "prog1")) (XLength (Le, 4), None))

let test_expect_length_fail () =
  check outcome "fail" (Some (X.Fail "expect length <= 3: the warrior is 4 cells"))
    (X.check (metrics_of (example "prog1")) (XLength (Le, 3), None))

let test_expect_cycles_in_loop_fail () =
  let src = golden_src (example "prog8") in
  let mov = "(MOV I (Dir x) (Dec x))" in
  let src = between src "" mov ^ mov ^ " (expect (cycles <= 2))" ^ between src mov "\000" in
  let e = parse_exp (sexp_from_string src) in
  let tagged = tag_expr e in
  let t = Option.get (while_tag tagged) in
  let m = M.measure (L.of_expr e) in
  check Alcotest.int "collected" 1 (List.length (X.collect tagged)) ;
  check outcome "fail"
    (Some (X.Fail (Printf.sprintf "expect cycles <= 2: the while of node %d executes 3 per iteration" t)))
    (X.check m (List.hd (X.collect tagged)))

let test_expect_no_enclosing_loop () =
  check outcome "fail" (Some (X.Fail "expect cycles <= 3: no loop encloses this expectation"))
    (X.check (metrics_of (example "prog8")) (XCycles (Le, 3), Some 999))

let test_expect_global_no_loop () =
  check outcome "fail" (Some (X.Fail "expect cycles <= 3: the program has no loop"))
    (X.check (metrics_of (example "prog0")) (XCycles (Le, 3), None))

let test_expect_step_prog1 () =
  check outcome "pass" (Some X.Pass) (X.check (metrics_of (example "prog1")) (XStep 4, None))

let test_expect_to_beh () =
  check Alcotest.string "spec"
    "# written by run_compile.exe --emit-beh\nredcode: prog7.red\nalive 201\ndead 202\n"
    (X.to_beh ~redcode:"prog7.red" [XAlive 201; XDead 202; XLength (Le, 5)])


(* Tests from the final review *)
let test_review_nested_loops_split () =
  let e = parse_exp (sexp_from_string "(do-while (DN 5) (seq (do-while (DN 3) (NOP)) (expect (cycles <= 100))))") in
  let m = M.measure (L.of_expr e) in
  check Alcotest.int "two loops" 2 (List.length m.loops) ;
  check Alcotest.(list int) "both counters" [3; 5]
    (List.sort compare (List.map (fun (n, _, _) -> n) (counters m))) ;
  check outcome "outer expectation" (Some X.Pass) (X.check m (List.hd (X.collect (tag_expr e)))) ;
  check Alcotest.(list (option int)) "no dies_after around an inner loop" [None; None]
    (List.map (fun (_, _, d) -> d) (counters m)) ;
  check Alcotest.bool "report says so" true (contains (M.to_text ~maxlength:100 m) "inner loops counted once")

let test_review_many_branches_fast () =
  let ifs = String.concat " " (List.init 21 (fun _ -> "(if (JZ x) (NOP))")) in
  let src = Printf.sprintf "(let (x 0) (seq (JMP (Dir 2)) (DAT 0 (store x)) (repeat (seq %s))))" ifs in
  let t0 = Sys.time () in
  let m = M.measure (layout_of_src src) in
  check Alcotest.bool "measured in under a second" true (Sys.time () -. t0 < 1.0) ;
  check range "cycles" (r 22 43) (List.hd m.loops).cycles

let test_review_step_b_immediate () =
  let m = M.measure (layout_of_src
    "(let (p 0) (seq (JMP (Dir 2)) (DAT 0 (store p)) (repeat (seq (ADD B 4 p) (MOV 0 (Ind p))))))") in
  check step_t "step is the ADD's own B-number" (as_pairs [(7999, 8000, 24000, true)]) (as_pairs (steps m))

let test_review_dies_after_second_loop () =
  let m = M.measure (layout_of_src "(seq (do-while (DN 10) (NOP)) (do-while (DN 5) (NOP)))") in
  check Alcotest.(list (option int)) "no dies_after past the first loop" [None; None]
    (List.map (fun (_, _, d) -> d) (counters m))

let test_review_covers_core_bounded () =
  check outcome "prog7: counter ends first"
    (Some (X.Fail "expect covers-core: the loop ends after 100 iterations, visiting 100 of 8000 cells"))
    (X.check (metrics_of (example "prog7")) (XCoversCore, None)) ;
  check outcome "prog8: loop can exit"
    (Some (X.Fail "expect covers-core: the loop can exit before visiting every cell"))
    (X.check (metrics_of (example "prog8")) (XCoversCore, None)) ;
  check outcome "prog6: endless, step 1" (Some X.Pass) (X.check (metrics_of (example "prog6")) (XCoversCore, None)) ;
  check Alcotest.bool "report" true
    (contains (M.to_text ~maxlength:100 (metrics_of (example "prog7"))) "visits 100 cells before the counter ends")

let test_review_diagnostics_reported () =
  let m = M.measure (layout_of_src "(seq (JMP nowhere) (DAT 0 0))") in
  check Alcotest.bool "text" true (contains (M.to_text ~maxlength:100 m) "undefined label `nowhere`") ;
  check Alcotest.bool "json" true (contains (M.to_json m) "\"diagnostics\":[\"undefined label")


(* Tests for the deferred minor findings *)
let test_minor_expect_keeps_labels () =
  let src = golden_src (example "prog8") in
  let w = "(while" in
  let with_expect = between src "" w ^ "(expect (cycles <= 9)) " ^ w ^ between src w "\000" in
  check Alcotest.string "same redcode"
    (compile_prog (expr_of (example "prog8"))) (compile_prog (parse_exp (sexp_from_string with_expect)))


let test_minor_div_by_zero_b () =
  let m = M.measure (layout_of_src "(seq (DIV F 5 (Dir 0)) (DAT 1 1))") in
  check Alcotest.(list int) "DIV.F #5, $0 divides by its own B-number, 0" [0] m.div_by_zero


let test_minor_probe_count_positive () =
  check_raises "dead 0" (Cored.Parse.CTError "Not a valid expectation: (dead 0) (N must be at least 1)")
    (fun () -> ignore (parse_expectation (sexp_from_string "(dead 0)")))

let test_minor_optimize_needs_objective () =
  check_raises "(optimize)" (Cored.Parse.CTError "an (optimize ...) needs at least one objective")
    (fun () -> ignore (parse_source (sexp_from_string "(program (optimize) (MOV 0 1))")))


let test_minor_unreachable_qualified () =
  check Alcotest.bool "prog4" true
    (contains (M.to_text ~maxlength:100 (metrics_of (example "prog4"))) "unreachable 2 (ignoring 4 dynamic jumps)")


let test_minor_json_string () =
  check Alcotest.string "escapes" "\"a\\\"b\\n\\u0001\xc3\xa9\"" (M.json_string "a\"b\n\001\xc3\xa9")

let test_minor_json_policy_coresize () =
  let j = M.to_json ~policy:[M.Speed; M.Size] (metrics_of (example "prog1")) in
  check Alcotest.bool "coresize" true (contains j "\"coresize\":8000") ;
  check Alcotest.bool "policy" true (contains j "\"policy\":[\"speed\",\"size\"]")


(* OCaml tests: extend with your own tests *)
let ocaml_tests = [
  "parse", [
    test_case "A number" `Quick test_parse_num ;
  ] ;
  "emit", [
    test_case "prog8: the while's back jump" `Quick test_emit_prog8_back_jump_is_while ;
    test_case "prog8: a user MOV" `Quick test_emit_prog8_mov_is_user ;
    test_case "prog1: stores" `Quick test_emit_prog1_stores ;
    test_case "every golden's text is unchanged" `Quick test_emit_text_unchanged ;
  ] ;
  "layout", [
    test_case "prog8: cells" `Quick test_layout_prog8_cells ;
    test_case "prog8: successors" `Quick test_layout_prog8_succ ;
    test_case "prog8: loop" `Quick test_layout_prog8_loop ;
    test_case "prog1: offsets normalised" `Quick test_layout_offsets_normalised ;
    test_case "prog4: dynamic jumps" `Quick test_layout_prog4_dynamic ;
    test_case "undefined label" `Quick test_layout_undefined_label ;
    test_case "long line" `Quick test_layout_long_line ;
    test_case "duplicate label" `Quick test_layout_duplicate_label ;
  ] ;
  "metrics", [
    test_case "prog1" `Quick test_metrics_prog1 ;
    test_case "prog7" `Quick test_metrics_prog7 ;
    test_case "prog8" `Quick test_metrics_prog8 ;
    test_case "prog0: no loops" `Quick test_metrics_prog0_no_loops ;
    test_case "prog4: dynamic jumps" `Quick test_metrics_prog4_dynamic ;
    test_case "division by zero" `Quick test_metrics_div_by_zero ;
    test_case "json keys" `Quick test_metrics_json_keys ;
    test_case "prog1: fixed-step pointer" `Quick test_predict_prog1_step ;
    test_case "prog7: counter" `Quick test_predict_prog7_counter ;
    test_case "prog7: postincrement pointer" `Quick test_predict_prog7_step ;
    test_case "counter from 0" `Quick test_predict_counter_zero ;
    test_case "prog7: prediction = behaviour spec" `Quick test_predict_matches_behaviour ;
    test_case "partial cover" `Quick test_predict_partial_cover ;
  ] ;
  "policy", [
    test_case "default policy" `Quick test_policy_default ;
    test_case "compare: speed first" `Quick test_policy_compare_speed_first ;
    test_case "plain source" `Quick test_parse_source_plain ;
    test_case "program header" `Quick test_parse_source_header ;
    test_case "unknown objective" `Quick test_objective_unknown ;
  ] ;
  "expect", [
    test_case "length: pass" `Quick test_expect_length_pass ;
    test_case "length: fail" `Quick test_expect_length_fail ;
    test_case "cycles inside a while: fail" `Quick test_expect_cycles_in_loop_fail ;
    test_case "no enclosing loop" `Quick test_expect_no_enclosing_loop ;
    test_case "global, no loop" `Quick test_expect_global_no_loop ;
    test_case "prog1: step" `Quick test_expect_step_prog1 ;
    test_case "behaviour spec text" `Quick test_expect_to_beh ;
  ] ;
  "review", [
    test_case "nested loops sharing a header are split" `Quick test_review_nested_loops_split ;
    test_case "21 branches measured fast" `Quick test_review_many_branches_fast ;
    test_case "ADD.B with an immediate A steps by its B-number" `Quick test_review_step_b_immediate ;
    test_case "dies_after only for the first loop" `Quick test_review_dies_after_second_loop ;
    test_case "covers-core respects loop bounds" `Quick test_review_covers_core_bounded ;
    test_case "diagnostics are reported" `Quick test_review_diagnostics_reported ;
  ] ;
  "minor", [
    test_case "an expect statement keeps generated labels" `Quick test_minor_expect_keeps_labels ;
    test_case "DIV.F by a zero B-number" `Quick test_minor_div_by_zero_b ;
    test_case "execution probes need N >= 1" `Quick test_minor_probe_count_positive ;
    test_case "(optimize) needs an objective" `Quick test_minor_optimize_needs_objective ;
    test_case "unreachable says it ignores dynamic jumps" `Quick test_minor_unreachable_qualified ;
    test_case "JSON string escaping" `Quick test_minor_json_string ;
    test_case "JSON carries policy and coresize" `Quick test_minor_json_policy_coresize ;
  ] ;
  "interp", [

  ] ;
  "errors", [

  ]
]

(* Entry point of tester *)
let () =
  
  let compiler : compiler =
    SCompiler ( fun _ s -> (compile_prog (parse_exp (sexp_from_string s))) ) in
  
  let bbc_tests =
    let name : string = "compare" in
    tests_from_dir ~name ~compiler "bbctests" in
  
  let verify_tests =
    let name : string = "execute" in
    let runtime: runtime = unix_command "pmars/pmars -A -@ pmars/config/94b.opt %s" in
    let testeable : testeable = compare_status in
    tests_from_dir ~name ~compiler ~runtime ~testeable "bbctests" in
  
  run "Tests corewars-compiler" (ocaml_tests @ bbc_tests @ verify_tests)
