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
let rec while_meta (e : meta eexpr) : meta option =
  match e with
  | EFlow1 (While, _, _, m) -> Some m
  | EFlow1 (_, _, b, _) | ELet (_, _, b, _) -> while_meta b
  | EFlow2 (_, _, b1, b2, _) -> (match while_meta b1 with Some m -> Some m | None -> while_meta b2)
  | ESeq (es, _) -> List.find_map while_meta es
  | EComment _ | ELabel _ | EPrim2 _ | EExpect _ -> None

let while_tag (e : meta eexpr) : tag option = Option.map (fun (m : meta) -> m.tag) (while_meta e)

let is_mov (e : emitted) : bool =
  match e.instr with INSTR (IMOV, _, _, _) -> true | INSTR _ | ICOM _ | ILAB _ -> false

let place : place testable =
  testable (fun f p -> Format.pp_print_string f (match p with PA -> "PA" | PB -> "PB")) (=)

let test_emit_prog8_back_jump_is_while () =
  let e = expr_of (example "prog8") in
  let back = List.find (fun (x : emitted) -> match x.instr with
      | INSTR (IJMP, _, RLab (_, l), _) -> String.starts_with ~prefix:"_WHI" l
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
  check Alcotest.(list string) "labels" ["_LET1"; "_LET2"] p.cells.(1).labels ;
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
  check Alcotest.(option string) "label" (Some "_LET1") p.cells.(2).a.label

let test_layout_prog4_dynamic () =
  let p = layout_of (example "prog4") in
  let jumps = List.filter (fun i -> p.cells.(i).op = IJMP) (List.init (Array.length p.cells) Fun.id) in
  check Alcotest.int "four jumps" 4 (List.length jumps) ;
  List.iter (fun i -> check Alcotest.(list edge) (string_of_int i) [L.Dynamic] p.succ.(i)) jumps

let test_layout_undefined_label () =
  let p = layout_of_src "(JMP nowhere)" in
  check Alcotest.(list diagnostic) "diagnostics" [L.Undefined_label "nowhere"] p.diagnostics ;
  check Alcotest.(list edge) "jump" [L.Dynamic] p.succ.(0) ;
  check Alcotest.(option string) "operand keeps its label" (Some "nowhere") p.cells.(0).a.label ;
  check Alcotest.int "operand value" 0 p.cells.(0).a.value

let test_layout_long_line () =
  let l = String.make 250 'a' in
  let p = layout_of_src (Printf.sprintf "(seq (label %s) (JMP %s))" l l) in
  check Alcotest.bool "Long_line 0" true (List.mem (L.Long_line 0) p.diagnostics)

let test_layout_duplicate_label () =
  let p = layout_of_src "(seq (label a) (JMP 0) (label a) (JMP 0))" in
  check Alcotest.(list diagnostic) "diagnostics" [L.Duplicate_label ("a", 0, 1)] p.diagnostics


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
  (* the last if's jump is threaded to the loop head: 21 cycles when every if skips *)
  check range "cycles" (r 21 43) (List.hd m.loops).cycles

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
  check_raises "dead 0" (Cored.Ast.Error (Some { line = 1; col = 1 }, "Not a valid expectation: (dead 0) (N must be at least 1)"))
    (fun () -> ignore (parse_expectation (sexp_from_string "(dead 0)")))

let test_minor_optimize_needs_objective () =
  check_raises "(optimize)" (Cored.Ast.Error (Some { line = 1; col = 10 }, "an (optimize ...) needs at least one objective"))
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


(* Tests for the command line, through the driver *)
module D = Cored.Driver

let drive (files : (string * string) list) (args : string list) : D.output =
  D.run ~read:(fun f -> List.assoc_opt f files) args

let prog1_src = golden_src (example "prog1")
let prog1_red = compile_prog (expr_of (example "prog1")) ^ "\n"

let test_driver_plain () =
  let o = drive [("p.src", prog1_src)] ["p.src"] in
  check Alcotest.(triple string string int) "out, err, code" (prog1_red, "", 0) (o.out, o.err, o.code)

let test_driver_report_on_stderr () =
  let o = drive [("p.src", prog1_src)] ["--report"; "p.src"] in
  check Alcotest.string "redcode unchanged" prog1_red o.out ;
  check Alcotest.bool "report" true (contains o.err "policy: speed > size")

let test_driver_unknown_objective () =
  let o = drive [("p.src", prog1_src)] ["--optimize"; "fast"; "p.src"] in
  check Alcotest.(pair string int) "err, code" ("unknown objective `fast`: one of speed, size, stealth, boot\n", 1) (o.err, o.code)

let test_driver_expectation_fails () =
  let src = "(program (expect (length <= 3)) " ^ prog1_src ^ ")" in
  let o = drive [("p.src", src)] ["p.src"] in
  check Alcotest.(triple string string int) "error" ("", "expect length <= 3: the warrior is 4 cells\n", 1) (o.out, o.err, o.code) ;
  let w = drive [("p.src", src)] ["--expect=warn"; "p.src"] in
  check Alcotest.(triple string string int) "warning"
    (prog1_red, "warning: expect length <= 3: the warrior is 4 cells\n", 0) (w.out, w.err, w.code)

let test_driver_compile_error_is_clean () =
  let o = drive [("p.src", "(program (expect (cycles < 3)) (MOV 0 1))")] ["p.src"] in
  check Alcotest.(pair string int) "err, code" ("p.src:1:18: error: Not a valid expectation: (cycles < 3)\n", 1) (o.err, o.code)

let test_driver_missing_file () =
  let o = drive [] ["nope.src"] in
  check Alcotest.(pair string int) "err, code" ("error: no such file: nope.src\n", 1) (o.err, o.code)

let prog7_expect = read_file "examples/prog7_expect.src"

let test_driver_emit_beh () =
  let o = drive [("p.src", prog7_expect)] ["--emit-beh"; "out.beh"; "p.src"] in
  check Alcotest.int "code" 0 o.code ;
  check Alcotest.(list string) "files" ["out.red"; "out.beh"] (List.map fst o.files) ;
  check Alcotest.string "spec" "# written by run_compile.exe --emit-beh\nredcode: out.red\nalive 201\ndead 202\n"
    (List.assoc "out.beh" o.files)

let test_driver_emit_beh_suffix () =
  let o = drive [("p.src", prog7_expect)] ["--emit-beh"; "out.red"; "p.src"] in
  check Alcotest.(triple string int int) "refused" ("--emit-beh FILE must end in .beh\n", 1, 0)
    (o.err, o.code, List.length o.files)

let test_driver_emit_beh_nothing () =
  let o = drive [("p.src", prog1_src)] ["--emit-beh"; "out.beh"; "p.src"] in
  check Alcotest.(pair string int) "refused"
    ("--emit-beh: the program has no (alive N), (dead N) or (cell ...) expectation to export\n", 1) (o.err, o.code)


(* Tests for phase 1: correctness before the output changes *)
let instrs_of (path : string) : instruction list =
  List.map (fun (x : emitted) -> x.instr) (compile_body (expr_of path))

let test_phase1_cond1_tests_the_variable_field () =
  let jmn = List.find_map (fun i -> match i with
    | INSTR (IJMN, md, _, _) -> Some md | INSTR _ | ICOM _ | ILAB _ -> None)
    (instrs_of "bbctests/examples/cond1_afield.bbc") in
  check Alcotest.bool "JMN.A on an A-field variable" true (jmn = Some RA)


let test_phase1_shadowing_keeps_outer_field () =
  let add = List.find_map (fun i -> match i with
    | INSTR (IADD, md, _, _) -> Some md | INSTR _ | ICOM _ | ILAB _ -> None)
    (instrs_of "bbctests/examples/let_shadowing.bbc") in
  check Alcotest.bool "ADD.A on the outer x" true (add = Some RA)


let test_phase1_initializer_resolved_where_bound () =
  let src = "(let (x 1) (seq (JMP (Dir 4)) (DAT (store x) 0) (let (y x) (let (x 2) (seq (DAT (store x) 0) (DAT 0 (store y)))))))" in
  let labels = List.filter_map (fun (e : emitted) -> match e.instr with
    | INSTR (IDAT, _, _, RLab (_, l)) -> Some l | INSTR _ | ICOM _ | ILAB _ -> None)
    (compile_body (parse_exp (sexp_from_string src))) in
  check Alcotest.(list string) "y refers to the outer x" ["_LET1"] labels


let opcodes (src : string) : opcode list =
  List.filter_map (fun (e : emitted) -> match e.instr with
    | INSTR (op, _, _, _) -> Some op | ICOM _ | ILAB _ -> None)
    (compile_body (parse_exp (sexp_from_string src)))

let opcode : opcode testable = testable (fun f o -> Format.pp_print_string f (pp_opcode o)) (=)

let test_phase1_dowhile_gt_layout () =
  check Alcotest.(list opcode) "SLT y, x; SNE #0, #1; JMP head"
    [IJMP; IDAT; ISUB; ISLT; ISNE; IJMP]
    (opcodes "(let (x 5) (let (y 3) (seq (JMP (Dir 2)) (DAT (store x) (store y)) (do-while (GT x y) (SUB 1 x)))))")


let test_phase1_long_line_is_an_error () =
  let l = String.make 250 'a' in
  let o = drive [("p.src", Printf.sprintf "(seq (label %s) (JMP %s))" l l)] ["p.src"] in
  check Alcotest.(pair string int) "err, code"
    ("p.src: error: redcode line 5 has 269 characters; pMARS hangs on lines of 256 or more\n", 1) (o.err, o.code)


let test_phase1_parse_error_located () =
  let o = drive [("p.src", "(seq (MOV 0 1)\n  (FOO 1))")] ["p.src"] in
  check Alcotest.(pair string int) "err, code" ("p.src:2:3: error: Not a valid unary expr: (FOO 1)\n", 1) (o.err, o.code)

let test_phase1_atom_error_located () =
  let o = drive [("p.src", "(MOV (foo 1) 0)")] ["p.src"] in
  check Alcotest.(pair string int) "err, code" ("p.src:1:7: error: Not a valid mode: foo\n", 1) (o.err, o.code)

let test_phase1_compile_error_located () =
  let o = drive [("p.src", "(let (x 1)\n  (MOV x 0))")] ["p.src"] in
  check Alcotest.(pair string int) "err, code"
    ("p.src:2:3: error: variable `x` is used but no (store x) places it\n", 1) (o.err, o.code)

let test_phase1_tags_unchanged_by_locations () =
  (* tag numbering decides every generated label: a located AST must number nodes as before *)
  check Alcotest.(option int) "prog8's while" (Some 9) (Option.map (fun (m : meta) -> m.tag) (while_meta (tag_expr (expr_of (example "prog8")))))


let error_of (src : string) : string = (drive [("p.src", src)] ["p.src"]).err

let test_phase1_reserved_prefix_rejected () =
  check Alcotest.string "label" "p.src:1:6: error: `_x`: names starting with `_` are reserved for the compiler\n"
    (error_of "(seq (label _x) (JMP 0))")

let test_phase1_pmars_keyword_label_rejected () =
  check Alcotest.string "label END" "p.src:1:6: error: `END` is a pMARS keyword and cannot be a label\n"
    (error_of "(seq (label END) (JMP 0))")

let test_phase1_double_store_rejected () =
  check Alcotest.string "two stores" "p.src:1:1: error: variable `x` is stored twice; a let variable lives in one cell\n"
    (error_of "(let (x 1) (seq (DAT (store x) 0) (DAT 0 (store x))))")


let test_phase1_generated_labels_prefixed () =
  let labels = List.filter_map (fun i -> match i with ILAB l -> Some l | ICOM _ | INSTR _ -> None)
      (instrs_of (example "prog8")) in
  check Alcotest.(list string) "prog8's labels" ["_LET1"; "_LET2"; "_WHI9"; "_WHF9"] labels


let modifier_of (src : string) : rmod option =
  List.find_map (fun (e : emitted) -> match e.instr with
    | INSTR (_, md, _, _) -> Some md | ICOM _ | ILAB _ -> None)
    (compile_body (parse_exp (sexp_from_string src)))

let rmod_t : rmod testable = testable (fun f m -> Format.pp_print_string f ("[" ^ pp_rmod m ^ "]")) (=)

let test_phase1_icws_default_modifiers () =
  List.iter (fun (src, md) -> check (Alcotest.option rmod_t) src (Some md) (modifier_of src))
    [ ("(ADD 1 1)", RAB);                  (* arithmetic, A immediate: .AB *)
      ("(ADD (Dir 1) 0)", RB);             (* arithmetic, only B immediate: .B *)
      ("(ADD (Dir 1) (Dir 2))", RF);       (* arithmetic, neither: .F *)
      ("(MOV (Dir 0) (Dir 1))", RI);       (* MOV, neither immediate: .I *)
      ("(MOV 0 (Dir 1))", RAB);            (* MOV, A immediate: .AB *)
      ("(SLT (Dir 1) (Dir 2))", RB);       (* SLT, A not immediate: .B *)
      ("(SEQ (Dir 1) (Dir 2))", RI);       (* SEQ, neither: .I *)
      ("(JMZ (Dir 2) (Dir 1))", RB) ]      (* jumps: .B *)


(* Tests from the phase-1 branch review *)
let jump_modifier (src : string) : rmod option =
  List.find_map (fun (e : emitted) -> match e.instr with
    | INSTR ((IJMZ | IJMN | IDJN), md, _, _) -> Some md | INSTR _ | ICOM _ | ILAB _ -> None)
    (compile_body (parse_exp (sexp_from_string src)))

let test_review1_unary_jump_uses_variable_field () =
  List.iter (fun (src, md) -> check (Alcotest.option rmod_t) src (Some md) (jump_modifier src))
    [ ("(let (x 2) (seq (DAT (store x) 0) (DJN (Dir -1) x)))", RA);   (* x in A: DJN.A *)
      ("(let (x 2) (seq (DAT 0 (store x)) (JMN (Dir -1) x)))", RB);   (* x in B: JMN.B *)
      ("(let (x 2) (seq (DAT (store x) 0) (JMZ (Dir -1) (# x))))", RB) ] (* #x is the B-number: .B *)


let test_review1_user_name_like_a_fresh_one () =
  let o = drive [("p.src", "(let (x 1) (seq (JMP (Dir 4)) (DAT (store x) 0) (let (x 2) (let (x#1 3) (seq (DAT (store x) 0) (DAT (store x#1) 0))))))")] ["p.src"] in
  check Alcotest.(pair string int) "compiles" ("", 0) (o.err, o.code)

let test_review1_messages_name_the_user's_variable () =
  check Alcotest.string "original name"
    "p.src:1:46: error: variable `x` is used but no (store x) places it\n"
    (error_of "(let (x 1) (seq (DAT (store x) 0) (let (x 2) (MOV 0 x))))")


let test_review1_constant_skip_known () =
  let l = List.hd (metrics_of (example "dowhile_gt_count")).loops in
  check range "cycles" (r 3 3) l.cycles ;
  check range "overhead" (r 2 2) l.overhead


let test_review1_store_in_condition_is_labelled () =
  let labels = List.filter_map (fun i -> match i with ILAB l -> Some l | ICOM _ | INSTR _ -> None)
      (List.map (fun (e : emitted) -> e.instr)
         (compile_body (parse_exp (sexp_from_string "(let (x 4) (seq (if (EQ (store x) 4) (NOP)) (ADD 1 x)))")))) in
  check Alcotest.bool "_LET1 is defined" true (List.mem "_LET1" labels)


let test_review1_store_in_gt_condition_field () =
  (* GT is emitted as SLT b, a: a (store x) on the left lands in the B-field *)
  let add = List.find_map (fun i -> match i with INSTR (IADD, md, _, _) -> Some md | INSTR _ | ICOM _ | ILAB _ -> None)
      (List.map (fun (e : emitted) -> e.instr)
         (compile_body (parse_exp (sexp_from_string "(let (x 4) (seq (if (GT (store x) 3) (NOP)) (ADD 1 x)))")))) in
  check (Alcotest.option rmod_t) "ADD.AB on x in the B-field" (Some RAB) add


(* Tests for phase 2: jump threading *)
let instrs_of_src (src : string) : instruction list =
  List.map (fun (e : emitted) -> e.instr) (compile_body (parse_exp (sexp_from_string src)))

let targets (op : opcode) (is : instruction list) : string list =
  List.filter_map (fun i -> match i with
    | INSTR (o, _, RLab (RDir, l), _) when o = op -> Some l
    | INSTR _ | ICOM _ | ILAB _ -> None) is

let first_label (prefix : string) (is : instruction list) : string =
  List.find (String.starts_with ~prefix)
    (List.filter_map (fun i -> match i with ILAB l -> Some l | INSTR _ | ICOM _ -> None) is)

let strings = Alcotest.(list string)

let test_phase2_if_jumps_to_loop_head () =
  (* the scanner archetype: the if's false branch reached the repeat's JMP through _IF9 *)
  check strings "JMZ" ["_REP4"] (targets IJMZ (instrs_of_src (golden_src "bbctests/archetypes/scanner.bbc")))

let test_phase2_chain_followed () =
  (* inner if -> _IF: JMP _IFF (then-branch end) -> _IFF: JMP _REP *)
  let is = instrs_of_src
      "(let (x 0) (let (y 0) (seq (repeat (if (JZ x) (if (JN y) (NOP)) (NOP))) (DAT (store x) (store y)))))" in
  let rep = first_label "_REP" is in
  check strings "JMZ" [rep] (targets IJMZ is) ;
  check strings "JMP" [rep; rep] (targets IJMP is)

let test_phase2_user_jump_untouched () =
  let is = instrs_of_src "(repeat (seq (JMZ foo (Dir 5)) (NOP) (label foo)))" in
  check strings "user JMZ" ["foo"] (targets IJMZ is)

let test_phase2_user_jmp_not_followed () =
  (* a JMP the user wrote may have its target rewritten at run time: never thread through it *)
  let is = instrs_of_src "(let (x 0) (seq (label top) (if (JN x) (NOP)) (JMP top) (DAT (store x) 0)))" in
  check strings "JMZ" [first_label "_IF" is] (targets IJMZ is)

let test_phase2_threaded_loop_is_one_loop () =
  (* the threaded JMZ and the repeat's JMP both close _REP4: one loop, 2 cycles on an empty cell *)
  let m = M.measure (layout_of_src (golden_src "bbctests/archetypes/scanner.bbc")) in
  check Alcotest.int "one loop" 1 (List.length m.loops) ;
  check range "cycles" (r 2 4) (List.hd m.loops).cycles

let test_phase2_self_loop_terminates () =
  let is = instrs_of_src "(repeat (seq))" in
  check strings "JMP" [first_label "_REP" is] (targets IJMP is)


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
  "driver", [
    test_case "plain compile" `Quick test_driver_plain ;
    test_case "--report on stderr" `Quick test_driver_report_on_stderr ;
    test_case "unknown objective" `Quick test_driver_unknown_objective ;
    test_case "failed expectation: error, or warning" `Quick test_driver_expectation_fails ;
    test_case "compile error exits cleanly" `Quick test_driver_compile_error_is_clean ;
    test_case "missing file" `Quick test_driver_missing_file ;
    test_case "--emit-beh" `Quick test_driver_emit_beh ;
    test_case "--emit-beh needs .beh" `Quick test_driver_emit_beh_suffix ;
    test_case "--emit-beh with nothing to export" `Quick test_driver_emit_beh_nothing ;
  ] ;
  "phase1", [
    test_case "a unary condition tests its variable's field" `Quick test_phase1_cond1_tests_the_variable_field ;
    test_case "an inner let does not move an outer variable" `Quick test_phase1_shadowing_keeps_outer_field ;
    test_case "an initializer is resolved where its let binds it" `Quick test_phase1_initializer_resolved_where_bound ;
    test_case "do-while GT: strict, one extra cell" `Quick test_phase1_dowhile_gt_layout ;
    test_case "a line of 256+ characters is an error" `Quick test_phase1_long_line_is_an_error ;
    test_case "a parse error says where" `Quick test_phase1_parse_error_located ;
    test_case "a compile error says where" `Quick test_phase1_compile_error_located ;
    test_case "an error on an atom says where" `Quick test_phase1_atom_error_located ;
    test_case "names starting with _ are reserved" `Quick test_phase1_reserved_prefix_rejected ;
    test_case "a pMARS keyword is not a label" `Quick test_phase1_pmars_keyword_label_rejected ;
    test_case "a variable is stored once" `Quick test_phase1_double_store_rejected ;
    test_case "generated labels start with _" `Quick test_phase1_generated_labels_prefixed ;
    test_case "no modifier given: the ICWS'94 default" `Quick test_phase1_icws_default_modifiers ;
    test_case "tags are numbered as before" `Quick test_phase1_tags_unchanged_by_locations ;
  ] ;
  "review1", [
    test_case "a user JMZ/JMN/DJN uses its variable's field" `Quick test_review1_unary_jump_uses_variable_field ;
    test_case "a user name like a fresh one" `Quick test_review1_user_name_like_a_fresh_one ;
    test_case "messages name the user's variable" `Quick test_review1_messages_name_the_user's_variable ;
    test_case "SNE #0, #1 always skips (metrics)" `Quick test_review1_constant_skip_known ;
    test_case "a store inside a condition defines its label" `Quick test_review1_store_in_condition_is_labelled ;
    test_case "a store on the left of GT is in the B-field" `Quick test_review1_store_in_gt_condition_field ;
  ] ;
  "phase2", [
    test_case "an if at a loop's end jumps to its head" `Quick test_phase2_if_jumps_to_loop_head ;
    test_case "a chain of jumps is followed" `Quick test_phase2_chain_followed ;
    test_case "a user's jump is not rewritten" `Quick test_phase2_user_jump_untouched ;
    test_case "a user's JMP is not threaded through" `Quick test_phase2_user_jmp_not_followed ;
    test_case "a JMP to itself terminates" `Quick test_phase2_self_loop_terminates ;
    test_case "a threaded jump closes the same loop" `Quick test_phase2_threaded_loop_is_one_loop ;
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
