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

(* A golden's source is golden.src; a file it includes is read from the repository, relative to its
   root (snippets/imp.src) *)
let golden_read (src : string) (f : string) : string option =
  if f = "golden.src" then Some src
  else (try Some (In_channel.with_open_bin f In_channel.input_all) with Sys_error _ -> None)

let golden_files () : string list =
  List.concat_map (fun dir ->
    Sys.readdir dir |> Array.to_list |> List.sort compare
    |> List.filter (fun f -> Filename.check_suffix f ".bbc")
    |> List.map (Filename.concat dir))
    ["bbctests/examples"; "bbctests/known-bugs"; "bbctests/snippets"]


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
    (* a golden may carry a header (constants, a hill): compiled as the CLI compiles it *)
    let text = (Cored.Driver.run ~read:(golden_read (golden_src path)) ["golden.src"]).out in
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
    (* the epilogue is DAT $0, $0, empty core: not counted as nonblank *)
    [4; 3; 1; 0; 3; 3] [m.length; m.code; m.epilogue; m.data; m.nonzero; m.nonblank] ;
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
  (* prog1's loop moves its pointer 4 cells a lap: the one warning it earns *)
  check Alcotest.(triple string string int) "out, err, code" (prog1_red, "p.src:5:5: warning: a pointer here advances 4 cells per iteration and visits only 2000 of 8000 cells; write (expect (step 4)) if that is meant\n", 0) (o.out, o.err, o.code)

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
    (prog1_red, "p.src:5:5: warning: a pointer here advances 4 cells per iteration and visits only 2000 of 8000 cells; write (expect (step 4)) if that is meant\nwarning: expect length <= 3: the warrior is 4 cells\n", 0) (w.out, w.err, w.code)

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

let test_driver_emit_beh_hill () =
  (* the spec names the hill, so behave.py runs it under that hill's settings *)
  let o = drive [("p.src", "(program (hill tiny) (expect (alive 50)) (JMP 0))")] ["--emit-beh"; "out.beh"; "p.src"] in
  check Alcotest.string "spec" "# written by run_compile.exe --emit-beh\nredcode: out.red\nhill: tiny\nalive 50\n"
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
    (* said at the JMP whose line it is *)
    ("p.src:1:265: error: redcode line 5 has 269 characters; pMARS hangs on lines of 256 or more\n", 1) (o.err, o.code)


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
  check Alcotest.string "two stores, at the second" "p.src:1:35: error: variable `x` is stored twice; a let variable lives in one cell\n"
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
  check range "cycles" (r 2 4) (List.hd m.loops).cycles ;
  check Alcotest.(option string) "closed by the repeat" (Some "repeat") (List.hd m.loops).construct

let modifiers (op : opcode) (src : string) : rmod list =
  List.filter_map (fun i -> match i with
    | INSTR (o, md, _, _) when o = op -> Some md
    | INSTR _ | ICOM _ | ILAB _ -> None) (instrs_of_src src)

let rmods = Alcotest.list rmod_t

let test_phase2_condition_modifier () =
  let ptrs c = Printf.sprintf "(let (a 100) (let (b 104) (seq (if %s (NOP)) (DAT (store a) (store b)))))" c in
  check rmods "NE I compares whole cells" [RI] (modifiers ISNE (ptrs "(NE I (Ind a) (Ind b))")) ;
  (* two pointers name two cells: compared whole *)
  check rmods "no modifier: whole cells" [RI] (modifiers ISNE (ptrs "(NE (Ind a) (Ind b))")) ;
  check rmods "JZ F tests both fields" [RF]
    (modifiers IJMN "(let (x 0) (seq (if (JZ F x) (NOP)) (DAT 0 (store x))))") ;
  check rmods "DN A in a do-while" [RA]
    (modifiers IDJN "(let (x 3) (seq (do-while (DN A x) (NOP)) (DAT 0 (store x))))") ;
  (* the always-skipping SNE of a post-condition LT keeps its own modifier *)
  let lt = "(let (x 0) (let (y 0) (seq (do-while (LT F x y) (NOP)) (DAT (store x) (store y)))))" in
  check rmods "LT F: SLT.F" [RF] (modifiers ISLT lt) ;
  check rmods "LT F: SNE.AB #0, #1" [RAB] (modifiers ISNE lt)

let test_phase2_condition_bad_modifier () =
  check Alcotest.string "error" "p.src:1:25: error: Not a valid imod: Q\n"
    (error_of "(let (x 0) (seq (if (JZ Q x) (NOP)) (DAT 0 (store x))))")

let test_phase2_labelled_dat_is_data () =
  let counts src = let m = M.measure (layout_of_src src) in (m.data, m.unreachable) in
  let pair = Alcotest.(pair int int) in
  (* core-clear: the pointer's cell and the bomb, (label bomb) (DAT 0 0), are both data *)
  check pair "clear" (2, 0) (counts "(let (p 4) (seq (repeat (MOV I bomb (Inc p))) (DAT 0 (store p)) (label bomb) (DAT 0 0)))") ;
  check pair "a dead MOV is unreachable" (0, 1) (counts "(seq (repeat (NOP)) (label x) (MOV 0 1))") ;
  check pair "an unlabelled dead DAT is unreachable" (0, 1) (counts "(seq (repeat (NOP)) (DAT 0 0))")

let test_phase2_self_loop_terminates () =
  let is = instrs_of_src "(repeat (seq))" in
  check strings "JMP" [first_label "_REP" is] (targets IJMP is)


(* Tests from the phase-2 branch review *)
let test_review2_while_at_loop_end () =
  (* the while's exit jump landed on the repeat's JMP, its only way in: threading it left that JMP
     dead and the repeat's loop closed by the while *)
  let src = "(let (x 3) (let (y 0) (seq (repeat (seq (expect (cycles <= 10)) (ADD 1 y) (while (JN x) (SUB 1 x)))) (DAT (store x) (store y)))))" in
  let m = M.measure (layout_of_src src) in
  check Alcotest.int "nothing dead" 0 m.unreachable ;
  check Alcotest.bool "the repeat's loop" true
    (List.exists (fun (l : M.loop_metrics) -> l.construct = Some "repeat") m.loops) ;
  check Alcotest.(pair string int) "its expectation is checked" ("", 0)
    (let o = drive [("p.src", src)] ["p.src"] in (o.err, o.code))

let test_review2_gt_explicit_modifier () =
  (* GT is emitted as SLT y, x: an AB reading of x and y is BA on the swapped operands *)
  let gt m = Printf.sprintf "(let (x 0) (let (y 0) (seq (if (GT %s x y) (NOP)) (DAT (store x) 0) (DAT 0 (store y)))))" m in
  check rmods "AB" [RBA] (modifiers ISLT (gt "AB")) ;
  check rmods "BA" [RAB] (modifiers ISLT (gt "BA")) ;
  check rmods "F" [RF] (modifiers ISLT (gt "F")) ;
  check rmods "do-while GT AB" [RBA]
    (modifiers ISLT "(let (x 0) (let (y 0) (seq (do-while (GT AB x y) (NOP)) (DAT (store x) (store y)))))")

let test_review2_user_label_stops_threading () =
  (* tail labels the repeat's JMP, which the program overwrites: never thread through it *)
  let is = instrs_of_src
      "(let (x 1) (seq (repeat (seq (MOV I stop tail) (if (JZ x) (NOP)) (label tail))) (DAT 0 (store x)) (label stop) (DAT 0 0)))" in
  check strings "JMN" [first_label "_IF" is] (targets IJMN is)

let test_review2_generated_label_is_not_a_name () =
  let m = M.measure (layout_of_src "(seq (repeat (NOP)) (if (JZ 0) (NOP)) (DAT 0 0))") in
  check Alcotest.(pair int int) "data, unreachable" (0, 3) (m.data, m.unreachable)

let test_review2_unary_arity_message () =
  check Alcotest.string "error" "p.src:1:5: error: Not a valid unary cond: (JZ F F x)\n"
    (error_of "(if (JZ F F x) (NOP))")


(* Tests for phase 3: the optimizer under the policy *)
module O = Cored.Optimize

let body_of (src : string) : expr = parse_exp (sexp_from_string src)

let chosen ?(policy = M.default_policy) (src : string) : instruction list =
  List.map (fun (e : emitted) -> e.instr) (O.compile_body policy (body_of src))

let forced (opts : options) (src : string) : instruction list =
  List.map (fun (e : emitted) -> e.instr) (compile_body ~opts (body_of src))

let opcodes (is : instruction list) : opcode list =
  List.filter_map (fun i -> match i with INSTR (o, _, _, _) -> Some o | ICOM _ | ILAB _ -> None) is

let ops = Alcotest.list (testable (fun f o -> Format.pp_print_string f (pp_opcode o)) (=))

let countdown = "(let (x 3) (seq (while (JN x) (SUB 1 x)) (DAT 0 (store x))))"

let test_phase3_unary_while_rotated () =
  let is = chosen countdown in
  check ops "layout" [IJMP; ISUB; IJMN; IDAT] (opcodes is) ;
  check strings "the test loops to the body" [first_label "_WHI" is] (targets IJMN is) ;
  check strings "the entry jumps to the test" [first_label "_WHC" is] (targets IJMP is)

let test_phase3_rotation_by_policy () =
  let loop policy = List.hd (M.measure (L.build (O.compile_body policy (body_of countdown)))).loops in
  check range "speed first: rotated" (r 2 2) (loop M.default_policy).cycles ;
  check range "boot first: not rotated" (r 3 3) (loop [M.Boot; M.Speed]).cycles

let test_phase3_dz_while_not_rotated () =
  check ops "layout" [IDJN; INOP; IJMP; IDAT]
    (opcodes (chosen "(let (x 3) (seq (while (DZ x) (NOP)) (DAT 0 (store x))))"))

let binary = "(let (x 3) (seq (while (NE x 0) (SUB 1 x)) (DAT 0 (store x))))"

let test_phase3_binary_while_by_policy () =
  check ops "no gain here: kept" [ISNE; IJMP; ISUB; IJMP; IDAT] (opcodes (chosen binary)) ;
  check ops "forced: rotated" [IJMP; ISUB; ISEQ; IJMP; IDAT]
    (opcodes (forced { no_opts with rotate_binary = true } binary))


let test_phase3_driver_uses_the_choice () =
  let o = drive [("p.src", countdown)] ["--report"; "p.src"] in
  check Alcotest.bool "rotated" true (contains o.out "_WHC") ;
  check Alcotest.bool "the report says so" true (contains o.err "optimizations: rotate-unary") ;
  let o = drive [("p.src", "(program (optimize boot speed) " ^ countdown ^ ")")] ["--report"; "p.src"] in
  check Alcotest.bool "boot first: not rotated" false (contains o.out "_WHC") ;
  check Alcotest.bool "the report says so" true (contains o.err "optimizations: none")


let last_instr (is : instruction list) : instruction =
  List.hd (List.rev (List.filter (fun i -> match i with INSTR _ -> true | ICOM _ | ILAB _ -> false) is))

let instr_t = testable (fun f i -> Format.pp_print_string f (String.trim (pp_instrs [i]))) (=)

let test_phase3_repeat_data () =
  let is = chosen "(let (p 20) (repeat (seq (ADD 10 p) (MOV 0 (Ind p))) (store p)))" in
  let rep = first_label "_REP" is in
  check instr_t "p lives in the JMP's B-field" (INSTR (IJMP, RN, RLab (RDir, rep), RRef (RImm, 20))) (last_instr is) ;
  check Alcotest.bool "labelled as p's cell" true (List.mem (ILAB "_LET1") is) ;
  check rmods "ADD to p's B-field" [RAB] (modifiers IADD "(let (p 20) (repeat (seq (ADD 10 p) (MOV 0 (Ind p))) (store p)))") ;
  check instr_t "plain data" (INSTR (IJMP, RN, RLab (RDir, "_REP1"), RRef (RImm, 7))) (last_instr (chosen "(repeat (NOP) 7)"))

let test_phase3_repeat_data_store_once () =
  check Alcotest.bool "stored twice is an error" true
    (contains (error_of "(let (p 1) (seq (repeat (NOP) (store p)) (DAT 0 (store p))))") "stored twice")


let vars : (string -> string, unit, string) format = "(let (x 1) (let (p 9) (seq %s (DAT (store x) (store p)))))"

let test_phase3_peephole_removes_jumps_to_next () =
  let ops_of body = opcodes (chosen (Printf.sprintf vars body)) in
  check ops "empty if" [IDAT] (ops_of "(if (JN x) (seq))") ;
  check ops "empty else" [IJMZ; INOP; IDAT] (ops_of "(if (JN x) (NOP) (seq))") ;
  check ops "empty rotated while" [IJMN; IDAT] (ops_of "(while (JN x) (seq))")

let test_phase3_peephole_keeps_effects () =
  let ops_of body = opcodes (chosen (Printf.sprintf vars body)) in
  check ops "after a skip" [ISEQ; IJMP; IDAT] (ops_of "(if (EQ x 0) (seq))") ;
  check ops "DJN decrements" [IDJN; IDAT] (ops_of "(if (DZ x) (seq))") ;
  check ops "a predecrement" [IJMZ; IDAT] (ops_of "(if (JN (Dec p)) (seq))") ;
  check ops "a user label" [IJMZ; IDAT] (ops_of "(seq (label here) (if (JN x) (seq)))") ;
  check ops "a store" [IJMZ; IDAT] (opcodes (chosen "(let (y 1) (seq (if (JN (store y)) (seq)) (DAT 0 0)))"))


(* Tests from the phase-3 branch review *)
let pointer_in_jmp =
  "(let (x 0) (let (p 0) (let (k 3) (seq (repeat (seq (if (JZ k) (JMP 0)) (SUB 1 k) (if (JN x) (NOP))) (Inc p)) (DAT (store x) (store p)) (DAT (store k) 0)))))"

let test_review3_moving_jmp_not_threaded () =
  (* the repeat's JMP increments p every iteration: a jump threaded past it would skip that *)
  let is = chosen pointer_in_jmp in
  check strings "JMZ" [List.hd (List.rev (List.filter (String.starts_with ~prefix:"_IF")
    (List.filter_map (fun i -> match i with ILAB l -> Some l | INSTR _ | ICOM _ -> None) is)))] (targets IJMZ is)

let test_review3_rotated_while_is_the_while () =
  let src = "(let (x 3) (seq (while (JN x) (seq (SUB 1 x) (expect (cycles <= 3)))) (DAT 0 (store x))))" in
  check Alcotest.(pair string int) "its expectation is checked" ("", 0)
    (let o = drive [("p.src", src)] ["p.src"] in (o.err, o.code)) ;
  let loops src = (M.measure (L.build (O.compile_body M.default_policy (body_of src)))).loops in
  check Alcotest.(list (option string)) "described as the while" [Some "while"]
    (List.map (fun (l : M.loop_metrics) -> l.construct) (loops src)) ;
  check Alcotest.int "an if-else body: still one loop" 1
    (List.length (loops "(let (x 3) (seq (while (JN x) (if (JN x) (SUB 1 x) (SUB 2 x))) (DAT 0 (store x))))"))

let test_review3_peephole_keeps_numeric_spans () =
  (* JMP $2 and ADD $-2 count cells across the empty if: removing its jump would move their targets *)
  check ops "kept" [IJMP; IDAT; IJMZ; IADD; IJMP; IDAT; IDAT]
    (opcodes (chosen "(let (x 1) (seq (JMP (Dir 2)) (DAT 7 7) (if (JN x) (seq)) (ADD (Dir -2) c) (JMP 0) (DAT (store x) 0) (label c) (DAT 0 0)))")) ;
  (* $7998 is $-2 modulo the core *)
  check ops "kept, counted modulo the core" [IJMZ; INOP; IJMP; IADD; IJMP; IDAT; IDAT]
    (opcodes (chosen "(let (x 1) (seq (if (JN x) (NOP) (seq)) (ADD (Dir 7998) c) (JMP 0) (DAT (store x) 0) (label c) (DAT 0 0)))"))


(* Tests for the cell default: a plain reference or a pointer target names a cell, read at its
   B-field beside a value, whole against another cell *)
let test_cells_beside_a_value () =
  let in_a = "(let (x 7) (let (p 5) (seq %s (JMP 0) (DAT (store x) (store p)))))" in
  let in_b = "(let (x 7) (let (p 5) (seq %s (JMP 0) (DAT (store p) (store x)))))" in
  let md op tmpl body = modifiers op (Printf.sprintf (Scanf.format_from_string tmpl "%s") body) in
  check rmods "A-field variable into a plain reference" [RAB] (md IMOV in_a "(MOV x (Dir -1))") ;
  check rmods "B-field variable into a plain reference" [RB] (md IMOV in_b "(MOV x (Dir -1))") ;
  check rmods "a plain reference into an A-field variable" [RBA] (md IMOV in_a "(MOV (Dir -1) x)") ;
  check rmods "a number into an A-field pointer's target" [RAB] (md IADD in_b "(ADD 1 (Ind p))") ;
  check rmods "a number into a B-field pointer's target" [RAB] (md IADD in_a "(ADD 1 (Ind p))") ;
  check rmods "an A-field pointer's target, tested" [RB] (md IJMZ in_b "(JMZ (Dir 0) (Ind p))")

let test_cells_against_cells () =
  let src = "(let (a 100) (let (b 104) (seq %s (JMP 0) (DAT (store a) (store b)))))" in
  let md op body = modifiers op (Printf.sprintf (Scanf.format_from_string src "%s") body) in
  check rmods "two pointers, compared" [RI] (md ISNE "(if (NE (Ind a) (Ind b)) (NOP))") ;
  check rmods "a label into a pointer" [RI] (md IMOV "(MOV top (Ind b))") ;
  check rmods "arithmetic: the ICWS'94 default" [RF] (md IADD "(ADD (Ind a) (Ind b))")


(* Tests for constants (EQU) and expressions *)
let out_of (src : string) : string = (drive [("p.src", src)] ["p.src"]).out

let line_with (needle : string) (text : string) : string =
  Option.value ~default:"" (List.find_opt (fun l -> contains l needle) (String.split_on_char '\n' text))

let squash (s : string) : string = String.concat "" (String.split_on_char ' ' s)

let test_consts_equ () =
  let out = out_of "(program (const step 3044) (let (b 0) (seq (repeat (seq (ADD step b) (MOV I b (Ind b)))) (DAT 0 (store b)))))" in
  check Alcotest.string "EQU" "stepEQU3044" (squash (line_with "EQU" out)) ;
  check Alcotest.string "a constant is a number" "ADD.AB#step,$_LET1" (squash (line_with "ADD" out)) ;
  let lines = String.split_on_char '\n' out in
  let index needle = Option.get (List.find_index (fun l -> contains l needle) lines) in
  check Alcotest.bool "before the code" true (index "EQU" < index "ADD")

let test_consts_expressions () =
  let out = out_of "(program (const step 2667) (seq (JMP (+ imp (* 2 step))) (ADD (* 2 step) (Dir (+ imp 1))) (label imp) (MOV I (# 0) (Dir step))))" in
  check Alcotest.string "a label: direct" "JMP$imp+(2*step),#0" (squash (line_with "JMP" out)) ;
  check Alcotest.string "no label: immediate" "ADD.AB#2*step,$imp+1" (squash (line_with "ADD" out)) ;
  check Alcotest.string "a constant with a mode" "MOV.I#0,$step" (squash (line_with "MOV" out))

let test_consts_layout_evaluates () =
  (* imp is cell 2: from the JMP at 0, imp+2*step is 2 + 5334 cells ahead; from the ADD at 1,
     imp+1 is 2 cells ahead *)
  let p = L.build ~consts:[("step", XNum 2667)] (compile_body ~consts:[("step", XNum 2667)] (body_of
      "(seq (JMP (+ imp (* 2 step))) (ADD (* 2 step) (Dir (+ imp 1))) (label imp) (MOV I (# 0) (Dir step)))")) in
  check Alcotest.int "JMP target" 5336 p.cells.(0).a.value ;
  check Alcotest.(pair int int) "ADD operands" (5334, 2) (p.cells.(1).a.value, p.cells.(1).b.value) ;
  check Alcotest.int "MOV's B" 2667 p.cells.(2).b.value

let test_consts_errors () =
  let err src = (drive [("p.src", src)] ["p.src"]).err in
  check Alcotest.bool "a let of a constant's name" true
    (contains (err "(program (const x 3) (let (x 1) (DAT 0 (store x))))") "`x` is a constant") ;
  check Alcotest.bool "a variable in an expression" true
    (contains (err "(let (x 1) (seq (ADD (+ x 1) x) (DAT 0 (store x))))") "variable `x` cannot be part of an expression") ;
  check Alcotest.bool "a constant naming a label" true
    (contains (err "(program (const s (+ top 1)) (seq (label top) (DAT s 0)))") "a constant is a number") ;
  check Alcotest.bool "a label of a constant's name" true
    (contains (err "(program (const top 1) (seq (label top) (DAT 0 0)))") "`top` is a constant")


(* Tests for phase 4: warnings, driven by the policy *)
let err_of ?(args = []) (src : string) : string = (drive [("p.src", src)] (args @ ["p.src"])).err

let warnings_in (err : string) : string list =
  List.filter (fun l -> contains l ": warning: ") (String.split_on_char '\n' err)

let test_warnings_kept_rotation () =
  let boot_first = "(program (optimize boot speed) " ^ countdown ^ ")" in
  (match warnings_in (err_of boot_first) with
  | [w] ->
    check Alcotest.bool "at the while" true (String.starts_with ~prefix:"p.src:1:48: warning:" w) ;
    check Alcotest.bool "says what would remove it" true (contains w "rotat")
  | ws -> Alcotest.failf "one warning expected, got %d" (List.length ws)) ;
  check Alcotest.(list string) "rotated under speed first: none" [] (warnings_in (err_of countdown))

let bomber = "(let (p 0) (seq (repeat (seq (ADD 4 p) (MOV 0 (Ind p)) %s)) (DAT 0 (store p))))"

let test_warnings_step () =
  let src extra = Printf.sprintf (Scanf.format_from_string bomber "%s") extra in
  (match warnings_in (err_of ~args:["--optimize"; "size"] (src "")) with
  | [w] -> check Alcotest.bool "visits 2000 of 8000" true (contains w "2000 of 8000")
  | ws -> Alcotest.failf "one warning expected, got %d" (List.length ws)) ;
  check Alcotest.(list string) "stated with (expect (step 4)): none" [] (warnings_in (err_of (src "(expect (step 4))")))

let test_warnings_dead_code () =
  let dead = "(seq (repeat (NOP)) (MOV 0 1))" in
  (match warnings_in (err_of dead) with
  | [w] -> check Alcotest.bool "dead code at the MOV" true (String.starts_with ~prefix:"p.src:1:21: warning:" w && contains w "never executed")
  | ws -> Alcotest.failf "one warning expected, got %d" (List.length ws)) ;
  check Alcotest.(list string) "size not in the policy: none" [] (warnings_in (err_of ~args:["--optimize"; "speed"] dead)) ;
  check Alcotest.(list string) "--warn=none" [] (warnings_in (err_of ~args:["--warn=none"] dead)) ;
  check Alcotest.int "--warn=all adds it back" 1 (List.length (warnings_in (err_of ~args:["--warn=all"; "--optimize"; "speed"] dead)))

let test_warnings_archetypes_clean () =
  (* the archetypes are tight: under the default policy only steps that leave cells unvisited,
     which they do not state, are worth saying: the dwarf's and stone's mod 4, the scanner's 10
     (800 cells); the paper's copy pointer has no known step per lap (its inner loop's trip count is
     not known), so nothing is said about it *)
  List.iter (fun (name, n) ->
    let src = read_file ("archetypes/" ^ name ^ ".src") in
    check Alcotest.int name n (List.length (warnings_in (err_of src))))
    [("imp", 0); ("dwarf", 1); ("stone", 1); ("clear", 0); ("scanner", 1); ("seqscan", 0); ("paper", 0); ("impring", 0)]


(* Tests for the smaller gaps of the phase-1 review *)
let test_gaps_label_names () =
  check Alcotest.string "a pMARS predefined symbol"
    "p.src:1:6: error: `CORESIZE` is a pMARS predefined symbol and cannot be a label\n"
    (error_of "(seq (label CORESIZE) (DAT 0 0))") ;
  check Alcotest.string "a label's syntax"
    "p.src:1:6: error: `9x` is not a valid label: a letter, then letters, digits and _\n"
    (error_of "(seq (label 9x) (DAT 0 0))") ;
  check Alcotest.string "a reference's syntax"
    "p.src:1:6: error: `a-b` is not a valid label: a letter, then letters, digits and _\n"
    (error_of "(seq (JMP a-b) (DAT 0 0))")

let test_gaps_errors_say_where () =
  check Alcotest.bool "a syntax error: file:line:col, 1-based" true
    (String.starts_with ~prefix:"p.src:3:11: error:" (error_of "(seq\n  (MOV 0 1)\n  (ADD 1 2")) ;
  check Alcotest.string "stored twice: at the second store"
    "p.src:1:35: error: variable `x` is stored twice; a let variable lives in one cell\n"
    (error_of "(let (x 1) (seq (DAT (store x) 0) (DAT 0 (store x))))") ;
  check Alcotest.string "a condition's error before its body's"
    "p.src:1:17: error: (DN x) is only available in do-while\n"
    (error_of "(let (x 1) (seq (if (DN x) (MOV (store y) 0)) (DAT 0 (store x))))") ;
  let long = "(seq (JMP " ^ String.make 250 'a' ^ ") (label " ^ String.make 250 'a' ^ ") (DAT 0 0))" in
  check Alcotest.bool "a long line: at the node that emits it" true
    (String.starts_with ~prefix:"p.src:1:6: error: redcode line" (error_of long))

let test_gaps_locations_cleared () =
  ignore (sexp_from_string "(seq (MOV 0 1) (ADD 1 2) (SUB 3 4))") ;
  ignore (sexp_from_string "(NOP)") ;
  (* (NOP) is one list and one atom *)
  check Alcotest.int "only the last parse's nodes" 2 (Phys.length locations)


(* A pointer's step is its net change over one lap *)
let steps_of_loop (m : M.t) (construct : string) : (int * int) list =
  let l = List.find (fun (l : M.loop_metrics) -> l.construct = Some construct) m.loops in
  List.filter_map (fun (p : M.prediction) -> match p with
    | Step s when s.loop = l.loop.header -> Some (s.cell, s.k)
    | Step _ | Counter _ -> None) m.predictions

let test_net_step () =
  (* the paper: d (cell 6) moves -1 seven times in the inner do-while, then +2365 in the outer
     repeat; the inner loop's trip count is not known, so the outer loop predicts nothing for d *)
  let m = metrics_of "bbctests/archetypes/paper.bbc" in
  check Alcotest.(list (pair int int)) "outer: nothing for d" [] (List.filter (fun (c, _) -> c = 6) (steps_of_loop m "repeat")) ;
  check Alcotest.(list (pair int int)) "inner: -1 on d" [(6, 7999)] (List.filter (fun (c, _) -> c = 6) (steps_of_loop m "do-while")) ;
  (* two changes to one pointer on every lap add up *)
  let m = M.measure (layout_of_src "(let (p 0) (seq (repeat (seq (ADD 3 p) (MOV 0 (Ind p)) (ADD 5 p))) (DAT 0 (store p))))") in
  check Alcotest.(list (pair int int)) "3 + 5" [(4, 8)] (steps_of_loop m "repeat")


let test_json_names_optimizations () =
  let json src = (drive [("p.src", src)] ["--report=json"; "p.src"]).out in
  check Alcotest.bool "rotated" true (contains (json countdown) "\"optimizations\":[\"rotate-unary\"]") ;
  check Alcotest.bool "none" true (contains (json "(MOV 0 1)") "\"optimizations\":[]")


(* Tests from the phase-4 branch review *)
let test_review4_referenced_cells_are_data () =
  let dead src = (M.measure (layout_of_src src)).unreachable in
  check Alcotest.int "an SPL bomb the MOV copies" 0
    (dead "(let (p 100) (seq (repeat (seq (MOV I bomb (Ind p)) (ADD 4 p))) (DAT 0 (store p)) (label bomb) (SPL 0 0)))") ;
  check Alcotest.int "prog3's cells read through labels" 0 (dead (golden_src (example "prog3"))) ;
  check Alcotest.int "a cell written through a numeric offset" 0 (dead (golden_src (example "mixed_operand_field"))) ;
  check Alcotest.int "a dead MOV nobody references" 1 (dead "(seq (repeat (NOP)) (MOV 0 1))")


let test_review4_every_change_counts () =
  let steps src = steps_of_loop (M.measure (layout_of_src src)) "repeat" in
  (* JMZ $1, >p moves p too: 3 + 1 a lap (pMARS: p = 50 after 10 laps from 10) *)
  check Alcotest.(list (pair int int)) "a > on a JMZ" [(4, 4)]
    (steps "(let (p 10) (seq (repeat (seq (ADD 3 p) (MOV 0 (Ind p)) (JMZ (Dir 1) (Inc p)))) (DAT 0 (store p))))") ;
  (* the A operand's } moves an A-field pointer *)
  check Alcotest.(list (pair int int)) "a } on the A operand" [(3, 3)]
    (steps "(let (p 10) (seq (repeat (seq (ADD 2 p) (MOV I (Inc p) (Dir 5)))) (DAT (store p) 0)))") ;
  (* MOV 7 p resets p: its step is not known, so none is predicted *)
  check Alcotest.(list (pair int int)) "a write that is not a step" []
    (steps "(let (p 10) (seq (repeat (seq (ADD 3 p) (MOV 7 p) (MOV 0 (Ind p)))) (DAT 0 (store p))))")


let test_review4_expression_checks () =
  check Alcotest.string "a division by zero" "p.src:1:6: error: a division by zero in an expression: pMARS rejects the warrior\n"
    (error_of "(seq (DAT (/ 7 0) 1) (DAT 0 0))") ;
  check Alcotest.string "through a constant" "p.src:1:27: error: a division by zero in an expression: pMARS rejects the warrior\n"
    (error_of "(program (const z 0) (seq (DAT (% 7 z) 1) (DAT 0 0)))") ;
  check Alcotest.bool "in a constant" true
    (contains (error_of "(program (const z 0) (const w (/ 1 z)) (DAT w 0))") "a division by zero") ;
  check Alcotest.bool "a name pMARS cannot read" true
    (contains (error_of "(seq (MOV 0 (+ 1abc 1)) (DAT 0 0))") "`1abc` is not a valid label") ;
  (* CORESIZE is a number pMARS knows: immediate, 8000 + 1, no undefined label *)
  let src = "(seq (DAT (+ CORESIZE 1) 0) (DAT 0 0))" in
  check Alcotest.bool "a predefined symbol is a number" true (contains (out_of src) "DAT    #CORESIZE+1") ;
  let p = layout_of_src src in
  check Alcotest.(pair int int) "8001 mod 8000, and no diagnostic" (1, 0) (p.cells.(0).a.value, List.length p.diagnostics)

let test_review4_header_step_is_stated () =
  check Alcotest.(list string) "(expect (step 4)) in the header" []
    (warnings_in (err_of ("(program (expect (step 4)) " ^ Printf.sprintf (Scanf.format_from_string bomber "%s") "" ^ ")")))


(* Tests for phase 5: hills and warrior metadata *)
let first_lines (n : int) (text : string) : string list =
  List.filteri (fun i _ -> i < n) (List.filter (fun l -> l <> "") (String.split_on_char '\n' text))

let test_hills_header_and_flag () =
  check Alcotest.(list string) "(hill 94nop)" [";redcode-94nop"; ";assert CORESIZE==8000 && MAXLENGTH==100"]
    (first_lines 2 (out_of "(program (hill 94nop) (MOV 0 1))")) ;
  let o = drive [("p.src", "(program (hill 94nop) (MOV 0 1))")] ["--hill"; "tiny"; "--report"; "p.src"] in
  check Alcotest.(list string) "--hill overrides" [";redcode-tiny"; ";assert CORESIZE==800 && MAXLENGTH==20"] (first_lines 2 o.out) ;
  check Alcotest.bool "measured on its core and length" true (contains o.err "length 2/20") ;
  check Alcotest.(list string) "no hill: 94b, no assert" [";redcode-94b"; "MOV.I  $0     , $1"]
    (List.map String.trim (first_lines 2 (out_of "(MOV (Dir 0) (Dir 1))")))

let test_hills_rules () =
  check Alcotest.string "unknown" "p.src: error: unknown hill `foo`: one of 94b, 94nop, 94, 94x, tiny, nano\n"
    (error_of "(program (hill foo) (MOV 0 1))") ;
  check Alcotest.bool "no p-space on 94nop" true
    (contains (error_of "(program (hill 94nop) (seq (LDP 0 1) (DAT 0 0)))") "94nop has no p-space") ;
  check Alcotest.bool "longer than the hill allows" true
    (contains (error_of ("(program (hill tiny) (seq " ^ String.concat " " (List.init 20 (fun _ -> "(NOP)")) ^ "))"))
       "the warrior is 21 cells; tiny allows 20")

let test_metadata () =
  check Alcotest.(list string) "name, author, strategy, as written"
    [";redcode-94b"; ";name My Dwarf"; ";author Somebody"; ";strategy bombs every fourth cell"]
    (first_lines 4 (out_of "(program (name My Dwarf) (author Somebody) (strategy bombs every fourth cell) (MOV 0 1))"))


let test_review5_step_modulo_core () =
  (* a step is a number modulo the core: -4 and 7996 are one step on 94b, 3044 and -156 on tiny *)
  let sub k = Printf.sprintf "(let (p 0) (seq (repeat (seq (expect (step %s)) (SUB 4 p) (MOV 0 (Ind p)))) (DAT 0 (store p))))" k in
  check Alcotest.(pair string int) "7996" ("", 0) (let o = drive [("p.src", sub "7996")] ["p.src"] in (o.err, o.code)) ;
  check Alcotest.(pair string int) "-4" ("", 0) (let o = drive [("p.src", sub "-4")] ["p.src"] in (o.err, o.code)) ;
  let tiny = "(program (hill tiny) (let (b 0) (seq (repeat (seq (expect (step 3044)) (ADD 3044 b) (MOV I b (Ind b)))) (DAT 0 (store b)))))" in
  check Alcotest.int "3044 on tiny" 0 (drive [("p.src", tiny)] ["p.src"]).code


let test_review5_header_items () =
  let err src = error_of src in
  check Alcotest.bool "a newline cannot reach the output" true
    (contains (err "(program (name \"x\n  JMP 0\") (MOV 0 1))") "one line") ;
  check Alcotest.bool "an empty word" true (contains (err "(program (author \"\") (MOV 0 1))") "an (author ...) needs words") ;
  check Alcotest.bool "no words" true (contains (err "(program (author) (MOV 0 1))") "an (author ...) needs words") ;
  check Alcotest.bool "a second name" true (contains (err "(program (name a) (name b) (MOV 0 1))") "(name ...) is given twice") ;
  check Alcotest.bool "a second hill" true (contains (err "(program (hill 94b) (hill tiny) (MOV 0 1))") "(hill ...) is given twice") ;
  check Alcotest.(list string) "strategy may repeat" [";strategy one"; ";strategy two"]
    (List.filter (fun l -> String.starts_with ~prefix:";strategy" l)
       (String.split_on_char '\n' (out_of "(program (strategy one) (strategy two) (MOV 0 1))")))


(* Tests for phase 6: A-field modes, the entry point, the macro layer *)
let test_amodes_on_numbers () =
  let line needle src = squash (line_with needle (out_of src)) in
  check Alcotest.string "} on a number" "MOV.I}2,$5" (line "MOV" "(seq (MOV I (} 2) (Dir 5)) (JMP 0))") ;
  check Alcotest.string "names" "MOV.I{2,*top" (line "MOV" "(seq (label top) (MOV I (ADec 2) (AInd top)) (JMP 0))") ;
  check Alcotest.string "AInc on an expression" "ADD.AB#1,}top+1" (line "ADD" "(seq (label top) (ADD 1 (AInc (+ top 1))) (JMP 0))") ;
  check Alcotest.bool "not on a variable" true
    (contains (error_of "(let (x 3) (seq (MOV 0 (} x)) (DAT 0 (store x))))") "its (store x) decides")


let clear_with_start =
  "(program (start top) (let (p 4) (seq (DAT 0 (store p)) (label top) (repeat (MOV I bomb (Inc p))) (label bomb) (DAT 0 0))))"

let test_start_entry () =
  let out = out_of clear_with_start in
  check Alcotest.string "ORG" "ORGtop" (squash (line_with "ORG" out)) ;
  let o = drive [("p.src", clear_with_start)] ["--report"; "p.src"] in
  (* measured from top: the pointer's cell before it is data, not dead code, and the loop starts at once *)
  check Alcotest.bool "boot 0" true (contains o.err "boot 0") ;
  check Alcotest.bool "nothing dead" true (contains o.err "unreachable 0") ;
  check Alcotest.bool "an unknown label" true
    (contains (error_of "(program (start nowhere) (MOV 0 1))") "(start nowhere): no label `nowhere`")


(* Tests for the macro layer: typed templates and for *)
let lines_with (needle : string) (text : string) : string list =
  List.map squash (List.filter (fun l -> contains l needle) (String.split_on_char '\n' text))

let test_macros_num_and_for () =
  let out = out_of "(program (define (put (k Num)) (MOV k (Dir k))) (seq (for i 1 3 (put i)) (JMP 0)))" in
  check Alcotest.(list string) "three expansions" ["MOV.AB#1,$1"; "MOV.AB#2,$2"; "MOV.AB#3,$3"] (lines_with "MOV" out) ;
  let out = out_of "(program (const step 10) (define (at (k Num)) (DAT 0 (* k step))) (seq (JMP 0) (for i 1 2 (at i))))" in
  check Alcotest.(list string) "a Num in an expression" ["DAT#0,#1*step"; "DAT#0,#2*step"] (lines_with "DAT    #0" out)

let test_macros_lab_var_code () =
  let out = out_of "(program (define (guard (x Var) (out Lab) (body Code)) (if (JZ x) (JMP out) body)) (let (c 0) (seq (guard c done (NOP)) (label done) (JMP 0) (DAT 0 (store c)))))" in
  check Alcotest.bool "the label passed in" true (contains (squash out) "JMP$done") ;
  check Alcotest.bool "the variable passed in" true (contains (squash out) "$_LET1") ;
  check Alcotest.bool "the code passed in" true (contains (squash out) "NOP")

let test_macros_hygiene () =
  let out = out_of "(program (define (spin) (seq (label here) (JMP here))) (seq (spin) (spin)))" in
  check Alcotest.(list string) "two labels, one per expansion" ["JMP$_X1_here,#0"; "JMP$_X2_here,#0"] (lines_with "JMP" out) ;
  (* the template's own let x does not capture the caller's x passed as y *)
  let out = out_of "(program (define (bump (y Var)) (let (x 5) (seq (ADD 1 y) (JMP 0) (DAT 0 (store x))))) (let (x 0) (seq (bump x) (DAT 0 (store x)))))" in
  check Alcotest.(list string) "the caller's x" ["ADD.AB#1,$_LET1"] (lines_with "ADD" out)

let test_macros_errors () =
  let err src = error_of src in
  let has needle src = check Alcotest.bool needle true (contains (err src) needle) in
  has "`put` takes 1 argument, given 2" "(program (define (put (k Num)) (MOV k 1)) (put 1 2))" ;
  has "`put`'s k is a Num" "(program (define (put (k Num)) (MOV k 1)) (put (seq (NOP))))" ;
  has "`at`'s l is a Lab" "(program (define (at (l Lab)) (JMP l)) (at 3))" ;
  has "`bump`'s v is a Var: `q` is not a let variable here" "(program (define (bump (v Var)) (ADD 1 v)) (bump q))" ;
  has "`b` calls `c`, which is not defined before it" "(program (define (b) (c)) (define (c) (NOP)) (b))" ;
  has "a (for ...) bound must be a number known when compiling" "(program (seq (label top) (for i 1 top (NOP))))" ;
  has "a (for ...) repeats at most 1000 times" "(program (for i 1 5000 (NOP)))" ;
  has "variable `v` is stored twice" "(program (define (twice) (let (v 1) (seq (DAT 0 (store v)) (DAT 0 (store v))))) (twice))" ;
  (* an error inside an expansion says where the call is *)
  check Alcotest.bool "at the call" true
    (String.starts_with ~prefix:"p.src:1:43: error:" (err "(program (define (bad) (MOV 0 (store z))) (bad))"))


(* A for inside a template replaces its own variable only: never a name the caller passed in *)
let test_review6_for_does_not_capture () =
  let thrice = out_of "(program (define (thrice (c Code)) (for i 1 3 c)) (let (i 0) (seq (thrice (ADD 1 i)) (JMP 0) (DAT 0 (store i)))))" in
  check Alcotest.(list string) "Code argument" ["ADD.AB#1,$_LET1"; "ADD.AB#1,$_LET1"; "ADD.AB#1,$_LET1"] (lines_with "ADD" thrice) ;
  let bump = out_of "(program (define (bump (v Var)) (for k 1 2 (ADD 1 v))) (let (k 0) (seq (bump k) (JMP 0) (DAT 0 (store k)))))" in
  check Alcotest.(list string) "Var argument" ["ADD.AB#1,$_LET1"; "ADD.AB#1,$_LET1"] (lines_with "ADD" bump) ;
  let go = out_of "(program (define (go (l Lab)) (for k 1 2 (JMP l))) (seq (label k) (go k)))" in
  check Alcotest.(list string) "Lab argument" ["JMP$k,#0"; "JMP$k,#0"] (lines_with "JMP" go)

(* A name bound inside a for's body would be replaced by the for's numbers: it is an error *)
let test_review6_for_variable_rebound () =
  let has needle src = check Alcotest.bool needle true (contains (error_of src) needle) in
  has "`k` is the variable of a (for k ...)" "(program (for k 1 2 (let (k 3) (seq (ADD 1 k) (DAT 0 (store k))))))" ;
  has "`k` is the variable of a (for k ...)" "(program (for k 1 2 (for k 1 2 (NOP))))" ;
  has "`k` is the variable of a (for k ...)" "(program (for k 1 2 (seq (label k) (NOP))))"

(* A template's own label is a label: in an expression, and as a Lab argument *)
let test_review6_template_label_is_a_label () =
  check Alcotest.string "in an expression" "" (error_of "(program (define (spin) (seq (label here) (NOP) (JMP (+ here 0)))) (spin))") ;
  let out = out_of "(program (define (go (t Lab)) (JMP t)) (define (loop) (seq (label here) (NOP) (go here))) (loop))" in
  check Alcotest.(list string) "as a Lab argument" ["JMP$_X1_here,#0"] (lines_with "JMP" out) ;
  check Alcotest.bool "an error names the label as written" true
    (contains (error_of "(program (define (at (l Lab)) (JMP l)) (define (b) (let (v 1) (seq (at v) (DAT 0 (store v))))) (b))") "a label, not v")

(* A label a template defines is the user's label renamed: every guard that keeps a user's label
   keeps it *)
let test_review6_template_label_is_the_users () =
  let jumps src = List.length (lines_with "JMP" (out_of src)) in
  check Alcotest.int "outside a template: the labelled jump stays" 2
    (jumps "(let (x 0) (seq (if (JZ x) (seq (NOP) (label l)) (seq)) (ADD 1 l) (JMP 0) (DAT 0 (store x))))") ;
  check Alcotest.int "inside a template: the same" 2
    (jumps "(program (define (t (v Var)) (seq (if (JZ v) (seq (NOP) (label l)) (seq)) (ADD 1 l))) (let (x 0) (seq (t x) (JMP 0) (DAT 0 (store x)))))") ;
  check Alcotest.bool "a labelled DAT in a template is data, not dead code" false
    (contains (error_of "(program (define (b) (seq (JMP 0) (label bomb) (DAT 0 0))) (b))") "dead code")

(* A cell used as a pointer counts, through its value, the cells between it and its target: removing
   one of them would move the target and not the value *)
let test_review6_pointer_values_count () =
  let src p = Printf.sprintf "(program (start top) (let (p %d) (seq (DAT 0 (store p)) (label top) (if (EQ 1 1) (NOP)) (MOV 9 (Ind p)) (JMP 0) (label target) (DAT 7 7))))" p in
  check Alcotest.(list string) "a let pointer across the jump: kept" ["SEQ.AB#1,#1"] (lines_with "SEQ" (out_of (src 6))) ;
  check Alcotest.(list string) "a let pointer before it: fused" ["SNE.AB#1,#1"] (lines_with "SNE" (out_of (src (-1)))) ;
  let labelled = "(seq (label ptr) (DAT 0 6) (label top) (if (EQ 1 1) (NOP)) (MOV 9 (Ind ptr)) (JMP 0) (label target) (DAT 7 7))" in
  check Alcotest.(list string) "a labelled pointer across the jump: kept" ["SEQ.AB#1,#1"] (lines_with "SEQ" (out_of ("(program (start top) " ^ labelled ^ ")")))

(* The smaller gaps of the phase-6 review *)
let test_phase7_template_binders_are_scoped () =
  (* a template's let and for rename their name inside their own scope only: the global label stays *)
  let out = out_of "(program (define (t) (seq (JMP top) (let (top 1) (seq (ADD 1 top) (DAT 0 (store top)))))) (seq (label top) (t)))" in
  check Alcotest.(list string) "a let's name outside the let" ["JMP$top,#0"] (lines_with "JMP" out) ;
  let out = out_of "(program (define (t) (seq (JMP k) (for k 1 2 (NOP)))) (seq (label k) (t)))" in
  check Alcotest.(list string) "a for's name outside the for" ["JMP$k,#0"] (lines_with "JMP" out) ;
  (* a let's initial value is outside its scope: it names the program's label *)
  let out = out_of "(program (define (t) (let (top top) (seq (ADD 1 top) (DAT 0 (store top))))) (seq (label top) (t)))" in
  check Alcotest.(list string) "a let's initial value" ["DAT#0,$top"; "DAT$0,$0"] (lines_with "DAT" out)

let test_phase7_expansion_limits () =
  let has needle src = check Alcotest.bool needle true (contains (error_of src) needle) in
  has "a (for ...) repeats at most 1000 times" "(program (for k -4000000000000000000 4000000000000000000 (NOP)))" ;
  has "the program expands to more than 10000" "(program (for i 1 1000 (for j 1 1000 (NOP))))"

let test_phase7_header_words_and_later_calls () =
  check Alcotest.bool "a header word names no template" true
    (contains (error_of "(program (define (start (l Lab)) (JMP l)) (seq (label foo) (start foo)))") "`start` is a RED word") ;
  (* only a template's name can be read as a header item: a parameter or a for variable may take one *)
  check Alcotest.bool "a header word as a parameter or for variable" false
    (contains (error_of "(program (define (t (name Num)) (DAT 0 name)) (seq (t 3) (for name 1 2 (DAT 0 name))))") "error:") ;
  check Alcotest.bool "a let binding is no call" false
    (contains (error_of "(program (define (a) (let (b 3) (seq (NOP) (DAT 0 (store b))))) (define (b) (NOP)) (seq (a) (b)))") "error:")

let test_phase7_afield_error_names_the_variable () =
  let err = error_of "(program (define (t) (let (v 1) (seq (MOV 0 (} v)) (DAT 0 (store v))))) (t))" in
  check Alcotest.bool "as written" true (contains err "`v` is a let variable: its (store v)") ;
  let err = error_of "(let (x 1) (seq (MOV 0 (} x)) (DAT 0 (store x))))" in
  check Alcotest.bool "outside a template" true (contains err "`x` is a let variable: its (store x)")

(* A template's let or for named like a parameter shadows it, as a let does elsewhere *)
let test_phase8_let_shadows_a_parameter () =
  let out = out_of "(program (define (t (x Num)) (let (x 1) (seq (ADD 1 x) (JMP 0) (DAT 0 (store x))))) (t 5))" in
  check Alcotest.(list string) "inside the let: the variable" ["ADD.AB#1,$_LET1"] (lines_with "ADD" out) ;
  let out = out_of "(program (define (t (x Num)) (seq (MOV x 7) (let (x 1) (seq (ADD 1 x) (JMP 0) (DAT 0 (store x)))))) (t 5))" in
  check Alcotest.(list string) "before the let: the argument" ["MOV.AB#5,#7"] (lines_with "MOV" out)

(* A let's name is no number: every use would read as the number *)
let test_phase8_let_binder_is_a_name () =
  check Alcotest.bool "a number" true
    (contains (error_of "(let (5 1) (seq (ADD 1 5) (JMP 0) (DAT 0 (store 5))))") "`5` is a number and cannot name a variable")

(* Errors name a template's variable as written; a template's let does not hide a template *)
let test_phase8_template_names_in_errors () =
  let err = error_of "(program (define (t) (let (v 1) (let (v (+ v 1)) (seq (ADD 1 v) (DAT 0 (store v)))))) (seq (label v) (t)))" in
  check Alcotest.bool "an expression" true (contains err "variable `v` cannot be part of an expression") ;
  let err = error_of "(program (define (n (a Num)) (DAT 0 a)) (define (t) (let (v 1) (seq (n v) (DAT 0 (store v))))) (t))" in
  check Alcotest.bool "a Num argument" true (contains err "`v` is a let variable (pass it as a Var)") ;
  let out = out_of "(program (define (b) (NOP)) (define (t) (let (b 3) (seq (b) (JMP 0) (DAT 0 (store b))))) (t))" in
  check Alcotest.(list string) "a call in a let of its name" ["NOP#0,#0"] (lines_with "NOP" out)

(* (include "path") brings a file's templates, the path relative to the including file *)
let test_phase8_include () =
  let spin = "(define (spin) (seq (label here) (JMP here)))" in
  let o = drive [("p.src", "(program (include \"lib.src\") (spin))"); ("lib.src", spin)] ["p.src"] in
  check Alcotest.(list string) "one level" ["JMP$_X1_here,#0"] (lines_with "JMP" o.out) ;
  let o = drive [("w/p.src", "(program (include \"lib/a.src\") (seq (twice) (JMP 0)))");
                 ("w/lib/a.src", "(include \"b.src\") (define (twice) (seq (spin) (spin)))"); ("w/lib/b.src", spin)] ["w/p.src"] in
  check Alcotest.int "nested, relative to the including file" 3 (List.length (lines_with "JMP" o.out)) ;
  let o = drive [("p.src", "(program (include \"a.src\") (include \"b.src\") (seq (one) (two)))");
                 ("a.src", "(include \"c.src\") (define (one) (spin))"); ("b.src", "(include \"c.src\") (define (two) (spin))"); ("c.src", spin)] ["p.src"] in
  check Alcotest.bool "a file included twice is read once" false (contains o.err "error:")

let test_phase8_include_errors () =
  let err files = (drive files ["p.src"]).err in
  check Alcotest.bool "a cycle" true
    (contains (err [("p.src", "(program (include \"a.src\") (NOP))"); ("a.src", "(include \"b.src\")"); ("b.src", "(include \"a.src\")")]) "include cycle: p.src -> a.src -> b.src -> a.src") ;
  check Alcotest.string "a missing file, at the include" "p.src:1:10: error: no such file: lib.src\n"
    (err [("p.src", "(program (include \"lib.src\") (NOP))")]) ;
  check Alcotest.bool "only templates" true
    (contains (err [("p.src", "(program (include \"lib.src\") (NOP))"); ("lib.src", "(const k 3)")]) "an included file holds only (define ...) and (include ...) items") ;
  check Alcotest.bool "an error inside says where" true
    (contains (err [("p.src", "(program (include \"lib.src\") (NOP))"); ("lib.src", "\n(define (t (k Bad)) (NOP))")]) "in lib.src:2:12: `Bad` is not a kind")

(* The phase-8 review's findings on include *)
let test_phase8_include_paths () =
  let spin = "(define (spin) (seq (label here) (JMP here)))" in
  let err files = (drive files ["p.src"]).err in
  check Alcotest.bool "./lib.src and lib.src are one file" false
    (contains (err [("p.src", "(program (include \"./lib.src\") (include \"lib.src\") (spin))"); ("lib.src", spin)]) "error:") ;
  check Alcotest.bool "x/../lib.src is lib.src" false
    (contains (err [("p.src", "(program (include \"x/../lib.src\") (include \"lib.src\") (spin))"); ("lib.src", spin)]) "error:") ;
  check Alcotest.bool "a file including itself by another spelling" true
    (contains (err [("p.src", "(program (include \"a.src\") (NOP))"); ("a.src", "(include \"x/../a.src\")")]) "include cycle") ;
  check Alcotest.bool "include is a header word" true
    (contains (err [("p.src", "(program (define (include (a Lab)) (JMP a)) (seq (label q) (include q)))")]) "`include` is a RED word") ;
  check Alcotest.bool "one path" true
    (contains (err [("p.src", "(program (include \"a\" \"b\") (NOP))")]) "an (include ...) takes one path")

(* An error on an atom of an included template points at the call *)
let test_phase8_included_atoms_are_located () =
  let err = (drive [("p.src", "(program (include \"lib.src\")\n  (t))"); ("lib.src", "(define (t) (for k 1 zz (NOP)))")] ["p.src"]).err in
  check Alcotest.bool "at the call" true (String.starts_with ~prefix:"p.src:2:" err)

(* A snippet's demo compiles to its archetype's code: the same text once generated labels are
   numbered in order of appearance and spacing is ignored *)
let normalized (text : string) : string list =
  let names = Hashtbl.create 16 in
  let rename w =
    (* a template's own label is the user's, renamed per expansion: _X1_bomb is bomb *)
    let w = if String.starts_with ~prefix:"_X" w then
        (match String.index_from_opt w 2 '_' with Some i -> String.sub w (i + 1) (String.length w - i - 1) | None -> w)
      else w in
    if String.length w > 1 && w.[0] = '_' then
      (match Hashtbl.find_opt names w with
      | Some n -> n
      | None -> let n = Printf.sprintf "L%d" (Hashtbl.length names) in Hashtbl.replace names w n ; n)
    else w in
  let token_split l =
    let b = Buffer.create 16 and out = Buffer.create 64 in
    let flush () = if Buffer.length b > 0 then (Buffer.add_string out (rename (Buffer.contents b)) ; Buffer.clear b) in
    String.iter (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' then Buffer.add_char b c
      else (flush () ; if c <> ' ' && c <> '\t' && c <> '\r' then Buffer.add_char out c)) l ;
    flush () ; Buffer.contents out in
  List.filter (fun l -> l <> "") (List.map token_split (String.split_on_char '\n' text))

let test_phase8_snippet_demos_are_their_archetypes () =
  List.iter (fun name ->
    check Alcotest.(list string) name
      (normalized (golden_expected ("bbctests/archetypes/" ^ name ^ ".bbc")))
      (normalized (golden_expected ("bbctests/snippets/" ^ name ^ ".bbc"))))
    ["imp"; "dwarf"; "stone"; "scanner"; "clear"; "paper"; "quickscan"]

(* The manual verifies itself: every ```red block in docs/manual/ compiles, and a ```redcode block
   is what the compiler prints for the nearest ```red block before it (trailing spaces aside) *)
let manual_blocks (text : string) : (string * string) list =
  let lines = String.split_on_char '\n' text in
  let rec go acc lines = match lines with
    | [] -> List.rev acc
    | l :: rest when String.starts_with ~prefix:"```" l ->
      let info = String.trim (String.sub l 3 (String.length l - 3)) in
      let rec body b ls = match ls with
        | [] -> (List.rev b, [])
        | x :: xs when String.trim x = "```" -> (List.rev b, xs)
        | x :: xs -> body (x :: b) xs in
      let b, rest = body [] rest in
      go ((info, String.concat "\n" b) :: acc) rest
    | _ :: rest -> go acc rest in
  go [] lines

let trimmed (text : string) : string =
  let strip l = let n = ref (String.length l) in
    while !n > 0 && (l.[!n - 1] = ' ' || l.[!n - 1] = '\r') do decr n done ; String.sub l 0 !n in
  String.trim (String.concat "\n" (List.map strip (String.split_on_char '\n' text)))

let test_manual_examples () =
  let dir = "docs/manual" in
  let files = Sys.readdir dir |> Array.to_list |> List.sort compare |> List.filter (fun f -> Filename.check_suffix f ".md") in
  check Alcotest.bool "the manual has chapters" true (files <> []) ;
  let examples = ref 0 in
  List.iter (fun f ->
    let rec walk last blocks = match blocks with
      | ("red", src) :: rest ->
        incr examples ;
        let o = Cored.Driver.run ~read:(golden_read src) ["golden.src"] in
        check Alcotest.(pair string int) (f ^ ": compiles\n" ^ src) ("", 0)
          ((if contains o.err "error:" then o.err else ""), o.code) ;
        walk (Some (src, o.out)) rest
      | ("redcode", expected) :: rest ->
        (match last with
        | Some (src, out) -> check Alcotest.string (f ^ ": output\n" ^ src) (trimmed expected) (trimmed out)
        | None -> Alcotest.fail (f ^ ": a redcode block with no red block before it")) ;
        walk last rest
      | _ :: rest -> walk last rest
      | [] -> () in
    walk None (manual_blocks (read_file (Filename.concat dir f)))) files ;
  check Alcotest.bool "the manual has examples" true (!examples > 0)

let test_fused_skip () =
  let two = "(let (a 0) (let (b 1) (seq %s (JMP 0) (DAT (store a) (store b)))))" in
  let ops_of body = opcodes (chosen (Printf.sprintf (Scanf.format_from_string two "%s") body)) in
  check ops "NE around one instruction" [ISEQ; IMOV; IJMP; IDAT] (ops_of "(if (NE a b) (MOV 1 a))") ;
  check ops "EQ around one instruction" [ISNE; IJMP; IJMP; IDAT] (ops_of "(if (EQ a b) (JMP 0))") ;
  check ops "two instructions: kept" [ISNE; IJMP; IMOV; IMOV; IJMP; IDAT] (ops_of "(if (NE a b) (seq (MOV 1 a) (MOV 2 b)))") ;
  check ops "GT: SLT has no inverse" [ISLT; IJMP; IMOV; IJMP; IDAT] (ops_of "(if (GT a b) (MOV 1 a))") ;
  check ops "after a skip: kept" [ISEQ; ISNE; IJMP; IMOV; IJMP; IDAT] (ops_of "(seq (SEQ a b) (if (NE a b) (MOV 1 a)))")


let test_fused_skip_with_expressions () =
  let src body = Printf.sprintf "(program (const s 400) (let (a 0) (let (b 1) (seq (label f) %s (JMP 0) (DAT (store a) (store b))))))" body in
  let tests body = List.filter_map (fun l ->
      if contains l "  SEQ" then Some "SEQ" else if contains l "  SNE" then Some "SNE" else None)
      (String.split_on_char '\n' (out_of (src body))) in
  (* the compared cells are 400 past f, outside the warrior: removing a cell moves nothing they name *)
  check Alcotest.(list string) "anchored outside: fused" ["SEQ"] (tests "(if (NE I (Dir (+ f s)) (Dir (+ f (+ s 1)))) (MOV 1 a))") ;
  (* f+3 is a cell of the warrior past the jump that would go, and f+5 the epilogue: they stay *)
  check Alcotest.(list string) "anchored inside, across the jump: kept" ["SNE"] (tests "(if (NE I (Dir (+ f 3)) (Dir (+ f s))) (MOV 1 a))") ;
  check Alcotest.(list string) "anchored on the epilogue: kept" ["SNE"] (tests "(if (NE I (Dir (+ f 5)) (Dir (+ f s))) (MOV 1 a))") ;
  (* an address is taken modulo the core: f+8003 is f+3 on 94b *)
  check Alcotest.(list string) "anchored past the core, wrapping inside: kept" ["SNE"] (tests "(if (NE I (Dir (+ f 8003)) (Dir (+ f s))) (MOV 1 a))")


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
    test_case "--emit-beh names the hill" `Quick test_driver_emit_beh_hill ;
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
    test_case "a condition takes a modifier" `Quick test_phase2_condition_modifier ;
    test_case "a condition's bad modifier is an error" `Quick test_phase2_condition_bad_modifier ;
    test_case "a labelled DAT never run is data" `Quick test_phase2_labelled_dat_is_data ;
  ] ;
  "review2", [
    test_case "a while at a loop's end keeps the loop's JMP" `Quick test_review2_while_at_loop_end ;
    test_case "GT with an explicit AB or BA" `Quick test_review2_gt_explicit_modifier ;
    test_case "a user label stops threading" `Quick test_review2_user_label_stops_threading ;
    test_case "a generated label does not name data" `Quick test_review2_generated_label_is_not_a_name ;
    test_case "a unary cond with too many arguments" `Quick test_review2_unary_arity_message ;
  ] ;
  "review3", [
    test_case "a JMP that moves a pointer is not threaded through" `Quick test_review3_moving_jmp_not_threaded ;
    test_case "a rotated while is described as the while" `Quick test_review3_rotated_while_is_the_while ;
    test_case "peephole keeps cells that numeric offsets count" `Quick test_review3_peephole_keeps_numeric_spans ;
  ] ;
  "phase6", [
    test_case "A-field modes on numbers and labels" `Quick test_amodes_on_numbers ;
    test_case "(start label) is the entry point" `Quick test_start_entry ;
    test_case "templates: Num, and for" `Quick test_macros_num_and_for ;
    test_case "templates: Lab, Var, Code" `Quick test_macros_lab_var_code ;
    test_case "templates: fresh labels and lets" `Quick test_macros_hygiene ;
    test_case "templates: what they reject" `Quick test_macros_errors ;
    test_case "an EQ/NE if around one instruction is a skip" `Quick test_fused_skip ;
    test_case "fusion with label-anchored expressions" `Quick test_fused_skip_with_expressions ;
    test_case "a for captures no argument" `Quick test_review6_for_does_not_capture ;
    test_case "a for's variable is not rebound inside it" `Quick test_review6_for_variable_rebound ;
    test_case "a template's label is a label" `Quick test_review6_template_label_is_a_label ;
    test_case "a template's label is the user's" `Quick test_review6_template_label_is_the_users ;
    test_case "a pointer's value counts cells" `Quick test_review6_pointer_values_count ;
    test_case "a template's binders are scoped" `Quick test_phase7_template_binders_are_scoped ;
    test_case "expansion limits" `Quick test_phase7_expansion_limits ;
    test_case "header words, later calls" `Quick test_phase7_header_words_and_later_calls ;
    test_case "the A-field error names the variable" `Quick test_phase7_afield_error_names_the_variable ;
    test_case "a template's let shadows a parameter" `Quick test_phase8_let_shadows_a_parameter ;
    test_case "a let's binder is a name" `Quick test_phase8_let_binder_is_a_name ;
    test_case "a template's names in errors and calls" `Quick test_phase8_template_names_in_errors ;
    test_case "include" `Quick test_phase8_include ;
    test_case "include errors" `Quick test_phase8_include_errors ;
    test_case "include paths" `Quick test_phase8_include_paths ;
    test_case "included atoms are located" `Quick test_phase8_included_atoms_are_located ;
    test_case "snippet demos are their archetypes" `Quick test_phase8_snippet_demos_are_their_archetypes ;
    test_case "the manual's examples compile as shown" `Quick test_manual_examples ;
  ] ;
  "review5", [
    test_case "a step is compared modulo the core" `Quick test_review5_step_modulo_core ;
    test_case "what a header item may hold" `Quick test_review5_header_items ;
  ] ;
  "hills", [
    test_case "a hill from the header or the flag" `Quick test_hills_header_and_flag ;
    test_case "what a hill rules out" `Quick test_hills_rules ;
    test_case "name, author, strategy" `Quick test_metadata ;
  ] ;
  "review4", [
    test_case "a cell the program references is data" `Quick test_review4_referenced_cells_are_data ;
    test_case "every change to a pointer counts in its step" `Quick test_review4_every_change_counts ;
    test_case "what an expression may hold" `Quick test_review4_expression_checks ;
    test_case "a header (expect (step k)) states the step" `Quick test_review4_header_step_is_stated ;
  ] ;
  "netstep", [
    test_case "a pointer's step is its net change per lap" `Quick test_net_step ;
    test_case "--report=json names the optimizations" `Quick test_json_names_optimizations ;
  ] ;
  "gaps", [
    test_case "which names may be labels" `Quick test_gaps_label_names ;
    test_case "errors say where" `Quick test_gaps_errors_say_where ;
    test_case "Parse.locations holds one parse" `Quick test_gaps_locations_cleared ;
  ] ;
  "warnings", [
    test_case "a while the policy kept unrotated" `Quick test_warnings_kept_rotation ;
    test_case "a step that leaves cells unvisited" `Quick test_warnings_step ;
    test_case "dead code, when size is in the policy" `Quick test_warnings_dead_code ;
    test_case "the archetypes" `Quick test_warnings_archetypes_clean ;
  ] ;
  "consts", [
    test_case "(const name n) is an EQU before the code" `Quick test_consts_equ ;
    test_case "expressions keep their names" `Quick test_consts_expressions ;
    test_case "Layout evaluates expressions" `Quick test_consts_layout_evaluates ;
    test_case "what a constant cannot be" `Quick test_consts_errors ;
  ] ;
  "cells", [
    test_case "a cell beside a value is its B-field" `Quick test_cells_beside_a_value ;
    test_case "a cell against a cell is the whole cell" `Quick test_cells_against_cells ;
  ] ;
  "phase3", [
    test_case "a unary while is rotated" `Quick test_phase3_unary_while_rotated ;
    test_case "rotation follows the policy" `Quick test_phase3_rotation_by_policy ;
    test_case "a DZ while is not rotated" `Quick test_phase3_dz_while_not_rotated ;
    test_case "a binary while: only if the policy picks it" `Quick test_phase3_binary_while_by_policy ;
    test_case "the command line uses the choice" `Quick test_phase3_driver_uses_the_choice ;
    test_case "(repeat body arg): data in the loop's JMP" `Quick test_phase3_repeat_data ;
    test_case "(repeat body (store p)) counts as p's store" `Quick test_phase3_repeat_data_store_once ;
    test_case "peephole: a jump to the next cell goes" `Quick test_phase3_peephole_removes_jumps_to_next ;
    test_case "peephole: what a jump does besides jumping stays" `Quick test_phase3_peephole_keeps_effects ;
  ] ;
  "interp", [

  ] ;
  "errors", [

  ]
]

(* Entry point of tester *)
let () =
  
  let compiler : compiler =
    (* A golden is what run_compile.exe prints (its header, policy, hill and all); a compile error is
       the error it prints. *)
    SCompiler ( fun _ s ->
      let o = Cored.Driver.run ~read:(golden_read s) ["golden.src"] in
      if o.code = 0 then o.out else failwith o.err ) in
  
  let bbc_tests =
    let name : string = "compare" in
    tests_from_dir ~name ~compiler "bbctests" in
  
  let verify_tests =
    let name : string = "execute" in
    let runtime: runtime = unix_command "pmars/pmars -A -@ pmars/config/94b.opt %s" in
    let testeable : testeable = compare_status in
    tests_from_dir ~name ~compiler ~runtime ~testeable "bbctests" in
  
  run "Tests corewars-compiler" (ocaml_tests @ bbc_tests @ verify_tests)
