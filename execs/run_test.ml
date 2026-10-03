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
  | EComment _ | ELabel _ | EPrim2 _ -> None

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
