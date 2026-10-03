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
