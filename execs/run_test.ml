open Cored.Ast
open Cored.Parse
open Cored.Compile
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


(* OCaml tests: extend with your own tests *)
let ocaml_tests = [
  "parse", [
    test_case "A number" `Quick test_parse_num ;
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
