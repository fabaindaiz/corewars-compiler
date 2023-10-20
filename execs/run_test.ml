open Dev.Ast
open Dev.Parse
open Dev.Compile
open Alcotest
open Bbcsteptester.Type
open Bbcsteptester.Test
open Bbcsteptester.Runtime
open Printf


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
    Compiler (fun s o -> fprintf o "%s" (compile_prog (parse_exp (sexp_from_string s))) ) in
  
  let bbc_tests =
    let name : string = "compare" in
    let runtime : runtime = compileout in
    let oracle : oracle = Expected in
    let action : action = CompareOutput in
    tests_from_dir ~name ~compiler ~runtime ~oracle ~ action "bbctests" in
  
  let verify_tests =
    let runtime =
      unixcommand (fun s -> CCUnix.call "rt/pmars-0.9.4/pmars -A -@ rt/pmars-0.9.4/config/94b.opt %s" s) in
    
    let name : string = "execute" in
    let oracle : oracle = Expected in
    let action : action = IgnoreOutput in
    tests_from_dir ~name ~compiler ~runtime ~oracle ~action "bbctests" in
  
  run "Tests corewars-compiler" (ocaml_tests @ bbc_tests @ verify_tests)
