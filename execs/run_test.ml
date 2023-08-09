open Dev.Ast
open Dev.Parse
open Dev.Compile
open Alcotest
open Bbcsteptester.Test
open Printf





(* OCaml tests: extend with your own tests *)
let ocaml_tests = [
  "parse", [

  ] ;
  "interp", [

  ] ;
  "errors", [

  ]
]

(* Entry point of tester *)
let () =
  (* BBC tests: don't change the following, simply add .bbc files in the bbctests/ directory *)
  let bbc_tests = 
    let compile_flags = Option.value (Sys.getenv_opt "CFLAGS") ~default:"-g" in
    let compiler : compiler = 
      Compiler (fun s o -> fprintf o "%s" (compile_prog (parse_exp (sexp_from_string s))) ) in
    let oracle : oracle = Expected in
    let runtime : runtime = CompileOut in
    tests_from_dir ~compile_flags ~compiler ~oracle ~runtime "bbctests" in
  run "Tests corewars-compiler" (ocaml_tests @ bbc_tests)
