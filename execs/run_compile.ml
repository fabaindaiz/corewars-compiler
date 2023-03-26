open Dev.Parse
open Dev.Compile
open Printf

let () =
  let args = Sys.argv in
  if Array.length args > 1 && Sys.file_exists args.(1)
  then
    let src = sexp_from_file args.(1) in
    printf "%s\n" (compile_prog (parse_expr src))
  else
    printf "usage: run_compile.exe <filename>\n"
