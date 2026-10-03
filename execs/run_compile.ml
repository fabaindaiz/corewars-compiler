open Cored
open Printf

(* Standard output carries redcode only (pMARS and the goldens read it); the report goes to
   standard error, or replaces the redcode with --report=json. *)

type report = No_report | Text | Json

let usage = "usage: run_compile.exe [--optimize o1,o2] [--report[=json]] <filename>"

(* 94b: pmars/config/94b.opt, -l 100 *)
let maxlength = 100

let fail (msg : string) : 'a = eprintf "%s\n" msg ; exit 1

let () =
  let report = ref No_report and optimize = ref None and file = ref None in
  let rec go args = match args with
    | "--report" :: rest -> report := Text ; go rest
    | "--report=json" :: rest -> report := Json ; go rest
    | "--optimize" :: os :: rest -> optimize := Some (String.split_on_char ',' os) ; go rest
    | f :: rest when !file = None -> file := Some f ; go rest
    | _ :: _ -> fail usage
    | [] -> () in
  go (List.tl (Array.to_list Sys.argv)) ;
  match !file with
  | Some f when Sys.file_exists f ->
    let src = Parse.parse_source (Parse.sexp_from_file f) in
    let names = match !optimize with
      | Some names -> names
      | None -> Option.value src.optimize ~default:(List.map Metrics.string_of_objective Metrics.default_policy) in
    let policy = List.map (fun n -> match Metrics.objective_of_string n with
      | Some o -> o
      | None -> fail (sprintf "unknown objective `%s`: one of speed, size, stealth, boot" n)) names in
    let metrics () = Metrics.measure (Layout.of_expr src.body) in
    (match !report with
    | Json -> printf "%s\n" (Metrics.to_json (metrics ()))
    | Text ->
      eprintf "%spolicy: %s\n" (Metrics.to_text ~maxlength (metrics ()))
        (String.concat " > " (List.map Metrics.string_of_objective policy)) ;
      printf "%s\n" (Compile.compile_prog src.body)
    | No_report -> printf "%s\n" (Compile.compile_prog src.body))
  | Some _ | None -> printf "%s\n" usage
