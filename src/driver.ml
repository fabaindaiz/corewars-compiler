(** Driver: the command line, as a function from arguments to outputs **)
open Printf

(* Standard output carries redcode only (pMARS and the goldens read it); the report goes to
   standard error, or replaces the redcode with --report=json. The driver writes nothing itself:
   it returns what to print, what to write and the exit code, so it can be tested. *)

type output = {
  out : string;
  err : string;
  code : int;
  files : (string * string) list;  (* path, contents: written by the caller, in this order *)
}

type report = No_report | Text | Json

let usage = "usage: run_compile.exe [--optimize o1,o2] [--report[=json]] [--expect=warn] [--emit-beh FILE.beh] <filename>"

(* 94b: pmars/config/94b.opt, -l 100 *)
let maxlength = 100

exception Stop of output

let stop ?(code = 1) (err : string) : 'a = raise (Stop { out = ""; err = err ^ "\n"; code; files = [] })

(* --report and --report=json are two forms of one option, which Stdlib.Arg does not express. *)
let parse_args (args : string list) =
  let report = ref No_report and optimize = ref None and file = ref None in
  let warn = ref false and emit_beh = ref None in
  let rec go args = match args with
    | "--report" :: rest -> report := Text ; go rest
    | "--report=json" :: rest -> report := Json ; go rest
    | "--optimize" :: os :: rest -> optimize := Some (String.split_on_char ',' os) ; go rest
    | "--expect=warn" :: rest -> warn := true ; go rest
    | "--emit-beh" :: path :: rest -> emit_beh := Some path ; go rest
    | f :: rest when !file = None -> file := Some f ; go rest
    | _ :: _ -> stop usage
    | [] -> () in
  go args ;
  (!report, !optimize, !file, !warn, !emit_beh)

let execution (x : Ast.expectation) : bool =
  match x with
  | XAlive _ | XDead _ | XCell _ -> true
  | XLength _ | XCycles _ | XOverhead _ | XBoot _ | XStep _ | XCoversCore -> false

let compile ~(read : string -> string option) (args : string list) : output =
  let report, optimize, file, warn, emit_beh = parse_args args in
  let f = match file with Some f -> f | None -> raise (Stop { out = usage ^ "\n"; err = ""; code = 0; files = [] }) in
  let text = match read f with Some t -> t | None -> stop (sprintf "error: no such file: %s" f) in
  let sexp = match CCSexp.parse_string text with
    | Ok s -> s
    | Error msg -> stop (sprintf "error: unable to parse %s: %s" f msg) in
  let src = Parse.parse_source sexp in
  let names = match optimize with
    | Some names -> names
    | None -> Option.value src.optimize ~default:(List.map Metrics.string_of_objective Metrics.default_policy) in
  let policy = List.map (fun n -> match Metrics.objective_of_string n with
    | Some o -> o
    | None -> stop (sprintf "unknown objective `%s`: one of speed, size, stealth, boot" n)) names in
  let measured = lazy (Metrics.measure (Layout.of_expr src.body)) in
  let metrics () = Lazy.force measured in
  let expects = List.map (fun x -> (x, None)) src.expects @ Expect.collect (Ast.tag_expr src.body) in
  let failures = List.filter_map (fun x -> match Expect.check (metrics ()) x with
    | Some (Expect.Fail msg) -> Some msg
    | Some Expect.Pass | None -> None) expects in
  if failures <> [] && not warn then stop (String.concat "\n" failures) ;
  let warnings = String.concat "" (List.map (sprintf "warning: %s\n") failures) in
  let redcode = Compile.compile_prog src.body ^ "\n" in
  let files = match emit_beh with
    | None -> []
    | Some path ->
      (* The spec names its redcode beside it: a .beh path keeps the two apart. *)
      if not (Filename.check_suffix path ".beh") then stop "--emit-beh FILE must end in .beh" ;
      let probes = List.filter execution (List.map fst expects) in
      if probes = [] then stop "--emit-beh: the program has no (alive N), (dead N) or (cell ...) expectation to export" ;
      let red = Filename.remove_extension path ^ ".red" in
      [(red, redcode); (path, Expect.to_beh ~redcode:(Filename.basename red) probes)] in
  match report with
  | Json -> { out = Metrics.to_json ~policy (metrics ()) ^ "\n"; err = warnings; code = 0; files }
  | Text ->
    let r = sprintf "%spolicy: %s\n" (Metrics.to_text ~maxlength (metrics ()))
        (String.concat " > " (List.map Metrics.string_of_objective policy)) in
    { out = redcode; err = warnings ^ r; code = 0; files }
  | No_report -> { out = redcode; err = warnings; code = 0; files }

(* Every compile error ends as a message and exit code 1. The compiler still declares one CTError
   per module (i-7d2612-888db5), so each is caught here by name. *)
let run ~(read : string -> string option) (args : string list) : output =
  let error msg = { out = ""; err = sprintf "error: %s\n" msg; code = 1; files = [] } in
  try compile ~read args with
  | Stop o -> o
  | Parse.CTError msg | Compile.CTError msg | Lib.CTError msg | Util.CTError msg -> error msg
