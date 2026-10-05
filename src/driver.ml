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

let usage = "usage: run_compile.exe [--optimize o1,o2] [--report[=json]] [--expect=warn] [--warn=all|none] [--hill KEY] [--emit-beh FILE.beh] <filename>"

exception Stop of output

let stop ?(code = 1) (err : string) : 'a = raise (Stop { out = ""; err = err ^ "\n"; code; files = [] })

(* --report and --report=json are two forms of one option, which Stdlib.Arg does not express. *)
let parse_args (args : string list) =
  let report = ref No_report and optimize = ref None and file = ref None in
  let warn = ref false and emit_beh = ref None and warnings = ref Warnings.Policy and hill = ref None in
  let rec go args = match args with
    | "--report" :: rest -> report := Text ; go rest
    | "--report=json" :: rest -> report := Json ; go rest
    | "--optimize" :: os :: rest -> optimize := Some (String.split_on_char ',' os) ; go rest
    | "--expect=warn" :: rest -> warn := true ; go rest
    | "--warn=all" :: rest -> warnings := Warnings.All ; go rest
    | "--warn=none" :: rest -> warnings := Warnings.Nothing ; go rest
    | "--hill" :: h :: rest -> hill := Some h ; go rest
    | "--emit-beh" :: path :: rest -> emit_beh := Some path ; go rest
    | f :: rest when !file = None -> file := Some f ; go rest
    | _ :: _ -> stop usage
    | [] -> () in
  go args ;
  (!report, !optimize, !file, !warn, !emit_beh, !warnings, !hill)

let execution (x : Ast.expectation) : bool =
  match x with
  | XAlive _ | XDead _ | XCell _ -> true
  | XLength _ | XCycles _ | XOverhead _ | XBoot _ | XStep _ | XCoversCore -> false

let rec compile ~(read : string -> string option) (args : string list) : output =
  let report, optimize, file, warn, emit_beh, warnings, hill = parse_args args in
  let f = match file with Some f -> f | None -> raise (Stop { out = usage ^ "\n"; err = ""; code = 0; files = [] }) in
  (* A user's error says where (file:line:column when the node is known); an impossible state
     inside the compiler is an internal error, exit 2, so it is not mistaken for the user's. *)
  try compile_file ~read f report optimize warn emit_beh warnings hill with
  | Ast.Error (Some l, msg) -> stop (sprintf "%s:%d:%d: error: %s" f l.line l.col msg)
  | Ast.Error (None, msg) -> stop (sprintf "%s: error: %s" f msg)
  | Failure msg -> stop ~code:2 (sprintf "%s: internal error: %s" f msg)

and compile_file ~read f report optimize warn emit_beh warn_mode hill_flag : output =
  let text = match read f with Some t -> t | None -> stop (sprintf "error: no such file: %s" f) in
  let sexp = Parse.sexp_from_string text in
  let src = Parse.parse_source ~file:f ~read sexp in
  let names = match optimize with
    | Some names -> names
    | None -> Option.value src.optimize ~default:(List.map Metrics.string_of_objective Metrics.default_policy) in
  let policy = List.map (fun n -> match Metrics.objective_of_string n with
    | Some o -> o
    | None -> stop (sprintf "unknown objective `%s`: one of speed, size, stealth, boot" n)) names in
  (* The policy picks the transformations (Optimize.choose): what is printed, measured and checked
     is the chosen variant. *)
  (* The hill: the command line's, else the header's, else 94b, without an ;assert. *)
  let named = match hill_flag with Some h -> Some h | None -> src.hill in
  let hill = Option.map (fun k -> match Hill.find k with
    | Some h -> h
    | None -> stop (sprintf "%s: error: unknown hill `%s`: one of %s" f k Hill.keys)) named in
  let target = Option.value hill ~default:Hill.default in
  let maxlength = target.length in
  let variants = Optimize.measure_all ~consts:src.consts ~coresize:target.coresize ?start:src.start src.body in
  let opts, _, chosen = Optimize.pick policy variants in
  (* A variant faster than the chosen one is one the policy declined for an objective it ranks
     higher: only then is a loop's extra control instruction worth a warning. *)
  let faster = List.exists (fun (_, _, m) -> Metrics.compare [Metrics.Speed] m chosen < 0) variants in
  let metrics () = chosen in
  let tagged = Ast.tag_expr src.body in
  let located = List.map (fun (x, l) -> ((x, None), l)) src.expects
                @ List.map (fun (x, l) -> (x, Some l)) (Expect.collect_located tagged) in
  let expects = List.map fst located in
  (* A broken expectation says where it is written, as every error and warning does. *)
  let at kind l msg = match l with
    | Some (l : Ast.loc) -> sprintf "%s:%d:%d: %s: %s" f l.line l.col kind msg
    | None -> sprintf "%s: %s: %s" f kind msg in
  let failures = List.filter_map (fun (x, l) -> match Expect.check (metrics ()) x with
    | Some (Expect.Fail msg) -> Some (l, msg)
    | Some Expect.Pass | None -> None) located in
  if failures <> [] && not warn then stop (String.concat "\n" (List.map (fun (l, m) -> at "error" l m) failures)) ;
  (* Warnings on what the chosen program costs (Warnings.check), where it comes from. *)
  let costs = List.map (fun (w : Warnings.warning) -> match w.loc with
    | Some l -> sprintf "%s:%d:%d: warning: %s\n" f l.line l.col w.message
    | None -> sprintf "%s: warning: %s\n" f w.message)
      (Warnings.check ~mode:warn_mode ~policy ~expects ~faster (metrics ()) tagged) in
  let warnings = String.concat "" costs ^ String.concat "" (List.map (fun (l, m) -> at "warning" l m ^ "\n") failures) in
  let redcode = Compile.compile_prog ~opts ~consts:src.consts ?hill ~meta:src.meta ?start:src.start src.body ^ "\n" in
  let files = match emit_beh with
    | None -> []
    | Some path ->
      (* The spec names its redcode beside it: a .beh path keeps the two apart. *)
      if not (Filename.check_suffix path ".beh") then stop "--emit-beh FILE must end in .beh" ;
      let probes = List.filter execution (List.map fst expects) in
      if probes = [] then stop "--emit-beh: the program has no (alive N), (dead N) or (cell ...) expectation to export" ;
      let red = Filename.remove_extension path ^ ".red" in
      let hill = Option.map (fun (h : Hill.t) -> h.key) hill in
      [(red, redcode); (path, Expect.to_beh ?hill ~redcode:(Filename.basename red) probes)] in
  match report with
  | Json -> { out = Metrics.to_json ~policy ~optimizations:(Optimize.names opts) (metrics ()) ^ "\n"; err = warnings; code = 0; files }
  | Text ->
    let r = sprintf "%spolicy: %s\noptimizations: %s\n" (Metrics.to_text ~maxlength (metrics ()))
        (String.concat " > " (List.map Metrics.string_of_objective policy)) (Optimize.describe opts) in
    { out = redcode; err = warnings ^ r; code = 0; files }
  | No_report -> { out = redcode; err = warnings; code = 0; files }

let run ~(read : string -> string option) (args : string list) : output =
  try compile ~read args with Stop o -> o
