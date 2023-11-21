open Cmdliner
open! Middle_end
open! Common
open! Syntax
open! Format
open! Driver

type verb =
  | Verbose
  | Quiet

let input_file_arg =
  let doc = "The input file to be compiled" in
  let docv = "INPUT_FILE" in
  Arg.(required & pos 0 (some string) None & info [] ~docv ~doc)

let output_file_arg =
  let doc =
    "The output file for the compiled LLVM IR (default: input file after \
     removing the extension)"
  in
  let docv = "OUTPUT_FILE" in
  Arg.(value & opt (some string) None & info [ "o"; "output-file" ] ~docv ~doc)

let verb_t =
  let quiet =
    let doc = "Don't print the steps in the compilation pipeline." in
    (Quiet, Arg.info [ "q"; "quiet"; "silent" ] ~doc)
  in
  let verbose =
    let doc = "Print every step in the compilation pipeline." in
    (Verbose, Arg.info [ "v"; "verbose" ] ~doc)
  in
  Arg.(last & vflag_all [ Quiet ] [ quiet; verbose ])

let typechecking_t =
  let options =
    [
      ("static", Config.Static);
      ("dynamic", Config.Dynamic);
      ("none", Config.Nocheck);
    ]
  in
  let doc =
    Format.sprintf "The typechecking semantics to be used, options are %s"
      (Arg.doc_alts_enum options)
  in
  let docv = "TYPECHECKING" in
  let default = Config.Nocheck in
  Arg.(
    value & opt (enum options) default & info [ "t"; "typechecking" ] ~docv ~doc)

let compile input_file output_file_opt typechecking
    verb =
  let input_stripped = Filename.remove_extension input_file in
  let ir_file =
    match output_file_opt with
    | Some f -> Filename.remove_extension f ^ ".ll"
    | None -> input_stripped ^ ".ll"
  in
  let verbose = match verb with Verbose -> true | Quiet -> false in
  Compile.compile
    Config.
      {
        input = InputFile input_file;
        output = OutputFile ir_file;
        typechecking;
        verbose;
      }

let compiler_t =
  Term.(
    const compile
    $ input_file_arg
    $ output_file_arg
    $ typechecking_t
    $ verb_t)

let compiler_cmd =
  let doc = "A simple compiler" in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "Compiles the program in the input file to LLVM IR and writes the \
         result to the output file.";
      `S "EXAMPLES";
      `P "To compile a file named 'program.txt' and output to 'program.ll':";
      `P "$(b,compiler) program.txt -o program.ll";
    ]
  in
  let info = Cmd.info "toy_compiler" ~version:"0.0.1" ~doc ~man in
  Cmd.v info compiler_t

let () = exit (Cmd.eval compiler_cmd)