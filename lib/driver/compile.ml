open Syntax
open Common

let log (config : Config.t) fmt =
  if config.verbose then Format.eprintf fmt
  else Format.ifprintf Format.err_formatter fmt

let parse (conf : Config.t) =
  log conf "Parsing\n%!";
  let e_opt =
    match conf.input with
    | Stdin -> Parsing.Driver.parse_stdin ()
    | InputFile filename -> Parsing.Driver.parse_file filename
    | InputString str -> Parsing.Driver.parse_string str
  in
  let e = Option.get e_opt in
  log conf "%a\n\n%!" Typed_ast.pp_e e;
  e

let typecheck (conf : Config.t) typed_e =
  match conf.typechecking with
  | Static ->
      log conf "Typechecking\n%!";
      Typechecking.Static.typecheck typed_e;
      typed_e
  | _ -> typed_e

let erase_types conf typed_e =
  log conf "Erasing types\n%!";
  Ast_typed.to_ast typed_e

let uniquify conf e =
  log conf "Uniquifying\n%!";
  let t = Uniquify.uniquify e in
  log conf "%a\n\n%!" Ast.pp t;
  t

let analyze_store conf e =
  log conf "Transforming to Stored AST\n%!";
  let stored_e = Analyze.resolve_store e in
  log conf "%a\n\n%!" Storify.pp_expr stored_e;
  stored_e

let closure_convert conf stored_e =
  log conf "Converting closures\n%!";
  let program = Closure.closure stored_e in
  log conf "%a\n\n%!" Closure.pp_program program;
  program

let codegen conf _ =
  log conf "Converting to llvm IR\n%!";
  match conf.output with
  | OutputFile _ -> failwith "TODO"
  | Stderr -> failwith "TODO"

let init_codegen : Config.t -> unit =
  let counter =
    let count = ref 0 in
    fun () ->
      let c = !count in
      count := c + 1;
      c
  in
  fun config ->
    let module_name =
      match config.input with
      | InputFile filename ->
          filename |> Filename.basename |> Filename.remove_extension
      | Stdin | InputString _ ->
          (* hack for allowing compilation of multiple modules in the same session *)
          Format.sprintf "main_%d" (counter ())
    in
    let assertions =
      match config.typechecking with
      | Static | Nocheck -> false
      | Dynamic -> true
    in
    let tagged =
      match config.typechecking with
      | Static | Nocheck -> false
      | Dynamic -> true
    in
    Middle_end.Codegen.Config.init module_name assertions tagged

let compile (config : Config.t) =
  try
    init_codegen config;
    config
    |> parse
    |> typecheck config
    |> erase_types config
    |> uniquify config
    |> store_transform config
    |> closure_convert config
    |> codegen config
  with Error.Error e ->
    Format.eprintf "%a\n" Error.pp_loc e;
    exit 1
