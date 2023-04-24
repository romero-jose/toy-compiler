open Syntax
open Middle_end
open Common

let compile_e ?(verbose = false) e output_file =
  let log t =
    if verbose then Format.eprintf t else Format.ifprintf Format.err_formatter t
  in
  log "%a\n\n%!" Ast.pp_e e;
  log "Uniquifying\n%!";
  let e = Ast.uniquify e in
  log "%a\n\n%!" Ast.pp_e e;
  log "Transforming to ANF\n%!";
  let anf = Anf.trans e in
  log "%a\n\n%!" Anf.pp_expr anf;
  log "Beta reducing\n%!";
  let anf = Anf.beta_reduce anf in
  log "%a\n\n%!" Anf.pp_expr anf;
  log "Removing unused let bindings\n%!";
  let anf = Anf.remove_unused_let_bindings anf in
  log "%a\n\n%!" Anf.pp_expr anf;
  log "Converting closures\n%!";
  let program = Closure.closure anf in
  log "%a\n\n%!" Closure.pp_program program;
  log "Converting to llvm IR\n%!";
  let _ = Codegen.codegen_program program in
  let the_module = Codegen.Module.get () in
  Llvm.print_module output_file the_module

let try_compile ?(verbose = false) parse_func output_file =
  try
    Codegen.Module.init "main";
    let e = parse_func () in
    compile_e e output_file ~verbose
  with Error.Error e ->
    Format.eprintf "%a\n" Error.pp_loc e;
    exit 1

let compile_string ?(verbose = false) str output_file =
  try_compile ~verbose
    (fun () -> Option.get (Parsing.Driver.parse_string str))
    output_file

let compile_file ?(verbose = false) input_file output_file =
  try_compile ~verbose
    (fun () -> Option.get (Parsing.Driver.parse_file input_file))
    output_file

let compile_stdin ?(verbose = false) output_file =
  try_compile ~verbose
    (fun () -> Option.get (Parsing.Driver.parse_stdin ()))
    output_file

let run_file (filename : string) =
  let cmd = Filename.quote_command "lli" [ filename ] in
  let _ = Sys.command cmd in
  ()
