open Syntax
open Middle_end

let compile_e e output_file =
  let e = Ast.uniquify e in
  let anf = Anf.trans e in
  let anf = Anf.beta_reduce anf in
  let anf = Anf.remove_unused_let_bindings anf in
  let program = Closure.closure anf in
  let _ = Codegen.codegen_program program in
  Llvm.print_module output_file Codegen.the_module

let compile_string str output_file =
  let e = Option.get (Parsing.Driver.parse_string str) in
  compile_e e output_file

let compile_file input_file output_file =
  let e = Option.get (Parsing.Driver.parse_file input_file) in
  compile_e e output_file

let compile_stdin output_file =
  let e = Option.get (Parsing.Driver.parse_stdin ()) in
  compile_e e output_file

let run_file (filename : string) =
  let cmd = Filename.quote_command "lli" [ filename ] in
  let _ = Sys.command cmd in
  ()
