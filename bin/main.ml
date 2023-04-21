open Middle_end
open Common
open Syntax
open Parsing
open Format

let compile (e : Ast.e) =
  printf "%a\n\n%!" Ast.pp_e e;
  printf "Uniquifying\n%!";
  let e = Ast.uniquify e in
  printf "%a\n\n%!" Ast.pp_e e;
  printf "Transforming to ANF\n%!";
  let anf = Anf.trans e in
  printf "%a\n\n%!" Anf.pp_expr anf;
  printf "Beta reducing\n%!";
  let anf = Anf.beta_reduce anf in
  printf "%a\n\n%!" Anf.pp_expr anf;
  printf "Removing unused let bindings\n%!";
  let anf = Anf.remove_unused_let_bindings anf in
  printf "%a\n\n%!" Anf.pp_expr anf;
  printf "Converting closures\n%!";
  let program = Closure.closure anf in
  printf "%a\n\n%!" Closure.pp_program program;
  printf "Converting to llvm IR\n%!";
  Codegen.Module.init "main";
  let funcs = Codegen.codegen_program program in
  let the_module = Codegen.Module.get () in
  Llvm.dump_module the_module;
  funcs

let _ =
  try
    let ast =
      match Driver.parse_stdin () with
      | Some v -> v
      | None ->
          eprintf "Input is empty";
          exit 1
    in
    let _ = compile ast in
    ()
  with Error.Error e -> Format.eprintf "%a\n" Error.pp_loc e; exit 1
