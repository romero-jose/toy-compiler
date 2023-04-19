open Toy_compiler
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
  let funcs = Codegen.codegen_program program in
  Llvm.dump_module Codegen.the_module;
  funcs

let _ =
  try
    let lexbuf = Sedlexing.Utf8.from_channel stdin in
    let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
    let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.file in
    printf "Parsing\n%!";
    let ast =
      match parser lexer with
      | Some expr -> expr
      | None ->
          eprintf "File is empty";
          exit 1
    in
    let _ = compile ast in
    ()
  with Lexer.Eof -> exit 0
