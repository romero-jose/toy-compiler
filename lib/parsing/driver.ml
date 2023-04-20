let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.file

let parse_file filename =
  let in_channel = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel in_channel in
  Sedlexing.set_filename lexbuf filename;
  let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
  parser lexer

let parse_stdin () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  Sedlexing.set_filename lexbuf "/dev/stdin";
  let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
  parser lexer

let parse_string str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
  parser lexer
