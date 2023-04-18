exception Eof

open Parser

let lexeme = Sedlexing.Utf8.lexeme

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let lower = [%sedlex.regexp? 'a' .. 'z']
let upper = [%sedlex.regexp? 'A' .. 'Z']
let alpha = [%sedlex.regexp? lower | upper]
let id0 = [%sedlex.regexp? alpha | '_']
let id = [%sedlex.regexp? id0, Star (alpha | digit | '_')]
let whitespace = [%sedlex.regexp? Plus ('\t' | ' ')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let lparen = [%sedlex.regexp? '(']
let rparen = [%sedlex.regexp? ')']
let dot = [%sedlex.regexp? '.']
let plus = [%sedlex.regexp? '+']
let minus = [%sedlex.regexp? '-']
let times = [%sedlex.regexp? '*']
let equal = [%sedlex.regexp? '=']
let not = [%sedlex.regexp? '!']
let less = [%sedlex.regexp? '<']
let arrow = [%sedlex.regexp? "->"]
let if_ = [%sedlex.regexp? "if"]
let then_ = [%sedlex.regexp? "then"]
let else_ = [%sedlex.regexp? "else"]
let fun_ = [%sedlex.regexp? "fun"]
let let_ = [%sedlex.regexp? "let"]
let in_ = [%sedlex.regexp? "in"]

let rec token lexbuf =
  match%sedlex lexbuf with
  | lparen -> LPAREN
  | rparen -> RPAREN
  | plus -> PLUS
  | minus -> MINUS
  | times -> TIMES
  | less -> LESS
  | not -> NOT
  | equal -> EQUAL
  | arrow -> ARROW
  | if_ -> IF
  | then_ -> THEN
  | else_ -> ELSE
  | fun_ -> FUN
  | let_ -> LET
  | in_ -> IN
  | id -> ID (Sedlexing.Utf8.lexeme lexbuf)
  | number -> INT (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
  | whitespace -> token lexbuf
  | newline ->
      Sedlexing.new_line lexbuf;
      token lexbuf
  | eof -> EOF
  | _ ->
    Format.eprintf "invalid token '%s'\n%!" (Sedlexing.Utf8.lexeme lexbuf);
    Format.eprintf "length %d\n%!" (String.length (Sedlexing.Utf8.lexeme lexbuf));
      exit 1
