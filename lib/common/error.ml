type t = Syntax of syntax | Codegen of codegen
and syntax = UnexpectedToken of string | ParsingError of string
and codegen = UninitializedModule of string

exception Error of (Loc.t option * t)

let error t = raise (Error (None, t))
let error_with_loc t loc = raise (Error (Some loc, t))

let rec pp fmt = function
  | Syntax syntax -> Format.fprintf fmt "Syntax error, %a" pp_syntax syntax
  | Codegen codegen -> Format.fprintf fmt "Codegen error, %a" pp_codegen codegen

and pp_syntax fmt = function
  | UnexpectedToken s -> Format.fprintf fmt "unexpected token: '%s'" s
  | ParsingError s -> Format.fprintf fmt "%s" s

and pp_codegen fmt = function
  | UninitializedModule s -> Format.fprintf fmt "%s" s

and pp_loc fmt ((loc, t) : Loc.t option * t) =
  match loc with
  | Some loc -> Format.fprintf fmt "In %a:\n%a\n" Loc.pp loc pp t
  | None -> Format.fprintf fmt "%a\n" pp t
