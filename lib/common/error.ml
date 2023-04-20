type t = Syntax of syntax
and syntax = UnexpectedToken of string | ParsingError of string

exception Error of (Loc.t * t)

let error loc t = raise (Error (loc, t))

let rec pp fmt = function
  | Syntax syntax -> Format.fprintf fmt "Syntax error, %a" pp_syntax syntax

and pp_syntax fmt = function
  | UnexpectedToken s -> Format.fprintf fmt "unexpected token: '%s'" s
  | ParsingError s -> Format.fprintf fmt "%s" s

and pp_loc fmt ((loc, t) : Loc.t * t) =
  Format.fprintf fmt "In %a:\n%a\n" Loc.pp loc pp t
