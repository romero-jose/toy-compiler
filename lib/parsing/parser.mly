%{
  open Syntax
  open Ast

  type bindings =
  | Binding of (string * e)
  | RecBindings of (string * e) list

  let expr_of_let_bindings (b : bindings) (e2 : e) =
  match b with
  | Binding (id, e1) -> Let (id, e1, e2)
  | RecBindings l -> Letrec (l, e2)

%}

%token ARROW
%token <string> ID
%token <int> INT
%token TRUE FALSE
%token NOT
%token PLUS
%token MINUS
%token TIMES
%token EQUAL LESS
%token AMPERSANDAMPERSAND BARBAR
%token AND
%token IF THEN ELSE
%token FUN
%token LPAREN RPAREN
%token LET REC IN
%token COMMA
%token DOT
%token EOF

%nonassoc IN
%nonassoc ELSE

%nonassoc below_COMMA
%left COMMA
%right BARBAR
%right AMPERSANDAMPERSAND
%left EQUAL LESS
%left ARROW
%left PLUS MINUS
%left TIMES

%nonassoc NOT

%start file
%type <e option> file
%type <e> expr
%type <e> simple_expr
%type <e> app_expr

%%

file:
  | EOF
    { None }
  | e = expr EOF
    { Some e }

expr:
  | e = app_expr
    { e }
  | op = op1 e = expr
    { Prim1 (op, e) }
  | e1 = expr op = op2 e2 = expr	
    { Prim2 (op, e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { If (e1, e2, e3) }
  | FUN x = ID ARROW e = expr
    { Lam (x, e) }
  | lbs = let_bindings e = expr { expr_of_let_bindings lbs e }
  | exprs = expr_comma_list %prec below_COMMA { Tuple (exprs) }

%inline op1:
  | MINUS {Neg}
  | NOT {Not}

%inline op2:
  | PLUS {Plus}
  | MINUS {Minus}
  | TIMES {Times}
  | EQUAL {Eq}
  | LESS {Less}
  | AMPERSANDAMPERSAND {And}
  | BARBAR {Or}

app_expr:
  | e = simple_expr
    { e }
  | e1 = app_expr e2 = simple_expr
    { App (e1, e2) }

simple_expr:
  | x = ID
    { Value (Var x) }
  | TRUE    
    { Value (Bool true) }
  | FALSE
    { Value (Bool false) }
  | n = INT
    { Value (Int n) }
  | e = simple_expr DOT LPAREN n = INT RPAREN{ Get (e, n) }
  | LPAREN e = expr RPAREN	
    { e }

%inline let_bindings:
  | LET b = binding IN { Binding (fst b, snd b) }
  | LET REC bs = and_bindings IN { RecBindings bs }

%inline binding:
  | id = ID EQUAL e = expr { (id, e) }

and_bindings:
  | b = binding { [b] }
  | b = binding AND bs = and_bindings { b :: bs }

(* copied from the OCaml parser *)
%inline expr_comma_list:
  es = separated_nontrivial_llist(COMMA, expr) { es }

reversed_separated_nontrivial_llist(separator, X):
  xs = reversed_separated_nontrivial_llist(separator, X)
  separator
  x = X
    { x :: xs }
| x1 = X
  separator
  x2 = X
    { [ x2; x1 ] }

%inline separated_nontrivial_llist(separator, X):
  xs = rev(reversed_separated_nontrivial_llist(separator, X))
    { xs }
