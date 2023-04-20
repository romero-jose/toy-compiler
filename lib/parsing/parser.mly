%{
  open Syntax
  open Ast
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
%token AND OR
%token IF THEN ELSE
%token FUN
%token LPAREN RPAREN
%token LET IN
%token EOF

%nonassoc ELSE

%right OR
%right AND
%left EQUAL
%left LESS
%left ARROW
%left PLUS MINUS
%left TIMES

%nonassoc NOT
%nonassoc IN

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
  | LET x = ID EQUAL e1 = expr IN e2 = expr { Let (x, e1, e2) }

%inline op1:
  | MINUS {Neg}
  | NOT {Not}

%inline op2:
  | PLUS {Plus}
  | MINUS {Minus}
  | TIMES {Times}
  | EQUAL {Eq}
  | LESS {Less}
  | AND {And}
  | OR {Or}

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
  | LPAREN e = expr RPAREN	
    { e }    