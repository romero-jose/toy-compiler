open Anf

type program = LetFun of func * program | Expr of expr
and func = Func of string * string list * expr

and expr =
  | Ret of cexpr
  | Let of string * cexpr * expr
  | If of atom * expr * expr

and cexpr =
  | Atom of atom
  | Call of atom * atom
  | Prim1 of Ast.op1 * atom
  | Prim2 of Ast.op2 * atom * atom

and atom = Const of const | Var of string | Closure of string * string list
[@@deriving show]

let rec closure (e : Anf.expr) : program =
  closure_e e (fun e -> Expr e)

and closure_e (e : Anf.expr) k : program =
  match e with
  | Ret cexpr -> closure_c cexpr (fun cexpr -> k (Ret cexpr))
  | Let (id, cexpr, expr) ->
      closure_c cexpr (fun cexpr ->
          closure_e expr (fun expr -> k (Let (id, cexpr, expr))))
  | If (a, expr1, expr2) ->
      closure_a a (fun a ->
          closure_e expr1 (fun expr1 ->
              closure_e expr2 (fun expr2 -> k (If (a, expr1, expr2)))))

and closure_a (a : Anf.atom) k : program =
  match a with
  | Anf.Const c -> k (Const c)
  | Anf.Var v -> k (Var v)
  | Anf.Lam (id, e) ->
      let free_vars = Anf.free_vars Anf.(Ret (Atom a)) in
      let function_name = Ast.Gensym.fresh "lambda" in
      let c = Closure (function_name, free_vars) in
      closure_e e (fun e -> LetFun (Func (function_name, [ id ], e), k c))

and closure_c (c : Anf.cexpr) k : program =
  match c with
  | Anf.Atom a -> closure_a a (fun a -> k (Atom a))
  | Anf.App (a1, a2) ->
      closure_a a1 (fun a1 -> closure_a a2 (fun a2 -> k (Call (a1, a2))))
  | Anf.Prim1 (op, a) -> closure_a a (fun a -> k (Prim1 (op, a)))
  | Anf.Prim2 (op, a1, a2) ->
      closure_a a1 (fun a1 -> closure_a a2 (fun a2 -> k (Prim2 (op, a1, a2))))
