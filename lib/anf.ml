open Ast

type expr =
  | Ret of cexpr
  | Let of string * cexpr * expr
  | If of atom * expr * expr

and cexpr =
  | Atom of atom
  | App of atom * atom
  | Prim1 of op1 * atom
  | Prim2 of op2 * atom * atom

and atom = Const of const | Var of string | Lam of string * expr
and const = Int of int | Bool of bool [@@deriving show { with_path = false }]

let atom_of_value : Ast.value -> atom = function
  | Int n -> Const (Int n)
  | Bool b -> Const (Bool b)
  | Var id -> Var id

let rec trans0 : Ast.e -> expr = function
  | Value v -> Ret (Atom (atom_of_value v))
  | Lam (x, e) -> Ret (Atom (Lam (x, trans0 e)))
  | App (e1, e2) ->
      trans1 e1 (fun e1 -> trans1 e2 (fun e2 -> Ret (App (e1, e2))))
  | Prim1 (op, e) -> trans1 e (fun e -> Ret (Prim1 (op, e)))
  | Prim2 (op, e1, e2) ->
      trans1 e1 (fun e1 -> trans1 e2 (fun e2 -> Ret (Prim2 (op, e1, e2))))
  | If (e1, e2, e3) -> trans1 e1 (fun e1 -> If (e1, trans0 e2, trans0 e3))
  | Let (x, e1, e2) -> trans1 e1 (fun e1 -> Let (x, Atom e1, trans0 e2))

and trans1 (e : Ast.e) (k : atom -> expr) : expr =
  match e with
  | Value v -> k (atom_of_value v)
  | Lam (x, e) -> k (Lam (x, trans0 e))
  | App (e1, e2) ->
      let name = Gensym.fresh "app" in
      trans1 e1 (fun e1 ->
          trans1 e2 (fun e2 -> Let (name, App (e1, e2), k (Var name))))
  | Prim1 (op, e) ->
      let name = Gensym.fresh "prim1" in
      trans1 e (fun e -> Let (name, Prim1 (op, e), k (Var name)))
  | Prim2 (op, e1, e2) ->
      let name = Gensym.fresh "prim2" in
      trans1 e1 (fun e1 ->
          trans1 e2 (fun e2 -> Let (name, Prim2 (op, e1, e2), k (Var name))))
  | If (e1, e2, e3) ->
      trans1 e1 (fun e1 ->
          If (e1, trans1 e2 (fun e2 -> k e2), trans1 e3 (fun e3 -> k e3)))
  | Let (x, e1, e2) ->
      trans1 e1 (fun e1 -> Let (x, Atom e1, trans1 e2 (fun e2 -> k e2)))

let trans : Ast.e -> expr =
  Gensym.init ();
  trans0

module StringSet = Set.Make (String)

let rec free_vars e = StringSet.elements (free_vars_e e)

and free_vars_e e =
  match e with
  | Ret c -> free_vars_c c
  | Let (id, c, e) ->
      StringSet.union (free_vars_c c)
        (StringSet.diff (free_vars_e e) (StringSet.singleton id))
  | If (a, e1, e2) ->
      StringSet.union (free_vars_a a)
        (StringSet.union (free_vars_e e1) (free_vars_e e2))

and free_vars_c c =
  match c with
  | Atom a -> free_vars_a a
  | App (a1, a2) -> StringSet.union (free_vars_a a1) (free_vars_a a2)
  | Prim1 (_, a) -> free_vars_a a
  | Prim2 (_, a1, a2) -> StringSet.union (free_vars_a a1) (free_vars_a a2)

and free_vars_a a =
  match a with
  | Const _ -> StringSet.empty
  | Var x -> StringSet.singleton x
  | Lam (id, e) -> StringSet.diff (free_vars_e e) (StringSet.singleton id)
