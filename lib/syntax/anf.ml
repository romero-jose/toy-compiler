open Ast

type expr =
  | Ret of cexpr
  | Let of string * cexpr * expr
  | Letrec of (string * cexpr) list * expr
  | If of atom * expr * expr

and cexpr =
  | Atom of atom
  | App of atom * atom
  | Prim1 of op1 * atom
  | Prim2 of op2 * atom * atom
  | Tuple of atom list
  | Get of atom * int

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
  | Letrec (bindings, e) ->
      let rec go acc bindings =
        match bindings with
        | [] -> Letrec (List.rev acc, trans0 e)
        | (x, e) :: tl -> trans1 e (fun e -> go ((x, Atom e) :: acc) tl)
      in
      go [] bindings
  | Tuple exprs ->
      let rec go exprs acc =
        match exprs with
        | e :: tl -> trans1 e (fun e -> go tl (e :: acc))
        | [] -> Ret (Tuple (List.rev acc))
      in
      go exprs []
  | Get (e, i) -> trans1 e (fun e -> Ret (Get (e, i)))

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
  | Letrec (bindings, e) ->
      let rec go acc bindings =
        match bindings with
        | [] -> Letrec (List.rev acc, trans1 e (fun e -> k e))
        | (x, e) :: tl -> trans1 e (fun e -> go ((x, Atom e) :: acc) tl)
      in
      go [] bindings
  | Tuple exprs ->
      let rec go exprs acc =
        let name = Gensym.fresh "tuple" in
        match exprs with
        | e :: tl -> trans1 e (fun e -> go tl (e :: acc))
        | [] -> Let (name, Tuple (List.rev acc), k (Var name))
      in
      go exprs []
  | Get (e, i) ->
      let name = Gensym.fresh "name" in
      trans1 e (fun e -> Let (name, Get (e, i), k (Var name)))

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
  | Letrec (bindings, e) ->
      let bound_variables = StringSet.of_list (List.map fst bindings) in
      let exprs = List.map (fun (_, c) -> free_vars_c c) bindings in
      let used_variables : StringSet.t =
        List.fold_left StringSet.union (free_vars_e e) exprs
      in
      StringSet.diff used_variables bound_variables
  | If (a, e1, e2) ->
      StringSet.union (free_vars_a a)
        (StringSet.union (free_vars_e e1) (free_vars_e e2))

and free_vars_c c =
  match c with
  | Atom a -> free_vars_a a
  | App (a1, a2) -> StringSet.union (free_vars_a a1) (free_vars_a a2)
  | Prim1 (_, a) -> free_vars_a a
  | Prim2 (_, a1, a2) -> StringSet.union (free_vars_a a1) (free_vars_a a2)
  | Tuple atoms ->
      List.fold_left StringSet.union StringSet.empty
        (List.map free_vars_a atoms)
  | Get (e, _) -> free_vars_a e

and free_vars_a a =
  match a with
  | Const _ -> StringSet.empty
  | Var x -> StringSet.singleton x
  | Lam (id, e) -> StringSet.diff (free_vars_e e) (StringSet.singleton id)

let rec subst_expr (subst : string * atom) = function
  | Ret cexpr -> Ret (subst_cexpr subst cexpr)
  | Let (x, c, e) -> Let (x, subst_cexpr subst c, subst_expr subst e)
  | Letrec (bindings, e) ->
      let bindings' =
        List.map (fun (x, c) -> (x, subst_cexpr subst c)) bindings
      in
      let e' = subst_expr subst e in
      Letrec (bindings', e')
  | If (c, then_e, else_e) ->
      let cond' = subst_atom subst c in
      let then_expr' = subst_expr subst then_e in
      let else_expr' = subst_expr subst else_e in
      If (cond', then_expr', else_expr')

and subst_cexpr subst = function
  | Atom atom -> Atom (subst_atom subst atom)
  | App (func, arg) -> App (subst_atom subst func, subst_atom subst arg)
  | Prim1 (op, arg) -> Prim1 (op, subst_atom subst arg)
  | Prim2 (op, arg1, arg2) ->
      Prim2 (op, subst_atom subst arg1, subst_atom subst arg2)
  | Tuple atoms -> Tuple (List.map (subst_atom subst) atoms)
  | Get (arg, i) -> Get (subst_atom subst arg, i)

and subst_atom (subst : string * atom) = function
  | Var name when name = fst subst -> snd subst
  | Var name -> Var name
  | Const c -> Const c
  | Lam (arg, body) ->
      if arg = fst subst then Lam (arg, body)
      else Lam (arg, subst_expr subst body)

let rec beta_reduce = function
  | Ret cexpr -> beta_reduce_cexpr cexpr (fun cexpr -> Ret cexpr)
  | Let (name, cexpr, expr) ->
      beta_reduce_cexpr cexpr (fun cexpr -> Let (name, cexpr, beta_reduce expr))
  | Letrec (bindings, expr) ->
      let rec go acc bindings =
        match bindings with
        | [] -> Letrec (List.rev acc, beta_reduce expr)
        | (x, c) :: tl -> beta_reduce_cexpr c (fun c -> go ((x, c) :: acc) tl)
      in
      go [] bindings
  | If (cond, then_expr, else_expr) ->
      If (beta_reduce_atom cond, beta_reduce then_expr, beta_reduce else_expr)

and beta_reduce_cexpr cexpr (k : cexpr -> expr) =
  match cexpr with
  | Atom atom -> k (Atom (beta_reduce_atom atom))
  | App (Lam (arg, body), arg') ->
      let subst = (arg, arg') in
      subst_expr subst body
  | App (func, arg) -> k (App (beta_reduce_atom func, beta_reduce_atom arg))
  | Prim1 (op, arg) -> k (Prim1 (op, beta_reduce_atom arg))
  | Prim2 (op, arg1, arg2) ->
      k (Prim2 (op, beta_reduce_atom arg1, beta_reduce_atom arg2))
  | Tuple atoms -> k (Tuple (List.map beta_reduce_atom atoms))
  | Get (arg, i) -> k (Get (arg, i))

and beta_reduce_atom = function
  | Var _ as v -> v
  | Const _ as c -> c
  | Lam (arg, body) -> (
      match body with
      | Ret (App (f, Var arg')) when arg' = arg -> f
      | _ -> Lam (arg, beta_reduce body))

let remove_unused_let_bindings (e : expr) : expr =
  let rec go_e (e : expr) =
    match e with
    | Ret c -> Ret (go_c c)
    | Let (id, c, body) ->
        let free_vars = free_vars_e body in
        if StringSet.mem id free_vars then Let (id, c, go_e body) else go_e body
    | Letrec (bindings, e) ->
        let free_vars_list = List.map (fun (_, c) -> free_vars_c c) bindings in
        let used_variables : StringSet.t =
          List.fold_left StringSet.union (free_vars_e e) free_vars_list
        in
        let bindings' =
          List.filter (fun (x, _) -> StringSet.mem x used_variables) bindings
        in
        Letrec (bindings', go_e e)
    | If (a, e1, e2) -> If (go_a a, go_e e1, go_e e2)
  and go_c (c : cexpr) : cexpr =
    match (c : cexpr) with
    | Atom a -> Atom (go_a a)
    | App (a1, a2) -> App (go_a a1, go_a a2)
    | Prim1 (op, a) -> Prim1 (op, go_a a)
    | Prim2 (op2, a1, a2) -> Prim2 (op2, go_a a1, go_a a2)
    | Tuple atoms -> Tuple (List.map go_a atoms)
    | Get (a, i) -> Get (go_a a, i)
  and go_a (a : atom) : atom =
    match (a : atom) with
    | Const c -> Const c
    | Var id -> Var id
    | Lam (id, e) -> Lam (id, go_e e)
  in
  go_e e
