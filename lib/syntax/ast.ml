type e =
  | Value of value
  | Lam of string * e
  | App of e * e
  | Prim1 of op1 * e
  | Prim2 of op2 * e * e
  | If of e * e * e
  | Let of string * e * e
  | Letrec of (string * e) list * e
  | Tuple of e list
  | Get of e * int
[@@deriving show { with_path = false }]

and value = Int of int | Bool of bool | Var of string
and op1 = Neg | Not
and op2 = Plus | Minus | Times | And | Or | Eq | Less

module Gensym = struct
  let counter = ref 0
  let init () = counter := 0

  let fresh s =
    let id = !counter in
    counter := id + 1;
    s ^ "." ^ string_of_int id
end

let uniquify e =
  let module Env = Map.Make (String) in
  let rec go (e : e) (env : string Env.t) : e =
    match (e : e) with
    | Value v ->
        Value
          (match v with Int _ | Bool _ -> v | Var x -> Var (Env.find x env))
    | Lam (x, e) ->
        let x' = Gensym.fresh x in
        let env' = Env.add x x' env in
        Lam (x', go e env')
    | App (e1, e2) -> App (go e1 env, go e2 env)
    | Prim1 (op, e) -> Prim1 (op, go e env)
    | Prim2 (op, e1, e2) -> Prim2 (op, go e1 env, go e2 env)
    | If (e1, e2, e3) -> If (go e1 env, go e2 env, go e3 env)
    | Let (x, e1, e2) ->
        let x' = Gensym.fresh x in
        let env' = Env.add x x' env in
        Let (x', go e1 env, go e2 env')
    | Letrec (bindings, e) ->
        let binding_list =
          List.map (fun (x, _) -> (x, Gensym.fresh x)) bindings
        in
        let binding_seq = List.to_seq binding_list in
        let env' = Env.add_seq binding_seq env in
        Letrec
          ( List.map (fun (x, e) -> (Env.find x env', go e env')) bindings,
            go e env' )
    | Tuple exprs -> Tuple (List.map (fun e -> go e env) exprs)
    | Get (e, i) -> Get (go e env, i)
  in
  go e Env.empty
