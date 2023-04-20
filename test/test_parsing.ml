open Alcotest
open Syntax
open Ast
open Parsing

(*
   type e =
     | Value of value
     | Lam of string * e
     | App of e * e
     | Prim1 of op1 * e
     | Prim2 of op2 * e * e
     | If of e * e * e
     | Let of string * e * e

   and value = Int of int | Bool of bool | Var of string
   and op1 = Neg | Not
   and op2 = Plus | Minus | Times | And | Or | Eq | Less
*)

let parse str =
  match Driver.parse_string str with Some v -> v | None -> assert false

let e : e testable = testable pp_e ( = )
let test_int () = (check e) "same e" (Value (Int 5)) (parse "5")
let test_bool_true () = (check e) "same e" (Value (Bool true)) (parse "true")
let test_bool_false () = (check e) "same e" (Value (Bool false)) (parse "false")

let test_var () =
  (check e) "same e" (Value (Var "identifier")) (parse "identifier")

let test_neg () = (check e) "same e" (Prim1 (Neg, Value (Int 1))) (parse "-1")

let test_not () =
  (check e) "same e" (Prim1 (Not, Value (Bool true))) (parse "!true")

let test_arithmetic () =
  (check e) "same e"
    (Prim2 (Minus, Prim2 (Plus, parse "1", parse "2 * 3"), parse "4"))
    (parse "1 + 2 * 3 - 4")

let test_boolean_arithmetic () =
  (check e) "same e"
    (Prim2
       ( Or,
         Prim2 (Less, parse "1", parse "2"),
         Prim2 (And, Prim2 (Eq, parse "3", parse "4"), Value (Bool false)) ))
    (parse "1 < 2 or 3 = 4 and false")

let test_let_bindings () =
  (check e) "same e"
    (Let
       ( "x",
         parse "1",
         Let ("y", parse "x + 10", Let ("z", parse "x + y * 2", parse "z")) ))
    (parse "let x = 1 in let y = x + 10 in let z = x + y * 2 in z")

let test_if () =
  (check e) "same e"
    (If (parse "true", parse "if false then true else false", parse "false"))
    (parse "if true then if false then true else false else false")

let test_fun () =
  (* Z combinator *)
  let inner =
    Lam ("v", App (App (Value (Var "x"), Value (Var "x")), Value (Var "v")))
  in
  let outer =
    Lam
      ( "f",
        App
          ( Lam ("x", App (Value (Var "f"), inner)),
            Lam ("x", App (Value (Var "f"), inner)) ) )
  in
  (check e) "same e" outer
    (parse
       "fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))")

let test_fibonacci () =
  (check e) "same e"
    (If
       ( parse "n < 2",
         parse "1",
         Prim2
           ( Plus,
             App (Value (Var "fibonacci"), parse "n - 1"),
             App (Value (Var "fibonacci"), parse "n - 2") ) ))
    (parse "if n < 2 then 1 else fibonacci (n - 1) + fibonacci (n - 2)")

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "parse",
        [
          test_case "int" `Quick test_int;
          test_case "true" `Quick test_bool_true;
          test_case "false" `Quick test_bool_false;
          test_case "var" `Quick test_var;
          test_case "neg" `Quick test_neg;
          test_case "not" `Quick test_not;
          test_case "arithmetic" `Quick test_arithmetic;
          test_case "boolean arithmetic" `Quick test_boolean_arithmetic;
          test_case "let bindings" `Quick test_let_bindings;
          test_case "if" `Quick test_if;
          test_case "function" `Quick test_fun;
          test_case "fibonacci" `Quick test_fibonacci;
        ] );
    ]
