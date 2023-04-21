open! Middle_end
open! Syntax
open! Driver

let output_dir =
  let name = "testcases" in
  if not (Sys.file_exists name) then Sys.mkdir name 0o755;
  name

let test str test_name =
  (* hack for allowing compilation of multiple modules *)
  Codegen.Module.init test_name;
  let file_name = Filename.remove_extension test_name ^ ".ll" in
  let output_file = Filename.concat output_dir file_name in
  Utils.compile_string str output_file;
  Utils.run_file output_file

let%expect_test "addn" =
  test
    {|
   let addn = fun n -> fun m -> n + m in
   let add1 = addn 1 in
   add1 5
   |}
    "addn";
  [%expect {| 6 |}]

let%expect_test "fact" =
  test
    {|
     let f = fun f -> fun n ->
       if n = 0 then
           1
       else
           n * (f f (n - 1))
     in
     f f 5
     |}
    "fact";
  [%expect {| 120 |}]

let%expect_test "tuple" =
  test {|
  (1, 2, 3).(0)
  |} "tuple";
  [%expect {| 1 |}]

let%expect_test "list" =
  test {|
  let hd = fun l -> l.(0) in
  let tl = fun l -> l.(1) in
  let cons = fun a -> fun b -> (a, b) in
  let l = cons 1 (cons 2 (cons 3 (false))) in
  hd (tl (tl l))
  |} "list";
  [%expect {| 3 |}]
