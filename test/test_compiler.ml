open! Middle_end
open! Syntax
open! Driver

let output_dir =
  let name = "testcases" in
  if not (Sys.file_exists name) then Sys.mkdir name 0o755;
  name

let test str test_name =
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
