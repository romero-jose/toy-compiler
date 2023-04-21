open Common
open Llvm
open Syntax
open Closure

type proto = Prototype of string * (string * lltype) list * lltype

let () = Llvm.enable_pretty_stacktrace ()

exception Error of string

let context = global_context ()

module Type = struct
  let i8 = Llvm.i8_type context
  let i32 = Llvm.i32_type context
  let i64 = Llvm.i64_type context
  let star = Llvm.pointer_type i64
  let array size = Llvm.array_type i64 size
  let array_ptr size = Llvm.pointer_type (array size)
  let closure_ptr = array_ptr

  let func args =
    let ret_type = i64 in
    let self_type = star in
    let arg_types = List.map (fun _ -> i64) args in
    Llvm.function_type ret_type (Array.of_list (self_type :: arg_types))

  let func_ptr args = Llvm.pointer_type (func args)
end

module Module = struct
  let _module : llmodule option ref = ref None
  let init name = _module := Some (create_module context name)

  let get : unit -> llmodule =
   fun () ->
    match !_module with
    | None ->
        Error.error
          (Codegen
             (UninitializedModule
                "tried to use LLVM module before it has been initialized"))
    | Some m -> m
end

let builder = builder context
let named_values = Hashtbl.create 32
let true_val = const_int Type.i64 1
let false_val = const_int Type.i64 0

module Print = struct
  let build_print (format : string) (arg : llvalue) =
    let the_module = Module.get () in
    let printf_func =
      let printf_type =
        var_arg_function_type (i32_type context) [| pointer_type Type.i8 |]
      in
      declare_function "printf" printf_type the_module
    in
    let format_str = build_global_stringptr format "format_str" builder in
    build_call printf_func [| format_str; arg |] "printf_call" builder

  let build_print_i64 (arg : llvalue) = build_print "%lld\n" arg
end

let build_store_to_array array_pointer (index : int) value =
  let zero = const_int Type.i64 0 in
  let idx = const_int Type.i64 index in
  let indices = [| zero; idx |] in
  let ptr = build_gep array_pointer indices "array_element_ptr" builder in
  build_store value ptr builder

let build_load_from_array array_pointer (index : int) =
  let zero = const_int Type.i64 0 in
  let idx = const_int Type.i64 index in
  let indices = [| zero; idx |] in
  let ptr = build_gep array_pointer indices "array_element_ptr" builder in
  build_load ptr "array_element" builder

let verify_function f =
  try
    (match Llvm_analysis.verify_function f with
    | true -> ()
    | false ->
        Format.printf "invalid function generated\n%s\n%!"
          (Llvm.string_of_llvalue f);
        Llvm_analysis.assert_valid_function f);
    f
  with e ->
    delete_function f;
    raise e

let declare_function name f_type args =
  let the_module = Module.get () in
  match lookup_function name the_module with
  | None -> declare_function name f_type the_module
  | Some f ->
      if Array.length (basic_blocks f) == 0 then ()
      else raise (Error "redefinition of function");

      if Array.length (params f) == List.length args then ()
      else raise (Error "redefinition of function with different # args");
      f

let load_var_from_env env idx id =
  let idx_v = const_int Type.i64 (idx + 1) in
  let name = Format.sprintf "%s" id in
  let var_ptr = build_gep env [| idx_v |] name builder in
  build_load var_ptr name builder

let rec codegen_program (program : program) =
  let the_module = Module.get () in
  match program with
  | LetFun (func, program') ->
      let f = codegen_func func in
      f :: codegen_program program'
  | Expr e ->
      let func_main_ =
        codegen_raw_function "main_" [] Type.i64 (fun _ ->
            let v = codegen_expr e in
            v)
      in
      let func_main =
        codegen_raw_function "main" [] Type.i32 (fun _ ->
            let v = build_call func_main_ [||] "return" builder in
            let _ = Print.build_print_i64 v in
            let ret = const_int Type.i32 0 in
            ret)
      in
      let function_list = [ func_main_; func_main ] in
      Llvm_analysis.assert_valid_module the_module;
      function_list

and codegen_expr (expr : expr) : llvalue =
  match expr with
  | Let (id, cexpr, expr) ->
      let v = codegen_cexpr cexpr in
      Hashtbl.add named_values id v;
      codegen_expr expr
  | If (a, e1, e2) ->
      (* cond *)
      let cond = codegen_atom a in
      let false_value = const_int Type.i64 0 in
      let cond_val = build_icmp Icmp.Ne cond false_value "cond" builder in
      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in
      (* then *)
      let then_bb = append_block context "then" the_function in
      position_at_end then_bb builder;
      let then_val = codegen_expr e1 in
      let new_then_bb = insertion_block builder in
      (* else *)
      let else_bb = append_block context "else" the_function in
      position_at_end else_bb builder;
      let else_val = codegen_expr e2 in
      let new_else_bb = insertion_block builder in
      (* merge *)
      let merge_bb = append_block context "if_join" the_function in
      position_at_end merge_bb builder;
      let incoming = [ (then_val, new_then_bb); (else_val, new_else_bb) ] in
      let phi = build_phi incoming "iftmp" builder in
      (* return to start block *)
      position_at_end start_bb builder;
      ignore (build_cond_br cond_val then_bb else_bb builder);
      (* Set a unconditional branch at the end of the 'then' block and the 'else' block to the 'merge' block. *)
      position_at_end new_then_bb builder;
      ignore (build_br merge_bb builder);
      position_at_end new_else_bb builder;
      ignore (build_br merge_bb builder);
      (* Finally, set the builder to the end of the merge block. *)
      position_at_end merge_bb builder;
      phi
  | Ret cexpr -> codegen_cexpr cexpr

and codegen_const (c : Anf.const) =
  match c with
  | Anf.Int n -> const_int Type.i64 n
  | Anf.Bool b -> const_int Type.i64 (match b with false -> 0 | true -> 1)

and codegen_atom (atom : atom) =
  match atom with
  | Const c -> codegen_const c
  | Var id -> (
      match Hashtbl.find_opt named_values id with
      | Some v -> v
      | None -> failwith (Format.sprintf "codegen: variable '%s' not found" id))
  | Closure (fname, captured_vars) ->
      let the_module = Module.get () in
      let f =
        match lookup_function fname the_module with
        | Some f -> f
        | None ->
            dump_module the_module;
            failwith ("function not defined: " ^ fname)
      in
      (* Allocate memory for the closure *)
      let closure_size = 1 + List.length captured_vars in
      let array_type = Type.array closure_size in
      let closure_ptr = build_malloc array_type "closure_ptr" builder in
      (* Store the function pointer as the first value of the array *)
      let f_cast = build_ptrtoint f Type.i64 "f_cast" builder in
      let _ = build_store_to_array closure_ptr 0 f_cast in
      (* Store the captured variables after the function pointer *)
      let () =
        List.iteri
          (fun i var ->
            let v = codegen_atom (Var var) in
            let idx = i + 1 in
            let _ = build_store_to_array closure_ptr idx v in
            ())
          captured_vars
      in
      (* Cast the closure pointer to an integer for return *)
      let cast_closure =
        Llvm.build_ptrtoint closure_ptr Type.i64 "cast_closure" builder
      in
      cast_closure

and codegen_cexpr (cexpr : cexpr) =
  match cexpr with
  | Atom a -> codegen_atom a
  | Call (f, arg) ->
      let args = [ arg ] in
      let closure = codegen_atom f in
      let cast_closure = build_inttoptr closure Type.star "closure" builder in
      let arg_vals = Array.map codegen_atom (Array.of_list args) in
      let index = const_int Type.i64 0 in
      let f_ptr_ptr = build_gep cast_closure [| index |] "f_ptr_ptr" builder in
      let f_ptr = build_load f_ptr_ptr "f_ptr" builder in
      let f_ptr_type = Type.func_ptr args in
      let f_ptr = build_inttoptr f_ptr f_ptr_type "f_ptr_cast" builder in
      let arg_vals' = Array.append [| cast_closure |] arg_vals in
      build_call f_ptr arg_vals' "return" builder
  | Prim1 (op, a) -> lift_op1 op (codegen_atom a)
  | Prim2 (op, a1, a2) -> lift_op2 op (codegen_atom a1) (codegen_atom a2)
  | Tuple atoms ->
      let array_type = Type.array (List.length atoms) in
      let array_ptr = build_malloc array_type "array" builder in
      List.iteri
        (fun i a ->
          let v = codegen_atom a in
          let _ = build_store_to_array array_ptr i v in
          ())
        atoms;
      (* Cast the array pointer to an integer for return *)
      let cast_closure =
        Llvm.build_ptrtoint array_ptr Type.i64 "cast_array" builder
      in
      cast_closure
  | Get (a, i) ->
      let v = codegen_atom a in
      let v_cast = build_inttoptr v (Type.array_ptr 0) "array_ptr" builder in
      build_load_from_array v_cast i

and lift_op1 op a =
  match op with
  | Ast.Neg -> build_neg a "neg" builder
  | Ast.Not -> build_xor a (const_int Type.i64 1) "not" builder

and lift_op2 op a1 a2 =
  match op with
  | Ast.Plus -> build_add a1 a2 "add" builder
  | Ast.Times -> build_mul a1 a2 "mul" builder
  | Ast.Minus -> build_sub a1 a2 "sub" builder
  | Ast.And -> build_and a1 a2 "and" builder
  | Ast.Or -> build_or a1 a2 "or" builder
  | Ast.Eq ->
      let cmp = build_icmp Icmp.Eq a1 a2 "eq_cmp" builder in
      build_select cmp true_val false_val "eq" builder
  | Ast.Less ->
      let cmp = build_icmp Icmp.Slt a1 a2 "cmp" builder in
      build_select cmp true_val false_val "eq" builder

and codegen_func (func : func) =
  match func with
  | Func { name; args; body; free_vars } ->
      let return_type = Type.i64 in
      let args =
        List.mapi
          (fun i arg -> (arg, if i = 0 then Type.star else Type.i64))
          ("env" :: args)
      in
      let f = codegen_proto (Prototype (name, args, return_type)) in
      (* bind free vars *)
      let env = (params f).(0) in
      let _ =
        List.mapi
          (fun i var ->
            let v = load_var_from_env env i var in
            set_value_name var v;
            Hashtbl.add named_values var v)
          free_vars
      in
      let ret_val = codegen_expr body in
      let _ = build_ret ret_val builder in
      verify_function f

and codegen_raw_function (name : string) (args : (string * lltype) list)
    (return_type : lltype) (body_builder : unit -> llvalue) =
  let f = codegen_proto (Prototype (name, args, return_type)) in
  let ret_val = body_builder () in
  let _ = build_ret ret_val builder in
  verify_function f

and codegen_proto = function
  | Prototype (name, args, return_type) ->
      Hashtbl.clear named_values;
      let arg_types = List.map snd args in
      let arg_names = List.map fst args in
      let f_type = Llvm.function_type return_type (Array.of_list arg_types) in
      let f = declare_function name f_type args in
      let params = params f in
      List.iteri
        (fun i arg ->
          let parameter = Array.get params i in
          set_value_name arg parameter;
          Hashtbl.add named_values arg parameter)
        arg_names;
      let bb = append_block context "entry" f in
      position_at_end bb builder;
      f
