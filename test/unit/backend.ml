open Alcotest
open Backend
open Ast

(** Helper functions for converting to strings *)

let core_type_to_string (ct : Parsetree.core_type) : string =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  Ppxlib.Pprintast.core_type fmt ct;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let expression_to_string (e : Parsetree.expression) : string =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  Ppxlib.Pprintast.expression fmt e;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(* Testable types for Alcotest *)
let parsetree_core_type : Parsetree.core_type testable =
  testable
    (fun fmt ct ->
      Format.fprintf fmt "%a" Ppxlib.Pprintast.core_type ct)
    (fun a b -> core_type_to_string a = core_type_to_string b)

let parsetree_expression : Parsetree.expression testable =
  testable
    (fun fmt e ->
      Format.fprintf fmt "%a" Ppxlib.Pprintast.expression e)
    (fun a b -> expression_to_string a = expression_to_string b)

(** Test cases for process_type_value *)

let test_process_type_var () =
  let input = TypVar (IdxVar "a") in
  let result = process_type_value input in
  let expected_str = "'a" in
  check string "type variable 'a"
    expected_str
    (core_type_to_string result)

let test_process_type_var_equality () =
  let input = TypVar (IdxVar "'eq") in
  let result = process_type_value input in
  let expected_str = "''eq" in
  check string "equality type variable ''eq"
    expected_str
    (core_type_to_string result)

let test_process_type_con_int () =
  let input = TypCon ([], IdxIdx "int") in
  let result = process_type_value input in
  check string "simple type constructor 'int'"
    "int"
    (core_type_to_string result)

let test_process_type_con_list () =
  let input = TypCon ([TypVar (IdxVar "a")], IdxIdx "list") in
  let result = process_type_value input in
  (* OCaml renders this as 'a list *)
  check (bool) "type constructor with single arg"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_type_con_either () =
  let input = TypCon (
    [TypVar (IdxVar "a"); TypVar (IdxVar "b")],
    IdxIdx "either"
  ) in
  let result = process_type_value input in
  check (bool) "type constructor with multiple args"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_type_par () =
  let input = TypPar (TypVar (IdxVar "a")) in
  let result = process_type_value input in
  let expected_str = "'a" in
  check string "parenthesized type"
    expected_str
    (core_type_to_string result)

let test_process_type_fun () =
  let input = TypFun (
    TypVar (IdxVar "a"),
    TypVar (IdxVar "b")
  ) in
  let result = process_type_value input in
  let result_str = core_type_to_string result in
  check (bool) "function type contains arrow"
    true
    (String.contains result_str '>')

let test_process_type_fun_nested () =
  let input = TypFun (
    TypVar (IdxVar "a"),
    TypFun (TypVar (IdxVar "b"), TypVar (IdxVar "c"))
  ) in
  let result = process_type_value input in
  check (bool) "nested function type"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_type_tuple () =
  let input = TypTuple [
    TypVar (IdxVar "a");
    TypVar (IdxVar "b");
  ] in
  let result = process_type_value input in
  let result_str = core_type_to_string result in
  check (bool) "tuple type contains asterisk"
    true
    (String.contains result_str '*')

let test_process_type_tuple_three () =
  let input = TypTuple [
    TypVar (IdxVar "a");
    TypVar (IdxVar "b");
    TypVar (IdxVar "c");
  ] in
  let result = process_type_value input in
  check (bool) "three-element tuple type"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_type_record_single () =
  let input = TypRecord [
    TypRow (IdxLab "name", TypCon ([], IdxIdx "string"), None)
  ] in
  let result = process_type_value input in
  let result_str = core_type_to_string result in
  check (bool) "record type contains angle bracket"
    true
    (String.contains result_str '<')

let test_process_type_record_multiple () =
  let input = TypRecord [
    TypRow (
      IdxLab "name",
      TypCon ([], IdxIdx "string"),
      Some (TypRow (IdxLab "age", TypCon ([], IdxIdx "int"), None))
    )
  ] in
  let result = process_type_value input in
  check (bool) "record type with multiple fields"
    true
    (String.length (core_type_to_string result) > 0)

(** Test cases for process_object_field_type *)

let test_process_object_field_simple () =
  let input = TypRow (IdxLab "x", TypCon ([], IdxIdx "int"), None) in
  let result = process_object_field_type input in
  check (int) "single object field"
    1
    (List.length result)

let test_process_object_field_multiple () =
  let input = TypRow (
    IdxLab "x",
    TypCon ([], IdxIdx "int"),
    Some (TypRow (IdxLab "y", TypCon ([], IdxIdx "string"), None))
  ) in
  let result = process_object_field_type input in
  check (int) "multiple object fields"
    2
    (List.length result)

(** Test cases for process_type (wrapper) *)

let test_process_type_wrapper () =
  let input = TypVar (IdxVar "t") in
  let result = process_type input in
  let result_str = core_type_to_string result in
  check string "process_type wrapper"
    "'t"
    result_str

(** Test cases for process_exp *)

let test_process_exp_idx () =
  let input = ExpIdx (IdxIdx "x") in
  let result = process_exp input in
  let result_str = expression_to_string result in
  check (bool) "expression from identifier"
    true
    (String.contains result_str 'x')

let test_process_exp_app () =
  let input = ExpApp (
    ExpIdx (IdxIdx "f"),
    ExpIdx (IdxIdx "x")
  ) in
  let result = process_exp input in
  check (bool) "function application expression"
    true
    (String.length (expression_to_string result) > 0)

let test_process_exp_infix () =
  let input = InfixApp (
    ExpIdx (IdxIdx "x"),
    IdxIdx "+",
    ExpIdx (IdxIdx "y")
  ) in
  let result = process_exp input in
  let result_str = expression_to_string result in
  check (bool) "infix application expression"
    true
    (String.length result_str > 0)

(** Test cases for complex types *)

let test_process_complex_function_type () =
  (* (int * string) -> bool *)
  let input = TypFun (
    TypTuple [TypCon ([], IdxIdx "int"); TypCon ([], IdxIdx "string")],
    TypCon ([], IdxIdx "bool")
  ) in
  let result = process_type_value input in
  check (bool) "complex function type"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_list_type () =
  (* 'a list -> int list *)
  let input = TypFun (
    TypCon ([TypVar (IdxVar "a")], IdxIdx "list"),
    TypCon ([TypCon ([], IdxIdx "int")], IdxIdx "list")
  ) in
  let result = process_type_value input in
  check (bool) "list type transformation"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_higher_order_function () =
  (* ('a -> 'b) -> 'a list -> 'b list *)
  let input = TypFun (
    TypFun (TypVar (IdxVar "a"), TypVar (IdxVar "b")),
    TypFun (
      TypCon ([TypVar (IdxVar "a")], IdxIdx "list"),
      TypCon ([TypVar (IdxVar "b")], IdxIdx "list")
    )
  ) in
  let result = process_type_value input in
  check (bool) "higher-order function type"
    true
    (String.length (core_type_to_string result) > 0)

(** Test suite organization *)

let type_tests = [
  "process type variable", `Quick, test_process_type_var;
  "process equality type variable", `Quick, test_process_type_var_equality;
  "process simple type constructor (int)", `Quick, test_process_type_con_int;
  "process type constructor with single arg (list)", `Quick, test_process_type_con_list;
  "process type constructor with multiple args", `Quick, test_process_type_con_either;
  "process parenthesized type", `Quick, test_process_type_par;
  "process function type", `Quick, test_process_type_fun;
  "process nested function type", `Quick, test_process_type_fun_nested;
  "process tuple type (2 elements)", `Quick, test_process_type_tuple;
  "process tuple type (3 elements)", `Quick, test_process_type_tuple_three;
  "process record type (single field)", `Quick, test_process_type_record_single;
  "process record type (multiple fields)", `Quick, test_process_type_record_multiple;
  "process_type wrapper function", `Quick, test_process_type_wrapper;
]

let object_field_tests = [
  "process single object field", `Quick, test_process_object_field_simple;
  "process multiple object fields", `Quick, test_process_object_field_multiple;
]

let expression_tests = [
  "process identifier expression", `Quick, test_process_exp_idx;
  "process function application", `Quick, test_process_exp_app;
  "process infix application", `Quick, test_process_exp_infix;
]

let complex_type_tests = [
  "process complex function type (tuple -> bool)", `Quick, test_process_complex_function_type;
  "process list type transformation", `Quick, test_process_list_type;
  "process higher-order function type", `Quick, test_process_higher_order_function;
]

(** Main test runner *)

let () =
  run "Backend" [
    "Type Processing", type_tests;
    "Object Field Processing", object_field_tests;
    "Expression Processing", expression_tests;
    "Complex Type Processing", complex_type_tests;
  ]
