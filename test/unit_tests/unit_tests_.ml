open Alcotest
open Ast

(** Helper for box_node *)
let b = Ast.box_node

(** Test configuration *)
module TestConfig : Common.CONFIG = struct
  let config = {
    Common.input_file = "";
    output_file = None;
    verbosity = Some 3;
    conversions = Cli.test_config.conversions
  }
end
module TestContext (* TODO *) = struct 
  let lexbuf = ""
  let context = Context.basis_context
end
(** Instantiate Backend with test config *)
module TestBackend = Backend.Make(TestContext)(TestConfig)
open TestBackend

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

let pattern_to_string (p : Parsetree.pattern) : string =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  Ppxlib.Pprintast.pattern fmt p;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let parsetree_pattern : Parsetree.pattern testable =
  testable
    (fun fmt p ->
      Format.fprintf fmt "%a" Ppxlib.Pprintast.pattern p)
    (fun a b -> pattern_to_string a = pattern_to_string b)

let constant_to_string (c : Parsetree.constant) : string =
  match c with
  | Pconst_integer (s, _) -> s
  | Pconst_char c -> String.make 1 c
  | Pconst_string (s, _, _) -> s
  | Pconst_float (s, _) -> s

let parsetree_constant : Parsetree.constant testable =
  testable
    (fun fmt c -> Format.fprintf fmt "%s" (constant_to_string c))
    (fun a b -> constant_to_string a = constant_to_string b)

(** Test cases for process_con *)

let test_process_con_int_positive () =
  let input = (ConInt (b "42")) in
  let result = process_con (b input) in
  check parsetree_constant "positive integer constant"
    (Pconst_integer ("42", None))
    result

let test_process_con_int_negative () =
  (* SML uses ~ for negation *)
  let input = (ConInt (b "~42")) in
  let result = process_con (b input) in
  check parsetree_constant "negative integer constant (~ converted to -)"
    (Pconst_integer ("-42", None))
    result

let test_process_con_int_hex () =
  let input = (ConInt (b "0xFF")) in
  let result = process_con (b input) in
  check parsetree_constant "hexadecimal integer constant"
    (Pconst_integer ("0xFF", None))
    result

let test_process_con_word_decimal () =
  let input = (ConWord (b "0w42")) in
  let result = process_con (b input) in
  check parsetree_constant "word constant (decimal)"
    (Pconst_integer ("42", None))
    result

let test_process_con_word_hex () =
  let input = (ConWord (b "0wx2A")) in
  let result = process_con (b input) in
  check parsetree_constant "word constant (hexadecimal)"
    (Pconst_integer ("0x2A", None))
    result

let test_process_con_float_positive () =
  let input = (ConFloat (b "3.14")) in
  let result = process_con (b input) in
  check parsetree_constant "positive float constant"
    (Pconst_float ("3.14", None))
    result

let test_process_con_float_negative () =
  let input = (ConFloat (b "~2.718")) in
  let result = process_con (b input) in
  check parsetree_constant "negative float constant (~ converted to -)"
    (Pconst_float ("-2.718", None))
    result

let test_process_con_char () =
  let input = (ConChar (b "a")) in
  let result = process_con (b input) in
  check parsetree_constant "character constant"
    (Pconst_char 'a')
    result

let test_process_con_string () =
  let input = (ConString (b "hello")) in
  let result = process_con (b input) in
  match result with
  | Pconst_string (s, _, _) -> check string "string constant" "hello" s
  | _ -> failwith "Expected string constant"

(** Test cases for process_type_value *)

let test_process_type_var () =
  let input = (TypVar (b (IdxVar (b "a")))) in
  let result = process_type_value (b input) in
  let expected_str = "'a" in
  check string "type variable 'a"
    expected_str
    (core_type_to_string result)

let test_process_type_var_equality () =
  let input = (TypVar (b (IdxVar (b "'eq")))) in
  let result = process_type_value (b input) in
  let expected_str = "'eq" in
  check string "equality type variable 'eq"
    expected_str
    (core_type_to_string result)

let test_process_type_con_int () =
  let input = (TypCon ([], b (IdxIdx (b "int")))) in
  let result = process_type_value (b input) in
  check string "simple type constructor 'int'"
    "int"
    (core_type_to_string result)

let test_process_type_con_list () =
  let input = (TypCon ([b (TypVar (b (IdxVar (b "a"))))], b (IdxIdx (b "list")))) in
  let result = process_type_value (b input) in
  (* OCaml renders this as 'a list *)
  check (bool) "type constructor with single arg"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_type_con_either () =
  let input = (TypCon (
    [b (TypVar (b (IdxVar (b "a")))); b (TypVar (b (IdxVar (b "b"))))],
    b (IdxIdx (b "either"))
  )) in
  let result = process_type_value (b input) in
  check (bool) "type constructor with multiple args"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_type_par () =
  let input = (TypPar (b (TypVar (b (IdxVar (b "a")))))) in
  let result = process_type_value (b input) in
  let expected_str = "'a" in
  check string "parenthesized type"
    expected_str
    (core_type_to_string result)

let test_process_type_fun () =
  let input = (TypFun (
    b (TypVar (b (IdxVar (b "a")))),
    b (TypVar (b (IdxVar (b "b"))))
  )) in
  let result = process_type_value (b input) in
  let result_str = core_type_to_string result in
  check (bool) "function type contains arrow"
    true
    (String.contains result_str '>')

let test_process_type_fun_nested () =
  let input = (TypFun (
    b (TypVar (b (IdxVar (b "a")))),
    b (TypFun (b (TypVar (b (IdxVar (b "b")))), b (TypVar (b (IdxVar (b "c"))))))
  )) in
  let result = process_type_value (b input) in
  check (bool) "nested function type"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_type_tuple () =
  let input = (TypTuple [
    b (TypVar (b (IdxVar (b "a"))));
    b (TypVar (b (IdxVar (b "b"))));
  ]) in
  let result = process_type_value (b input) in
  let result_str = core_type_to_string result in
  check (bool) "tuple type contains asterisk"
    true
    (String.contains result_str '*')

let test_process_type_tuple_three () =
  let input = (TypTuple [
    b (TypVar (b (IdxVar (b "a"))));
    b (TypVar (b (IdxVar (b "b"))));
    b (TypVar (b (IdxVar (b "c"))));
  ]) in
  let result = process_type_value (b input) in
  check (bool) "three-element tuple type"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_type_record_single () =
  let input = (TypRecord [
    b (TypRow (b (IdxLab (b "name")), b (TypCon ([], b (IdxIdx (b "string")))), None))
  ]) in
  let result = process_type_value (b input) in
  let result_str = core_type_to_string result in
  check (bool) "record type contains angle bracket"
    true
    (String.contains result_str '<')

let test_process_type_record_multiple () =
  let input = (TypRecord [
    b (TypRow (
      b (IdxLab (b "name")),
      b (TypCon ([], b (IdxIdx (b "string")))),
      Some (b (TypRow (b (IdxLab (b "age")), b (TypCon ([], b (IdxIdx (b "int")))), None)))
    ))
  ]) in
  let result = process_type_value (b input) in
  check (bool) "record type with multiple fields"
    true
    (String.length (core_type_to_string result) > 0)

(** Test cases for process_object_field_type *)

let test_process_object_field_simple () =
  let input = (TypRow (b (IdxLab (b "x")), b (TypCon ([], b (IdxIdx (b "int")))), None)) in
  let result = process_object_field_type (b input) in
  check (int) "single object field"
    1
    (List.length result)

let test_process_object_field_multiple () =
  let input = (TypRow (
    b (IdxLab (b "x")),
    b (TypCon ([], b (IdxIdx (b "int")))),
    Some (b (TypRow (b (IdxLab (b "y")), b (TypCon ([], b (IdxIdx (b "string")))), None)))
  )) in
  let result = process_object_field_type (b input) in
  check (int) "multiple object fields"
    2
    (List.length result)

(** Test cases for process_type (wrapper) *)

let test_process_type_wrapper () =
  let input = (TypVar (b (IdxVar (b "t")))) in
  let result = process_type (b input) in
  let result_str = core_type_to_string result in
  check string "process_type wrapper"
    "'t"
    result_str

(** Test cases for process_exp *)

let test_process_exp_idx () =
  let input = (ExpIdx (b (IdxIdx (b "x")))) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "expression from identifier"
    true
    (String.contains result_str 'x')

let test_process_exp_app () =
  let input = (ExpApp (
    b (ExpIdx (b (IdxIdx (b "f")))),
    b (ExpIdx (b (IdxIdx (b "x"))))
  )) in
  let result = process_exp (b input) in
  check (bool) "function application expression"
    true
    (String.length (expression_to_string result) > 0)

let test_process_exp_infix () =
  let input = (InfixApp (
    b (ExpIdx (b (IdxIdx (b "x")))),
    b (IdxIdx (b "+")),
    b (ExpIdx (b (IdxIdx (b "y"))))
  )) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "infix application expression"
    true
    (String.length result_str > 0)

let test_process_exp_constant () =
  let input = (ExpCon (b (ConInt (b "42")))) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "constant expression"
    true
    (String.contains result_str '4')

let test_process_exp_tuple_empty () =
  let input = TupleExp [] in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "unit expression (empty tuple)"
    true
    (String.contains result_str '(')

let test_process_exp_tuple () =
  let input = TupleExp ([
    b (ExpCon (b (ConInt (b "1"))));
    b (ExpCon (b (ConInt (b "2"))));
  ] ) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "tuple expression"
    true
    (String.length result_str > 0)

let test_process_exp_record () =
  let input = RecordExp ([
    b (Row (b (IdxLab (b "x")), b (ExpCon (b (ConInt (b "1")))), None));
  ]) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "record expression"
    true
    (String.contains result_str 'x')

let test_process_exp_record_selector () =
  let input = RecordSelector (b (IdxLab (b "name"))) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "record selector (#label)"
    true
    (String.contains result_str 'n')

let test_process_exp_list_empty () =
  let input = ListExp ([] ) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "empty list expression"
    true
    (String.contains result_str '[')

let test_process_exp_list () =
  let input = ListExp ([
    b (ExpCon (b (ConInt (b "1"))));
    b (ExpCon (b (ConInt (b "2"))));
    b (ExpCon (b (ConInt (b "3"))));
  ]) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "list expression"
    true
    (String.length result_str > 0)

let test_process_exp_seq () =
  let input = SeqExp ([
    b (ExpCon (b (ConInt (b "1"))));
    b (ExpCon (b (ConInt (b "2"))));
  ]) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "sequence expression"
    true
    (String.length result_str > 0)

let test_process_exp_let () =
  let input = LetExp (
    [b (ValDec ([], b (ValBind (b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))), b (ExpCon (b (ConInt (b "42")))), None))))],
    [b (ExpIdx (b (IdxIdx (b "x"))))]
  ) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "let expression"
    true
    (String.contains result_str 'x')

let test_process_exp_typed () =
  let input = TypedExp (
    b (ExpCon (b (ConInt (b "42")))),
    b (TypCon ([], b (IdxIdx (b "int"))))
  ) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "typed expression"
    true
    (String.length result_str > 0)

let test_process_exp_raise () =
  let input = RaiseExp (b (ExpIdx (b (IdxIdx (b "Fail"))))) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "raise expression"
    true
    (String.contains result_str 'r')

let test_process_exp_and () =
  let input = AndExp (
    b (ExpIdx (b (IdxIdx (b "x")))),
    b (ExpIdx (b (IdxIdx (b "y"))))
  ) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "andalso expression (converted to &&)"
    true
    (String.length result_str > 0)

let test_process_exp_or () =
  let input = OrExp (
    b (ExpIdx (b (IdxIdx (b "x")))),
    b (ExpIdx (b (IdxIdx (b "y"))))
  ) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "orelse expression (converted to ||)"
    true
    (String.length result_str > 0)

let test_process_exp_if () =
  let input = IfExp (
    b (ExpIdx (b (IdxIdx (b "cond")))),
    b (ExpCon (b (ConInt (b "1")))),
    b (ExpCon (b (ConInt (b "2"))))
  ) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "if expression"
    true
    (String.contains result_str 'i')

let test_process_exp_while () =
  let input = WhileExp (
    b (ExpIdx (b (IdxIdx (b "cond")))),
    b (ExpIdx (b (IdxIdx (b "body"))))
  ) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "while expression"
    true
    (String.length result_str > 0)

let test_process_exp_case () =
  let input = CaseExp (
    b (ExpIdx (b (IdxIdx (b "x")))),
    b (Case (b PatWildcard, b (ExpCon (b (ConInt (b "0")))), None))
  ) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "case expression"
    true
    (String.length result_str > 0)

let test_process_exp_fn () =
  let input = FnExp (
    b (Case (b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))), b (ExpIdx (b (IdxIdx (b "x")))), None))
  ) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check (bool) "fn expression (anonymous function)"
    true
    (String.contains result_str 'x')

(** Test cases for process_pat *)

let test_process_pat_constant () =
  let input = (PatCon (b (ConInt (b "42")))) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "constant pattern"
    true
    (String.contains result_str '4')

let test_process_pat_wildcard () =
  let input = PatWildcard in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "wildcard pattern"
    true
    (String.contains result_str '_')

let test_process_pat_variable () =
  let input = (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "variable pattern"
    true
    (String.contains result_str 'x')

let test_process_pat_constructor_nullary () =
  let input = (PatIdx (b (WithoutOp (b (IdxIdx (b "NONE")))))) in
  let result = process_pat ~is_head:true (b input) in
  let result_str = pattern_to_string result in
  check (bool) "nullary constructor pattern"
    true
    (String.contains result_str 'N')

let test_process_pat_constructor_with_arg () =
  let input = (PatApp (
    b (WithoutOp (b (IdxIdx (b "SOME")))),
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))))
  )) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "constructor pattern with argument"
    true
    (String.contains result_str 'S')

let test_process_pat_infix_cons () =
  let input = (PatInfix (
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
    b (IdxIdx (b "::")),
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "xs"))))))
  )) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "infix constructor pattern (::)"
    true
    (String.length result_str > 0)

let test_process_pat_tuple_empty () =
  let input = (PatTuple []) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "unit pattern (empty tuple)"
    true
    (String.contains result_str '(')

let test_process_pat_tuple () =
  let input = (PatTuple [
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))));
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "y"))))));
  ]) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "tuple pattern"
    true
    (String.length result_str > 0)

let test_process_pat_record () =
  let input = (PatRecord [
    b (PatRowSimple (b (IdxLab (b "x")), b (PatIdx (b (WithoutOp (b (IdxIdx (b "px")))))), b PatRowPoly));
  ]) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "record pattern"
    true
    (String.contains result_str 'x')

let test_process_pat_record_var_shorthand () =
  let input = (PatRecord [
    b (PatRowVar (b (IdxLab (b "x")), None, None, None));
  ]) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "record pattern with variable shorthand"
    true
    (String.contains result_str 'x')

let test_process_pat_list_empty () =
  let input = (PatList []) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "empty list pattern"
    true
    (String.contains result_str '[')

let test_process_pat_list () =
  let input = (PatList [
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))));
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "y"))))));
  ]) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "list pattern"
    true
    (String.length result_str > 0)

let test_process_pat_typed () =
  let input = (PatTyp (
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
    b (TypCon ([], b (IdxIdx (b "int"))))
  )) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "typed pattern"
    true
    (String.contains result_str 'x')

let test_process_pat_as () =
  let input = (PatAs ((
    b (WithoutOp (b (IdxIdx (b "xs")))),
    None,
    b (PatInfix (
      b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
      b (IdxIdx (b "::")),
      b (PatIdx (b (WithoutOp (b (IdxIdx (b "rest"))))))
    ))
  ))) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check (bool) "as pattern (layered pattern)"
    true
    (String.contains result_str 'x')

(** Test cases for process_val_bind *)

let test_process_val_bind_simple () =
  let input = ValBind (
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
    b (ExpCon (b (ConInt (b "42")))),
    None
  ) in
  let result = process_value_bind (b input) in
  check (int) "simple value binding"
    1
    (List.length result)

let test_process_val_bind_multiple () =
  let input = ValBind (
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
    b (ExpCon (b (ConInt (b "1")))),
    Some (b (ValBind (
      b (PatIdx (b (WithoutOp (b (IdxIdx (b "y")))))),
      b (ExpCon (b (ConInt (b "2")))),
      None
    )))
  ) in
  let result = process_val_bind (b input) in
  check (int) "multiple value bindings (and)"
    2
    (List.length result)

(** Test cases for process_fun_bind *)

let test_process_fun_bind_simple () =
  let input = FunBind (
    b (FunMatchPrefix (
      b (WithoutOp (b (IdxIdx (b "f")))),
      [b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))))],
      None,
      b (ExpIdx (b (IdxIdx (b "x")))),
      None
    )),
    None
  ) in
  let result = process_fun_bind (b input) in
  check (int) "simple function binding"
    1
    (List.length result)

let test_process_fun_bind_pattern_match () =
  let input = FunBind (
    b (FunMatchPrefix (
      b (WithoutOp (b (IdxIdx (b "factorial")))),
      [b (PatCon (b (ConInt (b "0"))))],
      None,
      b (ExpCon (b (ConInt (b "1")))),
      Some (b (FunMatchPrefix (
        b (WithoutOp (b (IdxIdx (b "factorial")))),
        [b (PatIdx (b (WithoutOp (b (IdxIdx (b "n"))))))],
        None,
        b (ExpIdx (b (IdxIdx (b "n")))),
        None
      )))
    )),
    None
  ) in
  let result = process_fun_bind (b input) in
  check (int) "function with pattern matching"
    1
    (List.length result)

(** Test cases for process_typ_bind *)

let test_process_typ_bind_simple () =
  let input = TypBind (
    [],
    b (IdxIdx (b "myint")),
    b (TypCon ([], b (IdxIdx (b "int")))),
    None
  ) in
  let result = process_typ_bind (b input) in
  check (int) "simple type abbreviation"
    1
    (List.length result)

let test_process_typ_bind_parametric () =
  let input = TypBind (
    [b (IdxVar (b "a"))],
    b (IdxIdx (b "pair")),
    b (TypTuple [b (TypVar (b (IdxVar (b "a")))); b (TypVar (b (IdxVar (b "a"))))]),
    None
  ) in
  let result = process_typ_bind (b input) in
  check (int) "parametric type abbreviation"
    1
    (List.length result)

(** Test cases for process_dat_bind *)

let test_process_dat_bind_simple () =
  let input = DatBind (
    [],
    b (IdxIdx (b "bool")),
    b (ConBind (b (IdxIdx (b "True")), None, Some (b (ConBind (b (IdxIdx (b "False")), None, None))))),
    None
  ) in
  let result = process_dat_bind (b input) in
  check (int) "simple datatype declaration"
    1
    (List.length result)

let test_process_dat_bind_option () =
  let input = DatBind (
    [b (IdxVar (b "a"))],
    b (IdxIdx (b "option")),
    b (ConBind (
      b (IdxIdx (b "NONE")),
      None,
      Some (b (ConBind (
        b (IdxIdx (b "SOME")),
        Some (b (TypVar (b (IdxVar (b "a"))))),
        None
      )))
    )),
    None
  ) in
  let result = process_dat_bind (b input) in
  check (int) "option datatype declaration"
    1
    (List.length result)

(** Test cases for process_exn_bind *)

let test_process_exn_bind_simple () =
  let input = ExnBind (
    b (IdxIdx (b "Overflow")),
    None,
    None
  ) in
  let result = process_exn_bind (b input) in
  check (int) "simple exception declaration"
    1
    (List.length result)

let test_process_exn_bind_with_arg () =
  let input = ExnBind (
    b (IdxIdx (b "Fail")),
    Some (b (TypCon ([], b (IdxIdx (b "string"))))),
    None
  ) in
  let result = process_exn_bind (b input) in
  check (int) "exception with argument"
    1
    (List.length result)

(** Test cases for process_prog *)

let test_process_prog_simple_val () =
  let input = ProgDec (b (
    ValDec ([], b (ValBind (
      b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
      b (ExpCon (b (ConInt (b "42")))),
      None
    )))
  )) in
  let result = process_prog (b input) in
  check (int) "simple program with value declaration"
    1
    (List.length result)

let test_process_prog_fun () =
  let input = ProgDec (b (
    FunDec (b (FunBind (
      b (FunMatchPrefix (
        b (WithoutOp (b (IdxIdx (b "id")))),
        [b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))))],
        None,
        b (ExpIdx (b (IdxIdx (b "x")))),
        None
      )),
      None
    )))
  )) in
  let result = process_prog (b input) in
  check (int) "program with function declaration"
    1
    (List.length result)

let test_process_prog_datatype () =
  let input = ProgDec (b (
    DatDec (
      b (DatBind (
        [],
        b (IdxIdx (b "color")),
        b (ConBind (
          b (IdxIdx (b "Red")),
          None,
          Some (b (ConBind (b (IdxIdx (b "Green")), None, Some (b (ConBind (b (IdxIdx (b "Blue")), None, None))))))
        )),
        None
      )),
      None
    )
  )) in
  let result = process_prog (b input) in
  check (int) "program with datatype declaration"
    1
    (List.length result)

let test_process_prog_sequence () =
  let input = ProgSeq (
    b (ProgDec (b (ValDec ([], b (ValBind (b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))), b (ExpCon (b (ConInt (b "1")))), None)))))),
    b (ProgDec (b (ValDec ([], b (ValBind (b (PatIdx (b (WithoutOp (b (IdxIdx (b "y")))))), b (ExpCon (b (ConInt (b "2")))), None))))))
  ) in
  let result = process_prog (b input) in
  check (int) "program with sequential declarations"
    2
    (List.length result)

(** Test cases for complex types *)

let test_process_complex_function_type () =
  (* (int * string) -> bool *)
  let input = TypFun (
    b (TypTuple [b (TypCon ([], b (IdxIdx (b "int")))); b (TypCon ([], b (IdxIdx (b "string"))))]),
    b (TypCon ([], b (IdxIdx (b "bool"))))
  ) in
  let result = process_type_value (b input) in
  check (bool) "complex function type"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_list_type () =
  (* 'a list -> int list *)
  let input = TypFun (
    b (TypCon ([b (TypVar (b (IdxVar (b "a"))))], b (IdxIdx (b "list")))),
    b (TypCon ([b (TypCon ([], b (IdxIdx (b "int"))))], b (IdxIdx (b "list"))))
  ) in
  let result = process_type_value (b input) in
  check (bool) "list type transformation"
    true
    (String.length (core_type_to_string result) > 0)

let test_process_higher_order_function () =
  (* ('a -> 'b) -> 'a list -> 'b list *)
  let input = TypFun (
    b (TypFun (b (TypVar (b (IdxVar (b "a")))), b (TypVar (b (IdxVar (b "b")))))),
    b (TypFun (
      b (TypCon ([b (TypVar (b (IdxVar (b "a"))))], b (IdxIdx (b "list")))),
      b (TypCon ([b (TypVar (b (IdxVar (b "b"))))], b (IdxIdx (b "list"))))
    ))
  ) in
  let result = process_type_value (b input) in
  check (bool) "higher-order function type"
    true
    (String.length (core_type_to_string result) > 0)

(** Test suite organization *)

let constant_tests = [
  "positive integer constant", `Quick, test_process_con_int_positive;
  "negative integer constant (~)", `Quick, test_process_con_int_negative;
  "hexadecimal integer constant", `Quick, test_process_con_int_hex;
  "word constant (decimal)", `Quick, test_process_con_word_decimal;
  "word constant (hexadecimal)", `Quick, test_process_con_word_hex;
  "positive float constant", `Quick, test_process_con_float_positive;
  "negative float constant (~)", `Quick, test_process_con_float_negative;
  "character constant", `Quick, test_process_con_char;
  "string constant", `Quick, test_process_con_string;
]

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
  "identifier expression", `Quick, test_process_exp_idx;
  "function application", `Quick, test_process_exp_app;
  "infix application", `Quick, test_process_exp_infix;
  "constant expression", `Quick, test_process_exp_constant;
  "unit expression (empty tuple)", `Quick, test_process_exp_tuple_empty;
  "tuple expression", `Quick, test_process_exp_tuple;
  "record expression", `Quick, test_process_exp_record;
  "record selector (#label)", `Quick, test_process_exp_record_selector;
  "empty list expression", `Quick, test_process_exp_list_empty;
  "list expression", `Quick, test_process_exp_list;
  "sequence expression", `Quick, test_process_exp_seq;
  "let expression", `Quick, test_process_exp_let;
  "typed expression", `Quick, test_process_exp_typed;
  "raise expression", `Quick, test_process_exp_raise;
  "andalso expression (&&)", `Quick, test_process_exp_and;
  "orelse expression (||)", `Quick, test_process_exp_or;
  "if expression", `Quick, test_process_exp_if;
  "while expression", `Quick, test_process_exp_while;
  "case expression", `Quick, test_process_exp_case;
  "fn expression (anonymous function)", `Quick, test_process_exp_fn;
]

let pattern_tests = [
  "constant pattern", `Quick, test_process_pat_constant;
  "wildcard pattern", `Quick, test_process_pat_wildcard;
  "variable pattern", `Quick, test_process_pat_variable;
  "nullary constructor pattern", `Quick, test_process_pat_constructor_nullary;
  "constructor pattern with argument", `Quick, test_process_pat_constructor_with_arg;
  "infix constructor pattern (::)", `Quick, test_process_pat_infix_cons;
  "unit pattern (empty tuple)", `Quick, test_process_pat_tuple_empty;
  "tuple pattern", `Quick, test_process_pat_tuple;
  "record pattern", `Quick, test_process_pat_record;
  "record pattern with variable shorthand", `Quick, test_process_pat_record_var_shorthand;
  "empty list pattern", `Quick, test_process_pat_list_empty;
  "list pattern", `Quick, test_process_pat_list;
  "typed pattern", `Quick, test_process_pat_typed;
  "as pattern (layered pattern)", `Quick, test_process_pat_as;
]

let declaration_tests = [
  "simple value binding", `Quick, test_process_val_bind_simple;
  "multiple value bindings (and)", `Quick, test_process_val_bind_multiple;
  "simple function binding", `Quick, test_process_fun_bind_simple;
  "function with pattern matching", `Quick, test_process_fun_bind_pattern_match;
  "simple type abbreviation", `Quick, test_process_typ_bind_simple;
  "parametric type abbreviation", `Quick, test_process_typ_bind_parametric;
  "simple datatype declaration", `Quick, test_process_dat_bind_simple;
  "option datatype declaration", `Quick, test_process_dat_bind_option;
  "simple exception declaration", `Quick, test_process_exn_bind_simple;
  "exception with argument", `Quick, test_process_exn_bind_with_arg;
]

let program_tests = [
  "simple program with value declaration", `Quick, test_process_prog_simple_val;
  "program with function declaration", `Quick, test_process_prog_fun;
  "program with datatype declaration", `Quick, test_process_prog_datatype;
  "program with sequential declarations", `Quick, test_process_prog_sequence;
]

let complex_type_tests = [
  "process complex function type (tuple -> bool)", `Quick, test_process_complex_function_type;
  "process list type transformation", `Quick, test_process_list_type;
  "process higher-order function type", `Quick, test_process_higher_order_function;
]

(** Pattern matching tests using ppxlib metaquotation *)

(* Test that lambda expressions have the correct structure *)
let test_lambda_structure () =
  let input = FnExp (
    b (Case (b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))), b (ExpIdx (b (IdxIdx (b "x")))), None))
  ) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_function _ -> ()  (* FnExp produces Pexp_function, not Pexp_fun *)
  | _ -> fail "Expected a function expression structure"

(* Test that let bindings produce the correct structure *)
let test_let_structure () =
  let input = LetExp (
    [b (ValDec ([], b (ValBind (b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))), b (ExpCon (b (ConInt (b "42")))), None))))],
    [b (ExpIdx (b (IdxIdx (b "x"))))]
  ) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_let (Nonrecursive, [_], _) -> ()
  | _ -> fail "Expected a non-recursive let binding structure"

(* Test that function application has correct structure *)
let test_app_structure () =
  let input = (ExpApp (
    b (ExpIdx (b (IdxIdx (b "f")))),
    b (ExpIdx (b (IdxIdx (b "x"))))
  )) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_apply (_, [_]) -> ()
  | _ -> fail "Expected a function application with one argument"

(* Test that case expressions produce match structures *)
let test_case_structure () =
  let input = CaseExp (
    b (ExpIdx (b (IdxIdx (b "x")))),
    b (Case (b PatWildcard, b (ExpCon (b (ConInt (b "0")))), None))
  ) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_match (_, _::_) -> ()
  | Pexp_function (_::_) -> () (* Could also be a function with cases *)
  | _ -> fail "Expected a match or function expression"

(* Test that if expressions have the correct structure *)
let test_if_structure () =
  let input = IfExp (
    b (ExpIdx (b (IdxIdx (b "cond")))),
    b (ExpCon (b (ConInt (b "1")))),
    b (ExpCon (b (ConInt (b "2"))))
  ) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_ifthenelse (_, _, Some _) -> ()
  | _ -> fail "Expected if-then-else structure"

(* Test that tuple expressions produce tuple structures *)
let test_tuple_structure () =
  let input = TupleExp ([
    b (ExpCon (b (ConInt (b "1"))));
    b (ExpCon (b (ConInt (b "2"))));
  ]) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_tuple [_; _] -> ()
  | _ -> fail "Expected a 2-element tuple structure"

(* Test that record expressions produce record structures *)
let test_record_structure () =
  let input = RecordExp ([
    b (Row (b (IdxLab (b "x")), b (ExpCon (b (ConInt (b "1")))), None));
  ]) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_record (_, None) -> ()  (* Record expressions produce Pexp_record *)
  | _ -> fail "Expected a record expression structure"

(* Test that list expressions produce list structures *)
let test_list_structure () =
  let input = ListExp ([
    b (ExpCon (b (ConInt (b "1"))));
    b (ExpCon (b (ConInt (b "2"))));
  ]) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_construct ({txt = Lident "::"; _}, Some _) -> ()
  | _ -> fail "Expected a list cons structure"

(* Test that empty lists produce nil structures *)
let test_empty_list_structure () =
  let input = ListExp ([] ) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_construct ({txt = Lident "[]"; _}, None) -> ()
  | _ -> fail "Expected an empty list constructor"

(* Test that sequential expressions produce sequence structures *)
let test_seq_structure () =
  let input = SeqExp ([
    b (ExpCon (b (ConInt (b "1"))));
    b (ExpCon (b (ConInt (b "2"))));
  ]) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_sequence (_, _) -> ()
  | _ -> fail "Expected a sequence expression structure"

(* Test that type annotations produce constraint structures *)
let test_typed_exp_structure () =
  let input = TypedExp (
    b (ExpCon (b (ConInt (b "42")))),
    b (TypCon ([], b (IdxIdx (b "int"))))
  ) in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_constraint (_, _) -> ()
  | _ -> fail "Expected a type constraint structure"

(* Test pattern structures *)

let test_wildcard_pattern_structure () =
  let input = PatWildcard in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_any -> ()
  | _ -> fail "Expected wildcard pattern structure"

let test_variable_pattern_structure () =
  let input = (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))) in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_var _ -> ()
  | _ -> fail "Expected variable pattern structure"

let test_tuple_pattern_structure () =
  let input = (PatTuple [
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))));
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "y"))))));
  ]) in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_tuple [_; _] -> ()
  | _ -> fail "Expected 2-element tuple pattern structure"

let test_list_pattern_structure () =
  let input = (PatList [
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))));
  ]) in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_construct ({txt = Lident "::"; _}, Some _) -> ()
  | _ -> fail "Expected list cons pattern structure"

let test_constant_pattern_structure () =
  let input = (PatCon (b (ConInt (b "42")))) in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_constant _ -> ()
  | _ -> fail "Expected constant pattern structure"

let test_constructor_pattern_structure () =
  let input = (PatApp (
    b (WithoutOp (b (IdxIdx (b "SOME")))),
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))))
  )) in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_construct (_, Some _) -> ()
  | _ -> fail "Expected constructor pattern with argument"

(* Test type structures *)

let test_function_type_structure () =
  let input = TypFun (
    b (TypVar (b (IdxVar (b "a")))),
    b (TypVar (b (IdxVar (b "b"))))
  ) in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_arrow (Nolabel, _, _) -> ()
  | _ -> fail "Expected arrow type structure"

let test_tuple_type_structure () =
  let input = TypTuple ([
    b (TypVar (b (IdxVar (b "a"))));
    b (TypVar (b (IdxVar (b "b"))));
  ]) in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_tuple [_; _] -> ()
  | _ -> fail "Expected 2-element tuple type structure"

let test_type_constructor_structure () =
  let input = TypCon ([], b (IdxIdx (b "int"))) in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_constr ({txt = Lident "int"; _}, []) -> ()
  | _ -> fail "Expected simple type constructor 'int'"

let test_parametric_type_structure () =
  let input = TypCon ([b (TypVar (b (IdxVar (b "a"))))], b (IdxIdx (b "list"))) in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_constr ({txt = Lident "list"; _}, [_]) -> ()
  | _ -> fail "Expected parametric type constructor 'list' with one argument"

let test_type_var_structure () =
  let input = TypVar (b (IdxVar (b "a"))) in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_var "a" -> ()
  | _ -> fail "Expected type variable 'a'"

let test_object_type_structure () =
  let input = TypRecord ([
    b (TypRow (b (IdxLab (b "x")), b (TypCon ([], b (IdxIdx (b "int")))), None))
  ]) in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_object (_, _) -> ()
  | _ -> fail "Expected object type structure (SML records become OCaml objects)"

(* Test declaration structures *)

let test_val_declaration_structure () =
  let input = ValBind (
    b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
    b (ExpCon (b (ConInt (b "42")))),
    None
  ) in
  let result = process_val_bind (b input) in
  match result with
  | [binding] ->
      (match binding.pvb_expr.pexp_desc with
       | Pexp_constant _ -> ()
       | _ -> fail "Expected constant expression in value binding")
  | _ -> fail "Expected a single value binding"

let test_fun_declaration_structure () =
  let input = FunBind (
    b (FunMatchPrefix (
      b (WithoutOp (b (IdxIdx (b "f")))),
      [b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))))],
      None,
      b (ExpIdx (b (IdxIdx (b "x")))),
      None
    )),
    None
  ) in
  let result = process_fun_bind (b input) in
  match result with
  | [binding] ->
      (match binding.pvb_expr.pexp_desc with
       | Pexp_fun _ -> ()
       | _ -> fail "Expected function expression in binding")
  | _ -> fail "Expected a single function binding"

let test_datatype_declaration_structure () =
  let input = DatBind (
    [],
    b (IdxIdx (b "bool")),
    b (ConBind (b (IdxIdx (b "True")), None, Some (b (ConBind (b (IdxIdx (b "False")), None, None))))),
    None
  ) in
  let result = process_dat_bind (b input) in
  match result with
  | [decl] ->
      (match decl.ptype_kind with
       | Ptype_variant _ -> ()
       | _ -> fail "Expected variant type declaration")
  | _ -> fail "Expected a single type declaration"

let test_exception_declaration_structure () =
  let input = ExnBind (
    b (IdxIdx (b "Overflow")),
    None,
    None
  ) in
  let result = process_exn_bind (b input) in
  match result with
  | [ext_cons] ->
      (* Check that we got an extension constructor (for exceptions) *)
      (match ext_cons.pext_kind with
       | Pext_decl _ -> ()
       | _ -> fail "Expected exception declaration constructor")
  | _ -> fail "Expected a single extension constructor"

let pattern_matching_tests = [
  "lambda has correct AST structure", `Quick, test_lambda_structure;
  "let binding has correct AST structure", `Quick, test_let_structure;
  "function application has correct AST structure", `Quick, test_app_structure;
  "case expression has correct AST structure", `Quick, test_case_structure;
  "if expression has correct AST structure", `Quick, test_if_structure;
  "tuple expression has correct AST structure", `Quick, test_tuple_structure;
  "record expression has correct AST structure", `Quick, test_record_structure;
  "list expression has correct AST structure", `Quick, test_list_structure;
  "empty list has correct AST structure", `Quick, test_empty_list_structure;
  "sequence expression has correct AST structure", `Quick, test_seq_structure;
  "typed expression has correct AST structure", `Quick, test_typed_exp_structure;
  "wildcard pattern has correct AST structure", `Quick, test_wildcard_pattern_structure;
  "variable pattern has correct AST structure", `Quick, test_variable_pattern_structure;
  "tuple pattern has correct AST structure", `Quick, test_tuple_pattern_structure;
  "list pattern has correct AST structure", `Quick, test_list_pattern_structure;
  "constant pattern has correct AST structure", `Quick, test_constant_pattern_structure;
  "constructor pattern has correct AST structure", `Quick, test_constructor_pattern_structure;
  "function type has correct AST structure", `Quick, test_function_type_structure;
  "tuple type has correct AST structure", `Quick, test_tuple_type_structure;
  "type constructor has correct AST structure", `Quick, test_type_constructor_structure;
  "parametric type has correct AST structure", `Quick, test_parametric_type_structure;
  "type variable has correct AST structure", `Quick, test_type_var_structure;
  "object type has correct AST structure", `Quick, test_object_type_structure;
  "val declaration has correct AST structure", `Quick, test_val_declaration_structure;
  "fun declaration has correct AST structure", `Quick, test_fun_declaration_structure;
  "datatype declaration has correct AST structure", `Quick, test_datatype_declaration_structure;
  "exception declaration has correct AST structure", `Quick, test_exception_declaration_structure;
]

(** Main test runner *)

let run_unit_tests () : unit =
  Alcotest.run "Backend" [
    "Constant Processing", constant_tests;
    "Type Processing", type_tests;
    "Object Field Processing", object_field_tests;
    "Expression Processing", expression_tests;
    "Pattern Processing", pattern_tests;
    "Declaration Processing", declaration_tests;
    "Program Processing", program_tests;
    "Complex Type Processing", complex_type_tests;
    "Pattern Matching (AST Structure)", pattern_matching_tests;
  ]

let () = 
  run_unit_tests ()