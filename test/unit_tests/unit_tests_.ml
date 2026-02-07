open Alcotest
open Ast

(** Helper for box_node *)
let b = Ast.box_node

module PR = Backend.Precedence_resolver
(** Access Precedence_resolver from backend library *)

(** Test configuration *)
module TestConfig : Common.CONFIG = struct
  let config =
    Common.create [
      Common.set Input_file Common.StdIn;
      Common.set Output_file Common.Silent;
      Common.set Verbosity 3
    ]
end

module TestContext (* TODO *) = struct
  let lexbuf = ""
  let context = Context.basis_context
end

module TestBackend = Backend.Make (TestContext) (TestConfig)

(** Instantiate Backend with test config *)

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
    (fun fmt ct -> Format.fprintf fmt "%a" Ppxlib.Pprintast.core_type ct)
    (fun a b -> core_type_to_string a = core_type_to_string b)

let parsetree_expression : Parsetree.expression testable =
  testable
    (fun fmt e -> Format.fprintf fmt "%a" Ppxlib.Pprintast.expression e)
    (fun a b -> expression_to_string a = expression_to_string b)

let pattern_to_string (p : Parsetree.pattern) : string =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  Ppxlib.Pprintast.pattern fmt p;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let parsetree_pattern : Parsetree.pattern testable =
  testable
    (fun fmt p -> Format.fprintf fmt "%a" Ppxlib.Pprintast.pattern p)
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
  let input = ConInt (b "42") in
  let result = process_con (b input) in
  check parsetree_constant "positive integer constant"
    (Pconst_integer ("42", None))
    result

let test_process_con_int_negative () =
  (* SML uses ~ for negation *)
  let input = ConInt (b "~42") in
  let result = process_con (b input) in
  check parsetree_constant "negative integer constant (~ converted to -)"
    (Pconst_integer ("-42", None))
    result

let test_process_con_int_hex () =
  let input = ConInt (b "0xFF") in
  let result = process_con (b input) in
  check parsetree_constant "hexadecimal integer constant"
    (Pconst_integer ("0xFF", None))
    result

let test_process_con_word_decimal () =
  let input = ConWord (b "0w42") in
  let result = process_con (b input) in
  check parsetree_constant "word constant (decimal)"
    (Pconst_integer ("42", None))
    result

let test_process_con_word_hex () =
  let input = ConWord (b "0wx2A") in
  let result = process_con (b input) in
  check parsetree_constant "word constant (hexadecimal)"
    (Pconst_integer ("0x2A", None))
    result

let test_process_con_float_positive () =
  let input = ConFloat (b "3.14") in
  let result = process_con (b input) in
  check parsetree_constant "positive float constant"
    (Pconst_float ("3.14", None))
    result

let test_process_con_float_negative () =
  let input = ConFloat (b "~2.718") in
  let result = process_con (b input) in
  check parsetree_constant "negative float constant (~ converted to -)"
    (Pconst_float ("-2.718", None))
    result

let test_process_con_char () =
  let input = ConChar (b "a") in
  let result = process_con (b input) in
  check parsetree_constant "character constant" (Pconst_char 'a') result

let test_process_con_string () =
  let input = ConString (b "hello") in
  let result = process_con (b input) in
  match result with
  | Pconst_string (s, _, _) -> check string "string constant" "hello" s
  | _ -> failwith "Expected string constant"

(** Test cases for process_type_value *)

let test_process_type_var () =
  let input = TypVar (b (IdxVar (b "a"))) in
  let result = process_type_value (b input) in
  let expected_str = "'a" in
  check string "type variable 'a" expected_str (core_type_to_string result)

let test_process_type_var_equality () =
  let input = TypVar (b (IdxVar (b "'eq"))) in
  let result = process_type_value (b input) in
  let expected_str = "'eq" in
  check string "equality type variable 'eq" expected_str
    (core_type_to_string result)

let test_process_type_con_int () =
  let input = TypCon ([], b (IdxIdx (b "int"))) in
  let result = process_type_value (b input) in
  check string "simple type constructor 'int'" "int"
    (core_type_to_string result)

let test_process_type_con_list () =
  let input =
    TypCon ([ b (TypVar (b (IdxVar (b "a")))) ], b (IdxIdx (b "list")))
  in
  let result = process_type_value (b input) in
  (* OCaml renders this as 'a list *)
  check bool "type constructor with single arg" true
    (String.length (core_type_to_string result) > 0)

let test_process_type_con_either () =
  let input =
    TypCon
      ( [ b (TypVar (b (IdxVar (b "a")))); b (TypVar (b (IdxVar (b "b")))) ],
        b (IdxIdx (b "either")) )
  in
  let result = process_type_value (b input) in
  check bool "type constructor with multiple args" true
    (String.length (core_type_to_string result) > 0)

let test_process_type_par () =
  let input = TypPar (b (TypVar (b (IdxVar (b "a"))))) in
  let result = process_type_value (b input) in
  let expected_str = "'a" in
  check string "parenthesized type" expected_str (core_type_to_string result)

let test_process_type_fun () =
  let input =
    TypFun (b (TypVar (b (IdxVar (b "a")))), b (TypVar (b (IdxVar (b "b")))))
  in
  let result = process_type_value (b input) in
  let result_str = core_type_to_string result in
  check bool "function type contains arrow" true
    (String.contains result_str '>')

let test_process_type_fun_nested () =
  let input =
    TypFun
      ( b (TypVar (b (IdxVar (b "a")))),
        b
          (TypFun
             (b (TypVar (b (IdxVar (b "b")))), b (TypVar (b (IdxVar (b "c"))))))
      )
  in
  let result = process_type_value (b input) in
  check bool "nested function type" true
    (String.length (core_type_to_string result) > 0)

let test_process_type_tuple () =
  let input =
    TypTuple
      [ b (TypVar (b (IdxVar (b "a")))); b (TypVar (b (IdxVar (b "b")))) ]
  in
  let result = process_type_value (b input) in
  let result_str = core_type_to_string result in
  check bool "tuple type contains asterisk" true
    (String.contains result_str '*')

let test_process_type_tuple_three () =
  let input =
    TypTuple
      [
        b (TypVar (b (IdxVar (b "a"))));
        b (TypVar (b (IdxVar (b "b"))));
        b (TypVar (b (IdxVar (b "c"))));
      ]
  in
  let result = process_type_value (b input) in
  check bool "three-element tuple type" true
    (String.length (core_type_to_string result) > 0)

let test_process_type_record_single () =
  let input =
    TypRecord
      [
        b
          (TypRow
             ( b (IdxLab (b "name")),
               b (TypCon ([], b (IdxIdx (b "string")))),
               None ));
      ]
  in
  let result = process_type_value (b input) in
  let result_str = core_type_to_string result in
  check bool "record type contains angle bracket" true
    (String.contains result_str '<')

let test_process_type_record_multiple () =
  let input =
    TypRecord
      [
        b
          (TypRow
             ( b (IdxLab (b "name")),
               b (TypCon ([], b (IdxIdx (b "string")))),
               Some
                 (b
                    (TypRow
                       ( b (IdxLab (b "age")),
                         b (TypCon ([], b (IdxIdx (b "int")))),
                         None ))) ));
      ]
  in
  let result = process_type_value (b input) in
  check bool "record type with multiple fields" true
    (String.length (core_type_to_string result) > 0)

(** Test cases for process_object_field_type *)

let test_process_object_field_simple () =
  let input =
    TypRow (b (IdxLab (b "x")), b (TypCon ([], b (IdxIdx (b "int")))), None)
  in
  let result = process_object_field_type (b input) in
  check int "single object field" 1 (List.length result)

let test_process_object_field_multiple () =
  let input =
    TypRow
      ( b (IdxLab (b "x")),
        b (TypCon ([], b (IdxIdx (b "int")))),
        Some
          (b
             (TypRow
                ( b (IdxLab (b "y")),
                  b (TypCon ([], b (IdxIdx (b "string")))),
                  None ))) )
  in
  let result = process_object_field_type (b input) in
  check int "multiple object fields" 2 (List.length result)

(** Test cases for process_type (wrapper) *)

let test_process_type_wrapper () =
  let input = TypVar (b (IdxVar (b "t"))) in
  let result = process_type (b input) in
  let result_str = core_type_to_string result in
  check string "process_type wrapper" "'t" result_str

(** Test cases for process_exp *)

let test_process_exp_idx () =
  let input = ExpIdx (b (IdxIdx (b "x"))) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "expression from identifier" true (String.contains result_str 'x')

let test_process_exp_app () =
  (* Updated for list-based ExpApp *)
  let input =
    ExpApp [ b (ExpIdx (b (IdxIdx (b "f")))); b (ExpIdx (b (IdxIdx (b "x")))) ]
  in
  let result = process_exp (b input) in
  check bool "function application expression" true
    (String.length (expression_to_string result) > 0)

(* TODO: Restore after implementing precedence resolution - tests removed with InfixApp
let test_process_exp_infix () = ...
let test_process_exp_infix_multiply () = ...
let test_process_exp_infix_cons () = ...
let test_process_exp_infix_nested () = ...
*)

let test_process_exp_constant () =
  let input = ExpCon (b (ConInt (b "42"))) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "constant expression" true (String.contains result_str '4')

let test_process_exp_tuple_empty () =
  let input = TupleExp [] in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "unit expression (empty tuple)" true
    (String.contains result_str '(')

let test_process_exp_tuple () =
  let input =
    TupleExp
      [ b (ExpCon (b (ConInt (b "1")))); b (ExpCon (b (ConInt (b "2")))) ]
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "tuple expression" true (String.length result_str > 0)

let test_process_exp_record () =
  let input =
    RecordExp
      [ b (Row (b (IdxLab (b "x")), b (ExpCon (b (ConInt (b "1")))), None)) ]
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "record expression" true (String.contains result_str 'x')

let test_process_exp_record_selector () =
  let input = RecordSelector (b (IdxLab (b "name"))) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "record selector (#label)" true (String.contains result_str 'n')

let test_process_exp_list_empty () =
  let input = ListExp [] in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "empty list expression" true (String.contains result_str '[')

let test_process_exp_list () =
  let input =
    ListExp
      [
        b (ExpCon (b (ConInt (b "1"))));
        b (ExpCon (b (ConInt (b "2"))));
        b (ExpCon (b (ConInt (b "3"))));
      ]
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "list expression" true (String.length result_str > 0)

let test_process_exp_seq () =
  let input =
    SeqExp [ b (ExpCon (b (ConInt (b "1")))); b (ExpCon (b (ConInt (b "2")))) ]
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "sequence expression" true (String.length result_str > 0)

let test_process_exp_let () =
  let input =
    LetExp
      ( [
          b
            (ValDec
               ( [],
                 b
                   (ValBind
                      ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
                        b (ExpCon (b (ConInt (b "42")))),
                        None )) ));
        ],
        [ b (ExpIdx (b (IdxIdx (b "x")))) ] )
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "let expression" true (String.contains result_str 'x')

let test_process_exp_typed () =
  let input =
    TypedExp
      (b (ExpCon (b (ConInt (b "42")))), b (TypCon ([], b (IdxIdx (b "int")))))
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "typed expression" true (String.length result_str > 0)

let test_process_exp_raise () =
  let input = RaiseExp (b (ExpIdx (b (IdxIdx (b "Fail"))))) in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "raise expression" true (String.contains result_str 'r')

let test_process_exp_and () =
  let input =
    AndExp (b (ExpIdx (b (IdxIdx (b "x")))), b (ExpIdx (b (IdxIdx (b "y")))))
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "andalso expression (converted to &&)" true
    (String.length result_str > 0)

let test_process_exp_or () =
  let input =
    OrExp (b (ExpIdx (b (IdxIdx (b "x")))), b (ExpIdx (b (IdxIdx (b "y")))))
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "orelse expression (converted to ||)" true
    (String.length result_str > 0)

let test_process_exp_if () =
  let input =
    IfExp
      ( b (ExpIdx (b (IdxIdx (b "cond")))),
        b (ExpCon (b (ConInt (b "1")))),
        b (ExpCon (b (ConInt (b "2")))) )
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "if expression" true (String.contains result_str 'i')

let test_process_exp_while () =
  let input =
    WhileExp
      (b (ExpIdx (b (IdxIdx (b "cond")))), b (ExpIdx (b (IdxIdx (b "body")))))
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "while expression" true (String.length result_str > 0)

let test_process_exp_case () =
  let input =
    CaseExp
      ( b (ExpIdx (b (IdxIdx (b "x")))),
        b (Case (b PatWildcard, b (ExpCon (b (ConInt (b "0")))), None)) )
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "case expression" true (String.length result_str > 0)

let test_process_exp_fn () =
  let input =
    FnExp
      (b
         (Case
            ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
              b (ExpIdx (b (IdxIdx (b "x")))),
              None )))
  in
  let result = process_exp (b input) in
  let result_str = expression_to_string result in
  check bool "fn expression (anonymous function)" true
    (String.contains result_str 'x')

(** Test cases for process_pat *)

let test_process_pat_constant () =
  let input = PatCon (b (ConInt (b "42"))) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "constant pattern" true (String.contains result_str '4')

let test_process_pat_wildcard () =
  let input = PatWildcard in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "wildcard pattern" true (String.contains result_str '_')

let test_process_pat_variable () =
  let input = PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))) in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "variable pattern" true (String.contains result_str 'x')

let test_process_pat_constructor_nullary () =
  let input = PatIdx (b (WithoutOp (b (IdxIdx (b "NONE"))))) in
  let result = process_pat ~is_head:true (b input) in
  let result_str = pattern_to_string result in
  check bool "nullary constructor pattern" true (String.contains result_str 'N')

let test_process_pat_constructor_with_arg () =
  (* Updated for list-based PatApp *)
  let input =
    PatApp
      [
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "SOME"))))));
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))));
      ]
  in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "constructor pattern with argument" true
    (String.contains result_str 'S')

(* TODO: Restore after implementing precedence resolution
let test_process_pat_infix_cons () =
  let input =
    PatInfix
      ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
        b (IdxIdx (b "::")),
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "xs")))))) )
  in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "infix constructor pattern (::)" true (String.length result_str > 0)
*)

let test_process_pat_tuple_empty () =
  let input = PatTuple [] in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "unit pattern (empty tuple)" true (String.contains result_str '(')

let test_process_pat_tuple () =
  let input =
    PatTuple
      [
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))));
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "y"))))));
      ]
  in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "tuple pattern" true (String.length result_str > 0)

let test_process_pat_record () =
  let input =
    PatRecord
      [
        b
          (PatRowSimple
             ( b (IdxLab (b "x")),
               b (PatIdx (b (WithoutOp (b (IdxIdx (b "px")))))),
               b PatRowPoly ));
      ]
  in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "record pattern" true (String.contains result_str 'x')

let test_process_pat_record_var_shorthand () =
  let input =
    PatRecord [ b (PatRowVar (b (IdxLab (b "x")), None, None, None)) ]
  in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "record pattern with variable shorthand" true
    (String.contains result_str 'x')

let test_process_pat_list_empty () =
  let input = PatList [] in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "empty list pattern" true (String.contains result_str '[')

let test_process_pat_list () =
  let input =
    PatList
      [
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))));
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "y"))))));
      ]
  in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "list pattern" true (String.length result_str > 0)

let test_process_pat_typed () =
  let input =
    PatTyp
      ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
        b (TypCon ([], b (IdxIdx (b "int")))) )
  in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "typed pattern" true (String.contains result_str 'x')

(* TODO: Restore after implementing precedence resolution - uses PatInfix
let test_process_pat_as () =
  let input =
    PatAs
      ( b (WithoutOp (b (IdxIdx (b "xs")))),
        None,
        b
          (PatInfix
             ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
               b (IdxIdx (b "::")),
               b (PatIdx (b (WithoutOp (b (IdxIdx (b "rest")))))) )) )
  in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  check bool "as pattern (layered pattern)" true
    (String.contains result_str 'x')
*)

(** Test configuration with guess_var enabled *)
module TestConfigWithGuessVar : Common.CONFIG = struct
  let config =
    Common.create [
      Common.set Input_file Common.StdIn;
      Common.set Output_file Common.Silent;
      Common.set Verbosity 3;
      Common.set Guess_var (Some "[A-Z][a-zA-Z0-9_]*")
    ]
end

module TestContextWithGuessVar = struct
  let lexbuf = ""
  let context = Context.basis_context
end

module TestBackendWithGuessVar =
  Backend.Make (TestContextWithGuessVar) (TestConfigWithGuessVar)

let test_process_pat_as_with_guess_var () =
  (* Test that an as pattern variable matching guess_var gets lowercased and has _ appended *)
  let input =
    PatAs
      ( b (WithoutOp (b (IdxIdx (b "Result")))),
        None,
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))) )
  in
  let result = TestBackendWithGuessVar.process_pat (b input) in
  let result_str = pattern_to_string result in
  (* "Result" should be converted to "result_" because it matches the guess_var pattern *)
  check string "as pattern variable matching guess_var gets _ suffix"
    "x as result_" result_str

let test_process_pat_ref () =
  (* Updated for list-based PatApp *)
  let input =
    PatApp
      [
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "ref"))))));
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))));
      ]
  in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  (* ref x should become { contents = x } *)
  check bool "ref pattern converted to record" true
    (String.contains result_str '{' && String.contains result_str 'c')

let test_process_pat_ref_nested () =
  (* Updated for list-based PatApp *)
  let input =
    PatTuple
      [
        b
          (PatApp
             [
               b (PatIdx (b (WithoutOp (b (IdxIdx (b "ref"))))));
               b (PatIdx (b (WithoutOp (b (IdxIdx (b "a"))))));
             ]);
        b
          (PatApp
             [
               b (PatIdx (b (WithoutOp (b (IdxIdx (b "ref"))))));
               b (PatIdx (b (WithoutOp (b (IdxIdx (b "b"))))));
             ]);
      ]
  in
  let result = process_pat (b input) in
  let result_str = pattern_to_string result in
  (* (ref a, ref b) should become ({ contents = a }, { contents = b }) *)
  check bool "nested ref patterns in tuple" true
    (String.contains result_str '{' && String.contains result_str 'a')

(** Test cases for process_val_bind *)

let test_process_val_bind_simple () =
  let input =
    ValBind
      ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
        b (ExpCon (b (ConInt (b "42")))),
        None )
  in
  let result = process_val_bind input in
  check int "simple value binding" 1 (List.length result)

let test_process_val_bind_multiple () =
  let input =
    ValBind
      ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
        b (ExpCon (b (ConInt (b "1")))),
        Some
          (b
             (ValBind
                ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "y")))))),
                  b (ExpCon (b (ConInt (b "2")))),
                  None ))) )
  in
  let result = process_val_bind input in
  check int "multiple value bindings (and)" 2 (List.length result)

(** Test cases for process_fun_bind *)

let test_process_fun_bind_simple () =
  let input =
    FunBind
      ( b
          (FunMatchPrefix
             ( b (WithoutOp (b (IdxIdx (b "f")))),
               [ b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))) ],
               None,
               b (ExpIdx (b (IdxIdx (b "x")))),
               None )),
        None )
  in
  let result = process_fun_bind input in
  check int "simple function binding" 1 (List.length result)

let test_process_fun_bind_pattern_match () =
  let input =
    FunBind
      ( b
          (FunMatchPrefix
             ( b (WithoutOp (b (IdxIdx (b "factorial")))),
               [ b (PatCon (b (ConInt (b "0")))) ],
               None,
               b (ExpCon (b (ConInt (b "1")))),
               Some
                 (b
                    (FunMatchPrefix
                       ( b (WithoutOp (b (IdxIdx (b "factorial")))),
                         [ b (PatIdx (b (WithoutOp (b (IdxIdx (b "n")))))) ],
                         None,
                         b (ExpIdx (b (IdxIdx (b "n")))),
                         None ))) )),
        None )
  in
  let result = process_fun_bind input in
  check int "function with pattern matching" 1 (List.length result)

(** Test cases for process_typ_bind *)

let test_process_typ_bind_simple () =
  let input =
    TypBind
      ([], b (IdxIdx (b "myint")), b (TypCon ([], b (IdxIdx (b "int")))), None)
  in
  let result = process_typ_bind input in
  check int "simple type abbreviation" 1 (List.length result)

let test_process_typ_bind_parametric () =
  let input =
    TypBind
      ( [ b (IdxVar (b "a")) ],
        b (IdxIdx (b "pair")),
        b
          (TypTuple
             [
               b (TypVar (b (IdxVar (b "a")))); b (TypVar (b (IdxVar (b "a"))));
             ]),
        None )
  in
  let result = process_typ_bind input in
  check int "parametric type abbreviation" 1 (List.length result)

(** Test cases for process_dat_bind *)

let test_process_dat_bind_simple () =
  let input =
    DatBind
      ( [],
        b (IdxIdx (b "bool")),
        b
          (ConBind
             ( b (IdxIdx (b "True")),
               None,
               Some (b (ConBind (b (IdxIdx (b "False")), None, None))) )),
        None )
  in
  let result = process_dat_bind input in
  check int "simple datatype declaration" 1 (List.length result)

let test_process_dat_bind_option () =
  let input =
    DatBind
      ( [ b (IdxVar (b "a")) ],
        b (IdxIdx (b "option")),
        b
          (ConBind
             ( b (IdxIdx (b "NONE")),
               None,
               Some
                 (b
                    (ConBind
                       ( b (IdxIdx (b "SOME")),
                         Some (b (TypVar (b (IdxVar (b "a"))))),
                         None ))) )),
        None )
  in
  let result = process_dat_bind input in
  check int "option datatype declaration" 1 (List.length result)

(** Test cases for process_exn_bind *)

let test_process_exn_bind_simple () =
  let input = ExnBind (b (IdxIdx (b "Overflow")), None, None) in
  let result = process_exn_bind input in
  check int "simple exception declaration" 1 (List.length result)

let test_process_exn_bind_with_arg () =
  let input =
    ExnBind
      ( b (IdxIdx (b "Fail")),
        Some (b (TypCon ([], b (IdxIdx (b "string"))))),
        None )
  in
  let result = process_exn_bind input in
  check int "exception with argument" 1 (List.length result)

(** Test cases for process_prog *)

let test_process_prog_simple_val () =
  let input =
    ProgDec
      (b
         (ValDec
            ( [],
              b
                (ValBind
                   ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
                     b (ExpCon (b (ConInt (b "42")))),
                     None )) )))
  in
  let result = process_prog input in
  check int "simple program with value declaration" 1 (List.length result)

let test_process_prog_fun () =
  let input =
    ProgDec
      (b
         (FunDec
            (b
               (FunBind
                  ( b
                      (FunMatchPrefix
                         ( b (WithoutOp (b (IdxIdx (b "id")))),
                           [ b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))) ],
                           None,
                           b (ExpIdx (b (IdxIdx (b "x")))),
                           None )),
                    None )))))
  in
  let result = process_prog input in
  check int "program with function declaration" 1 (List.length result)

let test_process_prog_datatype () =
  let input =
    ProgDec
      (b
         (DatDec
            ( b
                (DatBind
                   ( [],
                     b (IdxIdx (b "color")),
                     b
                       (ConBind
                          ( b (IdxIdx (b "Red")),
                            None,
                            Some
                              (b
                                 (ConBind
                                    ( b (IdxIdx (b "Green")),
                                      None,
                                      Some
                                        (b
                                           (ConBind
                                              (b (IdxIdx (b "Blue")), None, None)))
                                    ))) )),
                     None )),
              None )))
  in
  let result = process_prog input in
  check int "program with datatype declaration" 1 (List.length result)

let test_process_prog_sequence () =
  let input =
    ProgSeq
      ( b
          (ProgDec
             (b
                (ValDec
                   ( [],
                     b
                       (ValBind
                          ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
                            b (ExpCon (b (ConInt (b "1")))),
                            None )) )))),
        b
          (ProgDec
             (b
                (ValDec
                   ( [],
                     b
                       (ValBind
                          ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "y")))))),
                            b (ExpCon (b (ConInt (b "2")))),
                            None )) )))) )
  in
  let result = process_prog input in
  check int "program with sequential declarations" 2 (List.length result)

(** Test cases for complex types *)

let test_process_complex_function_type () =
  (* (int * string) -> bool *)
  let input =
    TypFun
      ( b
          (TypTuple
             [
               b (TypCon ([], b (IdxIdx (b "int"))));
               b (TypCon ([], b (IdxIdx (b "string"))));
             ]),
        b (TypCon ([], b (IdxIdx (b "bool")))) )
  in
  let result = process_type_value (b input) in
  check bool "complex function type" true
    (String.length (core_type_to_string result) > 0)

let test_process_list_type () =
  (* 'a list -> int list *)
  let input =
    TypFun
      ( b (TypCon ([ b (TypVar (b (IdxVar (b "a")))) ], b (IdxIdx (b "list")))),
        b
          (TypCon
             ([ b (TypCon ([], b (IdxIdx (b "int")))) ], b (IdxIdx (b "list"))))
      )
  in
  let result = process_type_value (b input) in
  check bool "list type transformation" true
    (String.length (core_type_to_string result) > 0)

let test_process_higher_order_function () =
  (* ('a -> 'b) -> 'a list -> 'b list *)
  let input =
    TypFun
      ( b
          (TypFun
             (b (TypVar (b (IdxVar (b "a")))), b (TypVar (b (IdxVar (b "b")))))),
        b
          (TypFun
             ( b
                 (TypCon
                    ([ b (TypVar (b (IdxVar (b "a")))) ], b (IdxIdx (b "list")))),
               b
                 (TypCon
                    ([ b (TypVar (b (IdxVar (b "b")))) ], b (IdxIdx (b "list"))))
             )) )
  in
  let result = process_type_value (b input) in
  check bool "higher-order function type" true
    (String.length (core_type_to_string result) > 0)

(** Test suite organization *)

let constant_tests =
  [
    ("positive integer constant", `Quick, test_process_con_int_positive);
    ("negative integer constant (~)", `Quick, test_process_con_int_negative);
    ("hexadecimal integer constant", `Quick, test_process_con_int_hex);
    ("word constant (decimal)", `Quick, test_process_con_word_decimal);
    ("word constant (hexadecimal)", `Quick, test_process_con_word_hex);
    ("positive float constant", `Quick, test_process_con_float_positive);
    ("negative float constant (~)", `Quick, test_process_con_float_negative);
    ("character constant", `Quick, test_process_con_char);
    ("string constant", `Quick, test_process_con_string);
  ]

let type_tests =
  [
    ("process type variable", `Quick, test_process_type_var);
    ("process equality type variable", `Quick, test_process_type_var_equality);
    ("process simple type constructor (int)", `Quick, test_process_type_con_int);
    ( "process type constructor with single arg (list)",
      `Quick,
      test_process_type_con_list );
    ( "process type constructor with multiple args",
      `Quick,
      test_process_type_con_either );
    ("process parenthesized type", `Quick, test_process_type_par);
    ("process function type", `Quick, test_process_type_fun);
    ("process nested function type", `Quick, test_process_type_fun_nested);
    ("process tuple type (2 elements)", `Quick, test_process_type_tuple);
    ("process tuple type (3 elements)", `Quick, test_process_type_tuple_three);
    ( "process record type (single field)",
      `Quick,
      test_process_type_record_single );
    ( "process record type (multiple fields)",
      `Quick,
      test_process_type_record_multiple );
    ("process_type wrapper function", `Quick, test_process_type_wrapper);
  ]

let object_field_tests =
  [
    ("process single object field", `Quick, test_process_object_field_simple);
    ( "process multiple object fields",
      `Quick,
      test_process_object_field_multiple );
  ]

let expression_tests =
  [
    ("identifier expression", `Quick, test_process_exp_idx);
    ("function application", `Quick, test_process_exp_app);
    (* TODO: Restore infix tests after implementing precedence resolution
    ("infix application", `Quick, test_process_exp_infix);
    ("infix multiply", `Quick, test_process_exp_infix_multiply);
    ("infix cons (::)", `Quick, test_process_exp_infix_cons);
    ("nested infix", `Quick, test_process_exp_infix_nested); *)
    ("constant expression", `Quick, test_process_exp_constant);
    ("unit expression (empty tuple)", `Quick, test_process_exp_tuple_empty);
    ("tuple expression", `Quick, test_process_exp_tuple);
    ("record expression", `Quick, test_process_exp_record);
    ("record selector (#label)", `Quick, test_process_exp_record_selector);
    ("empty list expression", `Quick, test_process_exp_list_empty);
    ("list expression", `Quick, test_process_exp_list);
    ("sequence expression", `Quick, test_process_exp_seq);
    ("let expression", `Quick, test_process_exp_let);
    ("typed expression", `Quick, test_process_exp_typed);
    ("raise expression", `Quick, test_process_exp_raise);
    ("andalso expression (&&)", `Quick, test_process_exp_and);
    ("orelse expression (||)", `Quick, test_process_exp_or);
    ("if expression", `Quick, test_process_exp_if);
    ("while expression", `Quick, test_process_exp_while);
    ("case expression", `Quick, test_process_exp_case);
    ("fn expression (anonymous function)", `Quick, test_process_exp_fn);
  ]

let pattern_tests =
  [
    ("constant pattern", `Quick, test_process_pat_constant);
    ("wildcard pattern", `Quick, test_process_pat_wildcard);
    ("variable pattern", `Quick, test_process_pat_variable);
    ("nullary constructor pattern", `Quick, test_process_pat_constructor_nullary);
    ( "constructor pattern with argument",
      `Quick,
      test_process_pat_constructor_with_arg );
    (* TODO: Restore after implementing precedence resolution *)
    (* ("infix constructor pattern (::)", `Quick, test_process_pat_infix_cons); *)
    ("unit pattern (empty tuple)", `Quick, test_process_pat_tuple_empty);
    ("tuple pattern", `Quick, test_process_pat_tuple);
    ("record pattern", `Quick, test_process_pat_record);
    ( "record pattern with variable shorthand",
      `Quick,
      test_process_pat_record_var_shorthand );
    ("empty list pattern", `Quick, test_process_pat_list_empty);
    ("list pattern", `Quick, test_process_pat_list);
    ("typed pattern", `Quick, test_process_pat_typed);
    (* TODO: Restore after implementing precedence resolution *)
    (* ("as pattern (layered pattern)", `Quick, test_process_pat_as); *)
    ("as pattern with guess_var", `Quick, test_process_pat_as_with_guess_var);
    ("ref pattern", `Quick, test_process_pat_ref);
    ("nested ref patterns", `Quick, test_process_pat_ref_nested);
  ]

let declaration_tests =
  [
    ("simple value binding", `Quick, test_process_val_bind_simple);
    ("multiple value bindings (and)", `Quick, test_process_val_bind_multiple);
    ("simple function binding", `Quick, test_process_fun_bind_simple);
    ( "function with pattern matching",
      `Quick,
      test_process_fun_bind_pattern_match );
    ("simple type abbreviation", `Quick, test_process_typ_bind_simple);
    ("parametric type abbreviation", `Quick, test_process_typ_bind_parametric);
    ("simple datatype declaration", `Quick, test_process_dat_bind_simple);
    ("option datatype declaration", `Quick, test_process_dat_bind_option);
    ("simple exception declaration", `Quick, test_process_exn_bind_simple);
    ("exception with argument", `Quick, test_process_exn_bind_with_arg);
  ]

let program_tests =
  [
    ( "simple program with value declaration",
      `Quick,
      test_process_prog_simple_val );
    ("program with function declaration", `Quick, test_process_prog_fun);
    ("program with datatype declaration", `Quick, test_process_prog_datatype);
    ("program with sequential declarations", `Quick, test_process_prog_sequence);
  ]

let complex_type_tests =
  [
    ( "process complex function type (tuple -> bool)",
      `Quick,
      test_process_complex_function_type );
    ("process list type transformation", `Quick, test_process_list_type);
    ( "process higher-order function type",
      `Quick,
      test_process_higher_order_function );
  ]

(** Pattern matching tests using ppxlib metaquotation *)

(* Test that lambda expressions have the correct structure *)
let test_lambda_structure () =
  let input =
    FnExp
      (b
         (Case
            ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
              b (ExpIdx (b (IdxIdx (b "x")))),
              None )))
  in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_function _ -> () (* FnExp produces Pexp_function, not Pexp_fun *)
  | _ -> fail "Expected a function expression structure"

(* Test that let bindings produce the correct structure *)
let test_let_structure () =
  let input =
    LetExp
      ( [
          b
            (ValDec
               ( [],
                 b
                   (ValBind
                      ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
                        b (ExpCon (b (ConInt (b "42")))),
                        None )) ));
        ],
        [ b (ExpIdx (b (IdxIdx (b "x")))) ] )
  in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_let (Nonrecursive, [ _ ], _) -> ()
  | _ -> fail "Expected a non-recursive let binding structure"

(* Test that function application has correct structure *)
let test_app_structure () =
  (* Updated for list-based ExpApp *)
  let input =
    ExpApp [ b (ExpIdx (b (IdxIdx (b "f")))); b (ExpIdx (b (IdxIdx (b "x")))) ]
  in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_apply (_, [ _ ]) -> ()
  | _ -> fail "Expected a function application with one argument"

(* Test that case expressions produce match structures *)
let test_case_structure () =
  let input =
    CaseExp
      ( b (ExpIdx (b (IdxIdx (b "x")))),
        b (Case (b PatWildcard, b (ExpCon (b (ConInt (b "0")))), None)) )
  in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_match (_, _ :: _) -> ()
  | Pexp_function (_ :: _) -> () (* Could also be a function with cases *)
  | _ -> fail "Expected a match or function expression"

(* Test that if expressions have the correct structure *)
let test_if_structure () =
  let input =
    IfExp
      ( b (ExpIdx (b (IdxIdx (b "cond")))),
        b (ExpCon (b (ConInt (b "1")))),
        b (ExpCon (b (ConInt (b "2")))) )
  in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_ifthenelse (_, _, Some _) -> ()
  | _ -> fail "Expected if-then-else structure"

(* Test that tuple expressions produce tuple structures *)
let test_tuple_structure () =
  let input =
    TupleExp
      [ b (ExpCon (b (ConInt (b "1")))); b (ExpCon (b (ConInt (b "2")))) ]
  in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_tuple [ _; _ ] -> ()
  | _ -> fail "Expected a 2-element tuple structure"

(* Test that record expressions produce record structures *)
let test_record_structure () =
  let input =
    RecordExp
      [ b (Row (b (IdxLab (b "x")), b (ExpCon (b (ConInt (b "1")))), None)) ]
  in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_record (_, None) -> () (* Record expressions produce Pexp_record *)
  | _ -> fail "Expected a record expression structure"

(* Test that list expressions produce list structures *)
let test_list_structure () =
  let input =
    ListExp [ b (ExpCon (b (ConInt (b "1")))); b (ExpCon (b (ConInt (b "2")))) ]
  in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_construct ({ txt = Lident "::"; _ }, Some _) -> ()
  | _ -> fail "Expected a list cons structure"

(* Test that empty lists produce nil structures *)
let test_empty_list_structure () =
  let input = ListExp [] in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> ()
  | _ -> fail "Expected an empty list constructor"

(* Test that sequential expressions produce sequence structures *)
let test_seq_structure () =
  let input =
    SeqExp [ b (ExpCon (b (ConInt (b "1")))); b (ExpCon (b (ConInt (b "2")))) ]
  in
  let result = process_exp (b input) in
  match result.pexp_desc with
  | Pexp_sequence (_, _) -> ()
  | _ -> fail "Expected a sequence expression structure"

(* Test that type annotations produce constraint structures *)
let test_typed_exp_structure () =
  let input =
    TypedExp
      (b (ExpCon (b (ConInt (b "42")))), b (TypCon ([], b (IdxIdx (b "int")))))
  in
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
  let input = PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))) in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_var _ -> ()
  | _ -> fail "Expected variable pattern structure"

let test_tuple_pattern_structure () =
  let input =
    PatTuple
      [
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))));
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "y"))))));
      ]
  in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_tuple [ _; _ ] -> ()
  | _ -> fail "Expected 2-element tuple pattern structure"

let test_list_pattern_structure () =
  let input = PatList [ b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))) ] in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_construct ({ txt = Lident "::"; _ }, Some _) -> ()
  | _ -> fail "Expected list cons pattern structure"

let test_constant_pattern_structure () =
  let input = PatCon (b (ConInt (b "42"))) in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_constant _ -> ()
  | _ -> fail "Expected constant pattern structure"

let test_constructor_pattern_structure () =
  (* Updated for list-based PatApp *)
  let input =
    PatApp
      [
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "SOME"))))));
        b (PatIdx (b (WithoutOp (b (IdxIdx (b "x"))))));
      ]
  in
  let result = process_pat (b input) in
  match result.ppat_desc with
  | Ppat_construct (_, Some _) -> ()
  | _ -> fail "Expected constructor pattern with argument"

(* Test type structures *)

let test_function_type_structure () =
  let input =
    TypFun (b (TypVar (b (IdxVar (b "a")))), b (TypVar (b (IdxVar (b "b")))))
  in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_arrow (Nolabel, _, _) -> ()
  | _ -> fail "Expected arrow type structure"

let test_tuple_type_structure () =
  let input =
    TypTuple
      [ b (TypVar (b (IdxVar (b "a")))); b (TypVar (b (IdxVar (b "b")))) ]
  in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_tuple [ _; _ ] -> ()
  | _ -> fail "Expected 2-element tuple type structure"

let test_type_constructor_structure () =
  let input = TypCon ([], b (IdxIdx (b "int"))) in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> ()
  | _ -> fail "Expected simple type constructor 'int'"

let test_parametric_type_structure () =
  let input =
    TypCon ([ b (TypVar (b (IdxVar (b "a")))) ], b (IdxIdx (b "list")))
  in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ _ ]) -> ()
  | _ -> fail "Expected parametric type constructor 'list' with one argument"

let test_type_var_structure () =
  let input = TypVar (b (IdxVar (b "a"))) in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_var "a" -> ()
  | _ -> fail "Expected type variable 'a'"

let test_object_type_structure () =
  let input =
    TypRecord
      [
        b
          (TypRow
             (b (IdxLab (b "x")), b (TypCon ([], b (IdxIdx (b "int")))), None));
      ]
  in
  let result = process_type_value (b input) in
  match result.ptyp_desc with
  | Ptyp_object (_, _) -> ()
  | _ ->
      fail "Expected object type structure (SML records become OCaml objects)"

(* Test declaration structures *)

let test_val_declaration_structure () =
  let input =
    ValBind
      ( b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
        b (ExpCon (b (ConInt (b "42")))),
        None )
  in
  let result = process_val_bind input in
  match result with
  | [ binding ] -> (
      match binding.pvb_expr.pexp_desc with
      | Pexp_constant _ -> ()
      | _ -> fail "Expected constant expression in value binding")
  | _ -> fail "Expected a single value binding"

let test_fun_declaration_structure () =
  let input =
    FunBind
      ( b
          (FunMatchPrefix
             ( b (WithoutOp (b (IdxIdx (b "f")))),
               [ b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))) ],
               None,
               b (ExpIdx (b (IdxIdx (b "x")))),
               None )),
        None )
  in
  let result = process_fun_bind input in
  match result with
  | [ binding ] -> (
      match binding.pvb_expr.pexp_desc with
      | Pexp_fun _ -> ()
      | _ -> fail "Expected function expression in binding")
  | _ -> fail "Expected a single function binding"

let test_datatype_declaration_structure () =
  let input =
    DatBind
      ( [],
        b (IdxIdx (b "bool")),
        b
          (ConBind
             ( b (IdxIdx (b "True")),
               None,
               Some (b (ConBind (b (IdxIdx (b "False")), None, None))) )),
        None )
  in
  let result = process_dat_bind input in
  match result with
  | [ decl ] -> (
      match decl.ptype_kind with
      | Ptype_variant _ -> ()
      | _ -> fail "Expected variant type declaration")
  | _ -> fail "Expected a single type declaration"

let test_exception_declaration_structure () =
  let input = ExnBind (b (IdxIdx (b "Overflow")), None, None) in
  let result = process_exn_bind input in
  match result with
  | [ ext_cons ] -> (
      (* Check that we got an extension constructor (for exceptions) *)
      match ext_cons.pext_kind with
      | Pext_decl _ -> ()
      | _ -> fail "Expected exception declaration constructor")
  | _ -> fail "Expected a single extension constructor"

let pattern_matching_tests =
  [
    ("lambda has correct AST structure", `Quick, test_lambda_structure);
    ("let binding has correct AST structure", `Quick, test_let_structure);
    ( "function application has correct AST structure",
      `Quick,
      test_app_structure );
    ("case expression has correct AST structure", `Quick, test_case_structure);
    ("if expression has correct AST structure", `Quick, test_if_structure);
    ("tuple expression has correct AST structure", `Quick, test_tuple_structure);
    ( "record expression has correct AST structure",
      `Quick,
      test_record_structure );
    ("list expression has correct AST structure", `Quick, test_list_structure);
    ("empty list has correct AST structure", `Quick, test_empty_list_structure);
    ("sequence expression has correct AST structure", `Quick, test_seq_structure);
    ( "typed expression has correct AST structure",
      `Quick,
      test_typed_exp_structure );
    ( "wildcard pattern has correct AST structure",
      `Quick,
      test_wildcard_pattern_structure );
    ( "variable pattern has correct AST structure",
      `Quick,
      test_variable_pattern_structure );
    ( "tuple pattern has correct AST structure",
      `Quick,
      test_tuple_pattern_structure );
    ( "list pattern has correct AST structure",
      `Quick,
      test_list_pattern_structure );
    ( "constant pattern has correct AST structure",
      `Quick,
      test_constant_pattern_structure );
    ( "constructor pattern has correct AST structure",
      `Quick,
      test_constructor_pattern_structure );
    ( "function type has correct AST structure",
      `Quick,
      test_function_type_structure );
    ("tuple type has correct AST structure", `Quick, test_tuple_type_structure);
    ( "type constructor has correct AST structure",
      `Quick,
      test_type_constructor_structure );
    ( "parametric type has correct AST structure",
      `Quick,
      test_parametric_type_structure );
    ("type variable has correct AST structure", `Quick, test_type_var_structure);
    ("object type has correct AST structure", `Quick, test_object_type_structure);
    ( "val declaration has correct AST structure",
      `Quick,
      test_val_declaration_structure );
    ( "fun declaration has correct AST structure",
      `Quick,
      test_fun_declaration_structure );
    ( "datatype declaration has correct AST structure",
      `Quick,
      test_datatype_declaration_structure );
    ( "exception declaration has correct AST structure",
      `Quick,
      test_exception_declaration_structure );
  ]

(** {1 Comment Preservation Tests} *)

(** Helper to count comments in SML source *)
let count_sml_comments (source : string) : int =
  let rec find_all_occurrences str pattern start acc =
    try
      let idx = String.index_from str start '(' in
      if idx + 1 < String.length str && str.[idx + 1] = '*' then
        find_all_occurrences str pattern (idx + 2) (acc + 1)
      else find_all_occurrences str pattern (idx + 1) acc
    with Not_found -> acc
  in
  find_all_occurrences source "(*" 0 0

(** Helper to recursively count comment attributes in Parsetree *)
let count_parsetree_comments (structure : Parsetree.structure) : int =
  let rec count_structure_item item =
    match item.Parsetree.pstr_desc with
    | Pstr_attribute _ -> 1 (* Count all Pstr_attribute as comments *)
    | Pstr_value (_, bindings) ->
        List.fold_left ( + ) 0 (List.map count_value_binding bindings)
    | Pstr_eval (expr, _) -> count_expression expr
    | Pstr_module _ -> 0
    | Pstr_type _ -> 0
    | Pstr_exception _ -> 0
    | _ -> 0
  and count_value_binding vb =
    count_expression vb.Parsetree.pvb_expr + count_pattern vb.Parsetree.pvb_pat
  and count_expression expr =
    match expr.Parsetree.pexp_desc with
    | Pexp_let (_, bindings, body) ->
        List.fold_left ( + ) (count_expression body)
          (List.map count_value_binding bindings)
    | Pexp_sequence (e1, e2) -> count_expression e1 + count_expression e2
    | Pexp_apply (e, args) ->
        count_expression e
        + List.fold_left ( + ) 0
            (List.map (fun (_, e) -> count_expression e) args)
    | Pexp_tuple exprs ->
        List.fold_left ( + ) 0 (List.map count_expression exprs)
    | Pexp_match (e, cases) ->
        count_expression e + List.fold_left ( + ) 0 (List.map count_case cases)
    | Pexp_ifthenelse (e1, e2, e3_opt) -> (
        count_expression e1 + count_expression e2
        + match e3_opt with Some e3 -> count_expression e3 | None -> 0)
    | Pexp_fun (_, _, pat, e) -> count_pattern pat + count_expression e
    | Pexp_constraint (e, _) -> count_expression e
    | _ -> 0
  and count_pattern _pat =
    0 (* Patterns should not have comments after hoisting *)
  and count_case case =
    count_pattern case.Parsetree.pc_lhs + count_expression case.Parsetree.pc_rhs
  in
  List.fold_left ( + ) 0 (List.map count_structure_item structure)

(** Test that all comments are preserved after conversion *)
let test_all_comments_preserved () =
  let source = "(* A *)\nval x = 1 (* B *)\n(* C *)\nval y = 2\n(* D *)" in
  let expected_count = 4 in
  (* A, B, C, D *)

  let module TestCtx = struct
    let lexbuf = source
    let context = Context.basis_context
  end in
  let module TestBE = Backend.Make (TestCtx) (TestConfig) in
  let ast = Frontend.parse source in
  let result = TestBE.process_prog ast in
  let actual_count = count_parsetree_comments result in

  check int "all comments preserved" expected_count actual_count

(** Test that comments are hoisted to structure items *)
let test_comments_hoisted_to_structure () =
  let source = "val x = 1 (* inline comment *)" in

  let module TestCtx = struct
    let lexbuf = source
    let context = Context.basis_context
  end in
  let module TestBE = Backend.Make (TestCtx) (TestConfig) in
  let ast = Frontend.parse source in
  let result = TestBE.process_prog ast in

  (* Count structure-level comment attributes *)
  let structure_comment_count =
    List.fold_left
      (fun acc item ->
        match item.Parsetree.pstr_desc with
        | Pstr_attribute _ -> acc + 1
        | _ -> acc)
      0 result
  in

  (* Should have at least one structure-level comment *)
  check bool "comment hoisted to structure level" true
    (structure_comment_count > 0)

(** Test with multiple declarations and comments *)
let test_multiple_declarations_with_comments () =
  let source =
    "(* Header comment *)\n" ^ "val x = 1\n" ^ "(* Middle comment *)\n"
    ^ "val y = 2\n" ^ "(* Trailing comment *)"
  in
  let expected_count = 3 in

  let module TestCtx = struct
    let lexbuf = source
    let context = Context.basis_context
  end in
  let module TestBE = Backend.Make (TestCtx) (TestConfig) in
  let ast = Frontend.parse source in
  let result = TestBE.process_prog ast in
  let actual_count = count_parsetree_comments result in

  check int "multiple comments preserved" expected_count actual_count

(** Comment preservation test suite *)
let comment_preservation_tests =
  [
    ("all comments preserved", `Quick, test_all_comments_preserved);
    ( "comments hoisted to structure items",
      `Quick,
      test_comments_hoisted_to_structure );
    ( "multiple declarations with comments",
      `Quick,
      test_multiple_declarations_with_comments );
  ]

(** {1 Twelf Integration Tests} *)

(** Helper to find all SML files recursively in a directory *)
let find_sml_files (dir : string) : string list =
  let rec scan_directory acc path =
    let entries = Sys.readdir path in
    Array.fold_left
      (fun acc entry ->
        let full_path = Filename.concat path entry in
        if Sys.is_directory full_path then scan_directory acc full_path
        else if
          Filename.check_suffix entry ".sml"
          || Filename.check_suffix entry ".fun"
          || Filename.check_suffix entry ".sig"
        then full_path :: acc
        else acc)
      acc entries
  in
  let files = scan_directory [] dir in
  List.sort String.compare files

(** Create a test for a single Twelf file *)
let test_twelf_file (file_path : string) () : unit =
  (* Read file content *)
  let ic = open_in file_path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;

  (* Configure the converter *)
  let config =
    Common.create [
      Common.set Input_file (Common.File [ file_path ]);
      Common.set Output_file Common.Silent;
      Common.set Verbosity 0;
      Common.set Convert_names Enable;
      Common.set Convert_keywords Enable;
      Common.set Rename_types Enable;
      Common.set Guess_pattern Enable;
      Common.set Guess_var (Some {|[A-Z]s?[0-9]?'?|})
    ]
  in

  try
    (* Parse SML using Frontend *)
    let sml_ast = Frontend.parse content in

    (* Create backend module for conversion *)
    let module TestCtx = struct
      let lexbuf = content
      let context = Context.basis_context
    end in
    let module TestCfg : Common.CONFIG = struct
      let config = config
    end in
    let module TestBackend = Backend.Make (TestCtx) (TestCfg) in
    (* Convert to OCaml *)
    let ocaml_ast = TestBackend.process_prog sml_ast in

    (* Print OCaml code *)
    let buf = Buffer.create 4096 in
    let fmt = Format.formatter_of_buffer buf in
    List.iter
      (fun item ->
        Ppxlib.Pprintast.structure_item fmt item;
        Format.pp_print_newline fmt ())
      ocaml_ast;
    Format.pp_print_flush fmt ();
    let ocaml_code' = Buffer.contents buf in
    let ocaml_code = Polish.polish ocaml_code' in

    (* Check if the generated OCaml is valid by parsing it *)
    begin try
      let lexbuf = Lexing.from_string ~with_positions:true ocaml_code in
      Lexing.set_filename lexbuf file_path;
      let _ = Parse.use_file lexbuf in
      () (* Test passes - OCaml code is valid *)
    with
    | Syntaxerr.Error e ->
        let buf = Buffer.create 256 in
        let fmt = Format.formatter_of_buffer buf in
        Location.report_exception fmt (Syntaxerr.Error e);
        Format.pp_print_flush fmt ();
        let error_msg = Buffer.contents buf in
        fail (Printf.sprintf "Generated invalid OCaml code:\n%s" error_msg)
    | e ->
        fail
          (Printf.sprintf "Error validating OCaml syntax: %s"
             (Printexc.to_string e))
    end
  with
  | Lexer.Error (_, _loc) -> fail "Lexing error"
  | Parser.Error -> fail "Parsing error"
  | e ->
      fail
        (Printf.sprintf "Conversion failed: %s\n%s" (Printexc.to_string e)
           (Printexc.get_backtrace ()))

(** Generate test cases for all Twelf files *)
let twelf_tests =
  let twelf_dir = "examples/input/twelf/src" in
  if Sys.file_exists twelf_dir && Sys.is_directory twelf_dir then
    let files = find_sml_files twelf_dir in
    List.map
      (fun file_path ->
        let rel_path =
          if String.length file_path > String.length twelf_dir + 1 then
            String.sub file_path
              (String.length twelf_dir + 1)
              (String.length file_path - String.length twelf_dir - 1)
          else file_path
        in
        (rel_path, `Quick, test_twelf_file file_path))
      files
  else
    (* If directory doesn't exist, create a single failing test *)
    [
      ( "twelf directory not found",
        `Quick,
        fun () ->
          fail (Printf.sprintf "Twelf directory not found: %s" twelf_dir) );
    ]

(** Precedence Resolver Tests *)

(** Helper to create resolved expression testable *)
let resolved_exp_to_string (resolved : PR.resolved_exp) : string =
  let rec aux = function
    | PR.ResolvedSingle e -> Ast.show_expression e
    | PR.ResolvedApp (f, args) ->
        Printf.sprintf "App(%s, [%s])" (aux f)
          (String.concat "; "
             (List.map (fun n -> Ast.show_expression n.value) args))
    | PR.ResolvedInfix (left, op, right) ->
        Printf.sprintf "Infix(%s, %s, %s)" (aux left) (Ast.show_idx op.value)
          (aux right)
  in
  aux resolved

let resolved_exp_testable : PR.resolved_exp testable =
  testable
    (fun fmt re -> Format.fprintf fmt "%s" (resolved_exp_to_string re))
    (fun a b -> resolved_exp_to_string a = resolved_exp_to_string b)

(** Test: 1 + 2 * 3 should parse as 1 + (2 * 3) *)
let test_precedence_mult_higher_than_add () =
  (* Input sequence: [1; +; 2; *; 3] *)
  let one = b (ExpCon (b (ConInt (b "1")))) in
  let plus = b (ExpIdx (b (IdxIdx (b "+")))) in
  let two = b (ExpCon (b (ConInt (b "2")))) in
  let mult = b (ExpIdx (b (IdxIdx (b "*")))) in
  let three = b (ExpCon (b (ConInt (b "3")))) in
  let input = [ one; plus; two; mult; three ] in

  let result = PR.resolve_precedence input in

  (* Expected: Infix(1, +, Infix(2, *, 3)) *)
  let expected =
    PR.ResolvedInfix
      ( PR.ResolvedSingle (ExpCon (b (ConInt (b "1")))),
        b (IdxIdx (b "+")),
        PR.ResolvedInfix
          ( PR.ResolvedSingle (ExpCon (b (ConInt (b "2")))),
            b (IdxIdx (b "*")),
            PR.ResolvedSingle (ExpCon (b (ConInt (b "3")))) ) )
  in

  check resolved_exp_testable "multiplication binds tighter than addition"
    expected result

(** Test: 1 + 2 + 3 should parse as (1 + 2) + 3 (left-associative) *)
let test_left_associative_addition () =
  (* Input sequence: [1; +; 2; +; 3] *)
  let one = b (ExpCon (b (ConInt (b "1")))) in
  let plus1 = b (ExpIdx (b (IdxIdx (b "+")))) in
  let two = b (ExpCon (b (ConInt (b "2")))) in
  let plus2 = b (ExpIdx (b (IdxIdx (b "+")))) in
  let three = b (ExpCon (b (ConInt (b "3")))) in
  let input = [ one; plus1; two; plus2; three ] in

  let result = PR.resolve_precedence input in

  (* Expected: Infix(Infix(1, +, 2), +, 3) *)
  let expected =
    PR.ResolvedInfix
      ( PR.ResolvedInfix
          ( PR.ResolvedSingle (ExpCon (b (ConInt (b "1")))),
            b (IdxIdx (b "+")),
            PR.ResolvedSingle (ExpCon (b (ConInt (b "2")))) ),
        b (IdxIdx (b "+")),
        PR.ResolvedSingle (ExpCon (b (ConInt (b "3")))) )
  in

  check resolved_exp_testable "addition is left-associative" expected result

(** Test: 1 :: 2 :: 3 should parse as 1 :: (2 :: 3) (right-associative) *)
let test_right_associative_cons () =
  (* Input sequence: [1; ::; 2; ::; 3] *)
  let one = b (ExpCon (b (ConInt (b "1")))) in
  let cons1 = b (ExpIdx (b (IdxIdx (b "::")))) in
  let two = b (ExpCon (b (ConInt (b "2")))) in
  let cons2 = b (ExpIdx (b (IdxIdx (b "::")))) in
  let three = b (ExpCon (b (ConInt (b "3")))) in
  let input = [ one; cons1; two; cons2; three ] in

  let result = PR.resolve_precedence input in

  (* Expected: Infix(1, ::, Infix(2, ::, 3)) *)
  let expected =
    PR.ResolvedInfix
      ( PR.ResolvedSingle (ExpCon (b (ConInt (b "1")))),
        b (IdxIdx (b "::")),
        PR.ResolvedInfix
          ( PR.ResolvedSingle (ExpCon (b (ConInt (b "2")))),
            b (IdxIdx (b "::")),
            PR.ResolvedSingle (ExpCon (b (ConInt (b "3")))) ) )
  in

  check resolved_exp_testable "cons (::) is right-associative" expected result

(** Test: f x y should parse as (f x) y (function application) *)
let test_function_application () =
  (* Input sequence: [f; x; y] - no operators, pure application *)
  let f = b (ExpIdx (b (IdxIdx (b "f")))) in
  let x = b (ExpIdx (b (IdxIdx (b "x")))) in
  let y = b (ExpIdx (b (IdxIdx (b "y")))) in
  let input = [ f; x; y ] in

  let result = PR.resolve_precedence input in

  (* Expected: App(f, [x; y]) *)
  let expected =
    PR.ResolvedApp
      ( PR.ResolvedSingle (ExpIdx (b (IdxIdx (b "f")))),
        [ b (ExpIdx (b (IdxIdx (b "x")))); b (ExpIdx (b (IdxIdx (b "y")))) ] )
  in

  check resolved_exp_testable "function application is left-associative"
    expected result

(** Test: 1 + 2 = 3 should parse as (1 + 2) = 3 (+ higher precedence than =) *)
let test_addition_higher_than_equality () =
  (* Input sequence: [1; +; 2; =; 3] *)
  let one = b (ExpCon (b (ConInt (b "1")))) in
  let plus = b (ExpIdx (b (IdxIdx (b "+")))) in
  let two = b (ExpCon (b (ConInt (b "2")))) in
  let eq = b (ExpIdx (b (IdxIdx (b "=")))) in
  let three = b (ExpCon (b (ConInt (b "3")))) in
  let input = [ one; plus; two; eq; three ] in

  let result = PR.resolve_precedence input in

  (* Expected: Infix(Infix(1, +, 2), =, 3) *)
  let expected =
    PR.ResolvedInfix
      ( PR.ResolvedInfix
          ( PR.ResolvedSingle (ExpCon (b (ConInt (b "1")))),
            b (IdxIdx (b "+")),
            PR.ResolvedSingle (ExpCon (b (ConInt (b "2")))) ),
        b (IdxIdx (b "=")),
        PR.ResolvedSingle (ExpCon (b (ConInt (b "3")))) )
  in

  check resolved_exp_testable "addition higher precedence than equality"
    expected result

(** Helper for resolved pattern testable *)
let resolved_pat_to_string (resolved : PR.resolved_pat) : string =
  let rec aux = function
    | PR.ResolvedPatSingle p -> Ast.show_pat p
    | PR.ResolvedPatApp (f, args) ->
        Printf.sprintf "PatApp(%s, [%s])" (aux f)
          (String.concat "; " (List.map (fun n -> Ast.show_pat n.value) args))
    | PR.ResolvedPatInfix (left, op, right) ->
        Printf.sprintf "PatInfix(%s, %s, %s)" (aux left) (Ast.show_idx op.value)
          (aux right)
  in
  aux resolved

let resolved_pat_testable : PR.resolved_pat testable =
  testable
    (fun fmt rp -> Format.fprintf fmt "%s" (resolved_pat_to_string rp))
    (fun a b -> resolved_pat_to_string a = resolved_pat_to_string b)

(** Test: x :: xs pattern should parse as infix cons *)
let test_pattern_cons () =
  (* Input sequence: [x; ::; xs] *)
  let x = b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))) in
  let cons = b (PatIdx (b (WithoutOp (b (IdxIdx (b "::")))))) in
  let xs = b (PatIdx (b (WithoutOp (b (IdxIdx (b "xs")))))) in
  let input = [ x; cons; xs ] in

  let result = PR.resolve_pat_precedence input in

  (* Expected: PatInfix(x, ::, xs) *)
  let expected =
    PR.ResolvedPatInfix
      ( PR.ResolvedPatSingle (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))),
        b (IdxIdx (b "::")),
        PR.ResolvedPatSingle (PatIdx (b (WithoutOp (b (IdxIdx (b "xs")))))) )
  in

  check resolved_pat_testable "pattern cons (::) resolves correctly" expected
    result

(** Test: Some x pattern should parse as constructor application *)
let test_pattern_constructor_app () =
  (* Input sequence: [Some; x] - no operators, pure pattern application *)
  let some = b (PatIdx (b (WithoutOp (b (IdxIdx (b "Some")))))) in
  let x = b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))) in
  let input = [ some; x ] in

  let result = PR.resolve_pat_precedence input in

  (* Expected: PatApp(Some, [x]) *)
  let expected =
    PR.ResolvedPatApp
      ( PR.ResolvedPatSingle (PatIdx (b (WithoutOp (b (IdxIdx (b "Some")))))),
        [ b (PatIdx (b (WithoutOp (b (IdxIdx (b "x")))))) ] )
  in

  check resolved_pat_testable "constructor application in patterns" expected
    result

let precedence_resolver_tests =
  [
    ("1 + 2 * 3 precedence", `Quick, test_precedence_mult_higher_than_add);
    ("1 + 2 + 3 left-assoc", `Quick, test_left_associative_addition);
    ("1 :: 2 :: 3 right-assoc", `Quick, test_right_associative_cons);
    ("f x y function application", `Quick, test_function_application);
    ("1 + 2 = 3 precedence levels", `Quick, test_addition_higher_than_equality);
    ("x :: xs pattern", `Quick, test_pattern_cons);
    ("Some x pattern", `Quick, test_pattern_constructor_app);
  ]

(** Main test runner *)

let run_unit_tests () : unit =
  Alcotest.run "Backend"
    [
      ("Constant Processing", constant_tests);
      ("Type Processing", type_tests);
      ("Object Field Processing", object_field_tests);
      ("Expression Processing", expression_tests);
      ("Pattern Processing", pattern_tests);
      ("Declaration Processing", declaration_tests);
      ("Program Processing", program_tests);
      ("Complex Type Processing", complex_type_tests);
      ("Pattern Matching (AST Structure)", pattern_matching_tests);
      ("Comment Preservation", comment_preservation_tests);
      ("Precedence Resolver", precedence_resolver_tests);
      ("Twelf Integration", twelf_tests);
    ]

let () = run_unit_tests ()
