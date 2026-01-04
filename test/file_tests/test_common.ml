include Alcotest
include Ast
include Shibboleth
include Backend
include Frontend

module type TEST_FILES = sig
  val test_name : string
  val input : string
  val expected_output : string
end

module type TEST_CASE = sig
include TEST_FILES
  val run_test : unit test_case
end
let test_config = {
    Common.input_file = "";
    output_file = None;
    verbosity = Some 2;
    conversions = {
      pattern_names = Common.Do_convert;
      constructor_names_values = Common.Do_convert;
      function_names = Common.Do_convert;
      uncurry_types = Common.Do_convert;
      uncurry_values = Common.Do_convert;
    };
  }
module TestConfig : Common.CONFIG = struct
  let config = test_config 
end

(** Instantiate Backend with test config *)
module TestBackend = Backend.Make(TestConfig)
open TestBackend
let process (before:string) : string =
  let middle = Re.replace ~all:true (Re.compile (Re.rep1 Re.space)) ~f:(fun s -> " ") before in 
  let after = Re.replace ~all:true (Re.compile (Re.str ";;")) ~f:(fun s -> "") middle in
  String.trim after

let compare_files (actual:string) (expected:string) : bool =
  let processed_actual = process actual in
  let processed_expected = process expected in
  (actual == expected)
let test_files = Alcotest.testable Fmt.string compare_files
module Make (Files : TEST_FILES) : TEST_CASE = struct
  include Files

  let run_test : unit test_case = test_case test_name `Quick (fun () ->
    let parsed_sml = Frontend.parse input in
    let module Backend = Backend.Make(struct let config = test_config end) in
    let ocaml_ast = Backend.process_sml ~prog:parsed_sml in
    let buffer = Buffer.create 256 in
    let fmt = Format.formatter_of_buffer buffer in
    List.iter (Astlib.Pprintast.top_phrase fmt) ocaml_ast;
    Format.pp_print_flush fmt ();
    let actual = Buffer.contents buffer in
    let processed_actual =  actual in
    let processed_expected =  expected_output in
    Alcotest.(check test_files) test_name processed_expected processed_actual)
end
