include Alcotest
include Ast
include Shibboleth
include Backend
include Frontend

module type TEST_FILES = sig
  val test_name : string
  val input : string
  val expected_output : string option 
end

module type TEST_CASE = sig
include TEST_FILES
  val run_test : unit test_case
end
let test_config = Cli.test_config
module TestConfig : Common.CONFIG = struct
  let config = test_config 
end

module TestStore (* TODO *) = struct
  let context = Context.create []
end
(** Instantiate Backend with test config *)
module TestBackend = Backend.Make(TestStore)(TestConfig)
open TestBackend
let process (before:string) : string =
  let middle = Re.replace ~all:true (Re.compile (Re.rep1 Re.space)) ~f:(fun s -> " ") before in 
  let after = Re.replace ~all:true (Re.compile (Re.str ";;")) ~f:(fun s -> "") middle in
  String.trim after

let compare_files (actual:string) (expected:string) : bool =
  let processed_actual = process actual in
  let processed_expected = process expected in
  (actual == expected)

let string_format : string Fmt.t = Fmt.string
let test_files = Alcotest.testable string_format compare_files
module Make (Files : TEST_FILES) : TEST_CASE = struct
  include Files

  let run_test : unit test_case = test_case test_name `Quick (fun () ->
    let parsed_sml = Frontend.parse input in
    let ocaml_ast = TestBackend.process_sml ~prog:parsed_sml in
    let buffer = Buffer.create 256 in
    let fmt = Format.formatter_of_buffer buffer in
    List.iter (Astlib.Pprintast.top_phrase fmt) ocaml_ast;
    Format.pp_print_flush fmt ();
    let actual = Buffer.contents buffer in
    let processed_actual =  actual in
    let test_name' = Re.replace ~all:true (Re.compile (Re.str " ")) ~f:(fun s -> "_") test_name in
    match expected_output with
    | None -> Alcotest.(check pass) test_name' input processed_actual
    | Some expected ->
        let processed_expected = expected in
        Alcotest.(check test_files) test_name processed_expected processed_actual)
end
