include Alcotest
include Ast
include Shibboleth
include Backend
include Frontend

(** Module type for file-based tests *)
module type TEST_FILES = sig
  val test_name : string
  val input_file : string
  val expected_file : string option
end

module type TEST_CASE = sig
  val test_name : string
  val run_test : unit test_case
end

let test_config = Common.mkOptions ()

module TestConfig : Common.CONFIG = struct
  let config = test_config
end

module TestStore (* TODO *) = struct
  let lexbuf = ""
  let context = Context.create (Context.Info.create [])
end

(** Instantiate Backend with test config *)
module Make_TestBackend (Ctx : sig
  val lexbuf : string
end) =
  Backend.Make
    (struct
      let lexbuf = Ctx.lexbuf
      let context = Context.create (Context.Info.create [])
    end)
    (TestConfig)

let process (before : string) : string =
  let middle =
    Re.replace ~all:true
      (Re.compile (Re.rep1 Re.space))
      ~f:(fun _ -> " ")
      before
  in
  let after =
    Re.replace ~all:true (Re.compile (Re.str ";;")) ~f:(fun _ -> "") middle
  in
  String.trim after

let compare_files (actual : string) (expected : string) : bool =
  let _processed_actual = process actual in
  let _processed_expected = process expected in
  actual == expected

let string_format : string Fmt.t = Fmt.string
let test_files = Alcotest.testable string_format compare_files

(** Get the directory containing the test data files *)
let data_dir =
  let test_dir = Filename.dirname __FILE__ in
  test_dir ^ "/data"

(** Read a file from the data directory *)
let read_data_file filename =
  let path = Filename.concat data_dir filename in
  In_channel.with_open_text path In_channel.input_all

(** Make a test case from file-based test specification *)
module Make (Files : TEST_FILES) : TEST_CASE = struct
  let test_name = Files.test_name

  let run_test : unit test_case =
    test_case test_name `Quick (fun () ->
        let input = read_data_file Files.input_file in
        let parsed_sml = Frontend.parse input in
        let module TestBackend = Make_TestBackend (struct
          let lexbuf = input
        end) in
        let ocaml_ast = TestBackend.process_sml ~prog:parsed_sml in
        let buffer = Buffer.create 256 in
        let fmt = Format.formatter_of_buffer buffer in
        List.iter (Astlib.Pprintast.top_phrase fmt) ocaml_ast;
        Format.pp_print_flush fmt ();
        let actual = Buffer.contents buffer in
        let processed_actual = actual in
        let test_name' =
          Re.replace ~all:true
            (Re.compile (Re.str " "))
            ~f:(fun _ -> "_")
            test_name
        in
        Format.fprintf Format.err_formatter "@,@,@[Running test: @[%s@]@]@,@,"
          test_name';
        Format.fprintf Format.err_formatter "@,@,@[Processed: %s@]@,@,"
          processed_actual;
        match Files.expected_file with
        | None ->
            Format.fprintf Format.err_formatter "From @[%s@]@," Files.input_file;
            Alcotest.(check pass) test_name' input processed_actual
        | Some expected_filename ->
            let expected = read_data_file expected_filename in
            let processed_expected = expected in
            Alcotest.(check test_files)
              test_name processed_expected processed_actual)
end
