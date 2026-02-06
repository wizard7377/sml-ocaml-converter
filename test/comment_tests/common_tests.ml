include Alcotest
include Ast
include Shibboleth
include Backend
include Frontend

type test_string = {
  content : string;
  start_percent : float;  (** Start position as a percentage of the file *)
  end_percent : float;    (** End position as a percentage of the file *)
}
(** Module type for file-based tests *)
module type TEST_FILES = sig
  val test_name : string
  val input_file : string 
  (** Expected output strings with their place thoughout the file as a percentage *)
  val expected_strings : test_string list 
end

module type TEST_CASE = sig 
  val test_name : string
  val run_test : unit test_case
end

let test_config = Common.make () 

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
  let rec check_group (grp : Re.Group.t) (expected: test_string list) : unit =
    match expected with
    | [] -> ()
    | { content = expected_content; start_percent; end_percent } :: rest ->
        let group_content = Re.Group.get grp 0 in
        assert (group_content = expected_content);
        let possible_matches = List.filter (fun s -> s.content = group_content) expected in
        let start_percent_actual = 
          let group_start = Re.Group.start grp 0 in
          let total_length = String.length group_content in
          (float_of_int group_start) /. (float_of_int total_length)
        in
        let end_percent_actual =
          let group_end = Re.Group.stop grp 0 in
          let total_length = String.length group_content in
          (float_of_int group_end) /. (float_of_int total_length)
        in
        match possible_matches with
        | [] -> Alcotest.failf "Unexpected comment found: %s" group_content
        | _ -> let has_good = List.exists (fun s -> s.start_percent <= start_percent_actual && s.end_percent >= end_percent_actual) possible_matches in 
            if not has_good then 
              Alcotest.failf "Comment found with incorrect position: %s" group_content
            else 
              Alcotest.(check pass) "Comment found with correct position" () ();
        check_group grp rest
  let rec check_comments (content: string) (expected: test_string list) : unit =
    match expected with
    | [] -> ()
    | { content = expected_content; start_percent; end_percent } :: rest ->
        let total_length = String.length content in
        let start_index = int_of_float (start_percent *. float_of_int total_length) in
        let end_index = int_of_float (end_percent *. float_of_int total_length) in
        let contains = Re.all (Re.compile (Re.str expected_content)) content in
        List.iter (fun c -> check_group c expected) contains
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
        check_comments processed_actual Files.expected_strings);
        
end
