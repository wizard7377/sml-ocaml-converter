include Test_common

module TestQid : TEST_FILES = struct
  let test_name = "Qid - Datatype and String Operations"
  let input_file = "file20_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestQid)
