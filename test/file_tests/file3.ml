include Test_common

module TestModeSyn : TEST_FILES = struct
  let test_name = "ModeSyn - Datatypes and Pattern Matching"
  let input_file = "file3_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestModeSyn)
