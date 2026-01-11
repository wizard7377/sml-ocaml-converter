include Test_common

module Test1 : TEST_FILES = struct
  let test_name = "TEST 1"
  let input_file = "file1_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (Test1)
