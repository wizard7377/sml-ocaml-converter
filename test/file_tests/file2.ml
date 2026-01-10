include Test_common

module TestFiles : TEST_FILES = struct
  let test_name = "TEST 1"
  let input_file = "file2_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestFiles)
