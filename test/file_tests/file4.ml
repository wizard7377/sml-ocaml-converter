include Test_common

module TestData : TEST_FILES = struct
  let test_name = "Data - Simple Structure with Refs"
  let input_file = "file4_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestData)
