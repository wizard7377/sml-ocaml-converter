include Test_common

module TestLimitToString : TEST_FILES = struct
  let test_name = "Limit - Option Pattern Matching"
  let input_file = "file23_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestLimitToString)
