include Test_common

module TestServerPatterns : TEST_FILES = struct
  let test_name = "Server - Complex Pattern Matching"
  let input_file = "file18_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestServerPatterns)
