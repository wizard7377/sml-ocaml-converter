include Test_common

module TestEVarLookup : TEST_FILES = struct
  let test_name = "EVar - Nested Let and Pattern Matching"
  let input_file = "file22_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestEVarLookup)
