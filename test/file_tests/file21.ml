include Test_common

module TestSplitLocal : TEST_FILES = struct
  let test_name = "Split - Local with Ref Lists"
  let input_file = "file21_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestSplitLocal)
