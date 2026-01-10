include Test_common

module TestStyleCheck : TEST_FILES = struct
  let test_name = "StyleCheck - Functor Application"
  let input_file = "file17_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestStyleCheck)
