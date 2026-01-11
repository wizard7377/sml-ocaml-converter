include Test_common

module TestModSyn : TEST_FILES = struct
  let test_name = "ModSyn - Complex Functor Application"
  let input_file = "file13_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestModSyn)
