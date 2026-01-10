include Test_common

module TestCompat : TEST_FILES = struct
  let test_name = "Compat - Functor with Many Structures"
  let input_file = "file15_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestCompat)
