include Test_common

module TestWeaken : TEST_FILES = struct
  let test_name = "Weaken - Functor with Local Declarations"
  let input_file = "file14_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestWeaken)
