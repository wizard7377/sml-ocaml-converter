include Test_common

module TestIndex : TEST_FILES = struct
  let test_name = "Index - Functor with Multiple Structures"
  let input_file = "file9_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestIndex)
