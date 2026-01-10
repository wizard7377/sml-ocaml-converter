include Test_common

module TestArraySlice : TEST_FILES = struct
  let test_name = "ArraySlice - Type Alias and Simple Functions"
  let input_file = "file6_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestArraySlice)

