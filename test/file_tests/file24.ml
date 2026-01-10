include Test_common

module Test24 : TEST_FILES = struct
  let test_name = "Comment Preservation"
  let input_file = "file24_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(Test24)
