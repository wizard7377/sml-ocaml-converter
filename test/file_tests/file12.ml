include Test_common

module TestOrigins : TEST_FILES = struct
  let test_name = "Origins - Functor Application"
  let input_file = "file12_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make(TestOrigins)
