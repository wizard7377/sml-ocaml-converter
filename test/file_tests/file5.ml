include Test_common

module TestTimeLimit : TEST_FILES = struct
  let test_name = "TimeLimit - Exception and Lambda"
  let input_file = "file5_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestTimeLimit)
