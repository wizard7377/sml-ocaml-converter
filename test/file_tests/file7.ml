include Test_common

module TestSigINT : TEST_FILES = struct
  let test_name = "SigINT - Simple Let Expression"
  let input_file = "file7_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestSigINT)
