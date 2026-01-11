include Test_common

module TestFixity : TEST_FILES = struct
  let test_name = "Fixity - Nested Datatypes and Functions"
  let input_file = "file19_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestFixity)
