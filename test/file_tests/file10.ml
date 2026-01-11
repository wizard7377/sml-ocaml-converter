include Test_common

module TestTabledSyn : TEST_FILES = struct
  let test_name = "TabledSyn - Functor Application"
  let input_file = "file10_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestTabledSyn)
