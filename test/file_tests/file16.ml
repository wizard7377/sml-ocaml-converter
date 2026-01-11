include Test_common

module TestOrder : TEST_FILES = struct
  let test_name = "Order - Functor Application with Table"
  let input_file = "file16_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestOrder)
