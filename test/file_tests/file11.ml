include Test_common

module TestFlit : TEST_FILES = struct
  let test_name = "Flit - Large Functor Application"
  let input_file = "file11_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestFlit)
