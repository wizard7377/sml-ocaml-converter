include Test_common

module TestDomains : TEST_FILES = struct
  let test_name = "Domains - Functor Applications"
  let input_file = "file8_input.sml"
  let expected_file = None
end

module TestCase = Test_common.Make (TestDomains)
