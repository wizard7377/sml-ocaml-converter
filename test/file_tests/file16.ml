include Test_common
module TestOrder : TEST_FILES = struct
  let test_name = {|Order - Functor Application with Table|}
  let input = {sml|
structure Order =
  Order (structure Table = IntRedBlackTree);
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestOrder)
