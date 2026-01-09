include Test_common
module TestTabledSyn : TEST_FILES = struct
  let test_name = {|TabledSyn - Functor Application|}
  let input = {sml|
structure TabledSyn =
  TabledSyn (structure Names = Names
             structure Table = IntRedBlackTree
             structure Index = Index);
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestTabledSyn)
