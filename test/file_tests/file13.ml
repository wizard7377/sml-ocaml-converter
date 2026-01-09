include Test_common
module TestModSyn : TEST_FILES = struct
  let test_name = {|ModSyn - Complex Functor Application|}
  let input = {sml|
structure ModSyn =
  ModSyn (structure Global = Global
          structure Names' = Names
          structure Origins = Origins
          structure Whnf = Whnf
          structure Strict = Strict
          structure IntTree = IntRedBlackTree
          structure HashTable = StringHashTable);
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestModSyn)
