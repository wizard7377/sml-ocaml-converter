include Test_common
module TestFlit : TEST_FILES = struct
  let test_name = {|Flit - Large Functor Application|}
  let input = {sml|
structure Flit =
  Flit (structure Global = Global
        structure Word = Word32
        structure Pack = PackWord32Little
        structure IntSyn = IntSyn
        structure Whnf = Whnf
        structure Print = Print
        structure Names = Names
        structure Index = Index
        structure Table = IntRedBlackTree)
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestFlit)
