include Test_common
module TestStyleCheck : TEST_FILES = struct
  let test_name = {|StyleCheck - Functor Application|}
  let input = {sml|
structure StyleCheck =
  StyleCheck (structure Whnf = Whnf
              structure Index = Index
              structure Origins = Origins);
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestStyleCheck)
