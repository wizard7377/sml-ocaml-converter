include Test_common
module TestData : TEST_FILES = struct
  let test_name = {|Data - Simple Structure with Refs|}
  let input = {sml|
structure Data : DATA =
struct
  val maxFill = ref 5
  val maxSplit = ref 5
  val maxRecurse = ref 2
end;
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestData)
