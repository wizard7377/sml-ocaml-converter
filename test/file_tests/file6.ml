include Test_common
module TestArraySlice : TEST_FILES = struct
  let test_name = {|ArraySlice - Type Alias and Simple Functions|}
  let input = {sml|
structure ArraySlice :> ARRAY_SLICE =
struct
  type 'a slice = 'a Array.array * int * int option
  fun slice s = s
  val appi = Array.appi
end;
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestArraySlice)
