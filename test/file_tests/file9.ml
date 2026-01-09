include Test_common
module TestIndex : TEST_FILES = struct
  let test_name = {|Index - Functor with Multiple Structures|}
  let input = {sml|
structure Index =
  Index (structure Global = Global
         structure Queue = Queue);

structure IndexSkolem =
  IndexSkolem (structure Global = Global
               structure Queue = Queue);
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestIndex)
