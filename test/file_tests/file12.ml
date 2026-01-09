include Test_common
module TestOrigins : TEST_FILES = struct
  let test_name = {|Origins - Functor Application|}
  let input = {sml|
structure Origins =
  Origins (structure Global = Global
           structure Table = StringHashTable);
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestOrigins)
