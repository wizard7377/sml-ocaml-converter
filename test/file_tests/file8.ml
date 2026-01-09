include Test_common
module TestDomains : TEST_FILES = struct
  let test_name = {|Domains - Functor Applications|}
  let input = {sml|
structure Integers = Integers(IntInf);

structure Rationals = Rationals(Integers);

structure IntegersMod7 = IntegersMod(val p = 7);
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestDomains)
