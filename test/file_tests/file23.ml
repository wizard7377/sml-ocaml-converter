include Test_common
module TestLimitToString : TEST_FILES = struct
  let test_name = {|Limit - Option Pattern Matching|}
  let input = {sml|
structure Params =
struct
  fun getLimit ("*"::nil) = NONE
    | getLimit (t::ts) = SOME (t)
    | getLimit (nil) = NONE

  fun limitToString (NONE) = "*"
    | limitToString (SOME(i)) = Int.toString i

  fun strategyToString FRS = "FRS"
    | strategyToString RFS = "RFS"
end
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestLimitToString)
