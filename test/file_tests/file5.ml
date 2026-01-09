include Test_common
module TestTimeLimit : TEST_FILES = struct
  let test_name = {|TimeLimit - Exception and Lambda|}
  let input = {sml|
structure TimeLimit :> TIME_LIMIT =
struct
  exception TimeOut
  val timeLimit = fn t => fn f => fn x => f(x)
end;
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestTimeLimit)
