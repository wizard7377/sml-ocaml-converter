include Test_common
module TestCompat : TEST_FILES = struct
  let test_name = {|Compat - Functor with Many Structures|}
  let input = {sml|
structure Compat :> COMPAT =
  Compat (structure Array = Array
          structure Vector = Vector
          structure Path = OS.Path
          structure Substring = Substring
          structure TextIO = TextIO
          structure Timer = Timer
          structure SocketIO = CompatSocketIO);
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestCompat)
