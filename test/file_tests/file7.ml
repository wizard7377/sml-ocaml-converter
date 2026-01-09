include Test_common
module TestSigINT : TEST_FILES = struct
  let test_name = {|SigINT - Simple Let Expression|}
  let input = {sml|
structure SigINT :> SIGINT =
struct

  fun interruptLoop (loop:unit -> unit) =
     let
     in
        loop ()
     end

end;
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestSigINT)
