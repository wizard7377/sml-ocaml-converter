include Test_common
module TestSplitLocal : TEST_FILES = struct
  let test_name = {|Split - Local with Ref Lists|}
  let input = {sml|
structure Split =
struct
  local
    val caseList : (int * string) list ref = ref nil
  in
    fun resetCases () = (caseList := nil)
    fun addCase (Psi, t) = (caseList := (Psi, t) :: !caseList)
    fun getCases () = (!caseList)
  end
end
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestSplitLocal)
