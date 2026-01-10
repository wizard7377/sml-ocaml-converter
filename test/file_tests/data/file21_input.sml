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
