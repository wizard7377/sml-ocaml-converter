structure Names =
struct
  local
    val evarList : (int * string) list ref = ref nil

    fun evarReset () = (evarList := nil)

    fun evarLookup (X) =
        let fun evlk (r, nil) = NONE
              | evlk (r, (r', name)::l) =
                if r = r' then SOME(name) else evlk (r, l)
        in
          evlk (X, (!evarList))
        end

    fun evarInsert entry = (evarList := entry::(!evarList))

    fun namedEVars () = !evarList

  in
    val varReset = evarReset
    val addEVar = evarInsert
    val getEVarOpt = evarLookup
    val namedEVars = namedEVars
  end
end
