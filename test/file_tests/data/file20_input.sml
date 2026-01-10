structure Names =
struct
  datatype Qid = Qid of string list * string

  fun qidToString (Qid (ids, name)) =
        List.foldr (fn (id, s) => id ^ "." ^ s) name ids

  fun validateQualName nil = NONE
    | validateQualName (l as id::ids) =
        if List.exists (fn s => s = "") l
          then NONE
        else SOME (Qid (rev ids, id))

  fun stringToQid name =
        validateQualName (rev (String.fields (fn c => c = #".") name))

  fun unqualified (Qid (nil, id)) = SOME id
    | unqualified _ = NONE
end
