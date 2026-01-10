structure ArraySlice :> ARRAY_SLICE =
struct
  type 'a slice = 'a Array.array * int * int option
  fun slice s = s
  val appi = Array.appi
end;
