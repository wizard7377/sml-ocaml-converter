structure Server =
struct

  exception Error of string
  fun error (msg) = raise Error(msg)

  fun quote (string) = "`" ^ string ^ "'"

  fun checkEmpty ("") = ()
    | checkEmpty (args) = error "Extraneous arguments"

  fun getFile ("", default) = default
    | getFile (fileName, default) = fileName

  fun getFile' ("") = error "Missing filename"
    | getFile' (fileName) = fileName

  fun getId (id::nil) = id
    | getId (nil) = error "Missing identifier"
    | getId (ts) = error "Extraneous arguments"

  fun getIds (ids) = ids

  fun getBool ("true"::nil) = true
    | getBool ("false"::nil) = false
    | getBool (nil) = error "Missing boolean value"
    | getBool (t::nil) = error (quote t ^ " is not a boolean")
    | getBool (ts) = error "Extraneous arguments"

end;
