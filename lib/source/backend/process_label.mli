type atag = string
type attr = Parsetree.attribute
type cite = Parsetree.payload
type 'a citer = 'a -> attr -> 'a

val change_name : string -> string -> attr
val fixed_name : string -> string -> attr

class process_label : Common.options -> string -> object
  method cite :
    'a. 'a citer -> (Lexing.position * Lexing.position) option -> 'a -> 'a

  method cite_exact : 'a. 'a citer -> string -> string list -> 'a -> 'a
  method until : Lexing.position -> attr list
  method destruct : unit -> bool
end
