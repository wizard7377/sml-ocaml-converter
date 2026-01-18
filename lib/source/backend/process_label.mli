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

  (* NEW METHODS for comment hoisting *)
  method enter_accumulate_context : unit
  method exit_accumulate_context : unit
  method flush_pending : (string * int) list
  method mark_structure_boundary : int -> unit
  method emit_pending_as_structure_items : unit -> Parsetree.structure_item list
  method emit_pending_as_signature_items : unit -> Parsetree.signature_item list
  
  (* Methods for leading comments at declaration boundaries *)
  method leading_comments : Lexing.position option -> Parsetree.structure_item list
  method leading_signature_comments : Lexing.position option -> Parsetree.signature_item list
end
