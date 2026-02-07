type atag = string
type attr = Parsetree.attribute
type cite = Parsetree.payload
type 'a citer = 'a -> attr -> 'a

val change_name : string -> string -> attr
val fixed_name : string -> string -> attr

class process_label : Common.t -> string -> object
  method cite :
    'a. 'a citer -> string list -> 'a -> 'a

  method cite_exact : 'a. 'a citer -> string -> string list -> 'a -> 'a
  method until : Lexing.position -> attr list
  method destruct : unit -> bool

  (** Convert node comments to standalone structure items, tracking them for error detection *)
  method comments_to_structure_items : string list -> Parsetree.structure_item list

  (** Check that all comments were emitted. Raises [Failure] if any were seen but not emitted. *)
  method check_all_comments_emitted : unit

  (* NEW METHODS for comment hoisting *)
  method enter_accumulate_context : unit
  method exit_accumulate_context : unit
  method flush_pending : (string * int) list
  method mark_structure_boundary : int -> unit
  method push_structure_boundary : int -> int
  method restore_structure_boundary : int -> unit
  method emit_pending_as_structure_items : unit -> Parsetree.structure_item list
  method emit_pending_as_signature_items : unit -> Parsetree.signature_item list

  (** Extract comments from the regex pool within an AST node's byte range
      and attach them as attributes. This is how expression/pattern-level comments
      get placed close to their original SML position. *)
  method cite_for_node :
    'a. 'a citer ->
    (Lexing.position * Lexing.position) option ->
    'a -> 'a

  (** Flush all remaining comments (pending + regex pool) as structure items *)
  method flush_all_remaining_as_structure_items : Parsetree.structure_item list

  (* Methods for leading comments at declaration boundaries *)
  method leading_comments :
    Lexing.position option -> Parsetree.structure_item list

  method leading_signature_comments :
    Lexing.position option -> Parsetree.signature_item list
end
