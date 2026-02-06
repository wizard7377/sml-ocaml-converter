(** Name processing for SML to OCaml conversion.

    This module provides contextual name transformation, handling:
    - Keyword escaping (SML identifiers that are OCaml keywords)
    - Capitalization adjustments (constructors, types, values)
    - Scoped name tracking for let bindings and modules
    - SML basis library constructor mapping (SOME -> Some, etc.)

    Uses {!Capital_utils} for low-level capitalization functions. *)

type context = ..
type context += PatternHead
type context += PatternTail

(** Value DEFINITIONS **)
type context += Value

type context += Type
type context += ModuleValue
type context += ModuleType
type context += Label

(** Constructor DEFINITIONS *)
type context += Constructor

type context += Operator
type context += Empty
type context += Functor
type note = int

val process_lowercase : string -> string
(** Re-exported capitalization utilities from {!Capital_utils} *)

val process_uppercase : string -> string
val process_caps : string -> string
val is_lowercase : string -> bool

class process_names : Common.t ref -> Context.t ref -> object
  method is_good : ?ctx:context -> name:string list -> bool
  method process_name : ?ctx:context -> name:string list -> Longident.t * bool
  method push_context : unit -> note
  method pop_context : note -> unit
  method add_name : ?global:bool -> from:string -> res:string -> unit -> unit
  method get_name : string -> string
end
