(** Name processing for SML to OCaml conversion.

    This module provides contextual name transformation, handling:
    - Keyword escaping (SML identifiers that are OCaml keywords)
    - Capitalization adjustments (constructors, types, values)
    - Scoped name tracking for let bindings and modules
    - SML basis library constructor mapping (SOME -> Some, etc.)

    Uses {!Backend_utils} for low-level capitalization functions. *)

include module type of Ast

type context = ..
type context += PatternHead
type context += PatternTail
type context += Value
type context += Type
type context += ModuleValue
type context += ModuleType
type context += Label
type context += Constructor
type context += Operator
type context += Empty
type context += Functor
type note = int

val process_lowercase : string -> string
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

(** {1 Name Processor Functor} *)

module type CONFIG = sig
  val config : Common.t
  val context : Context.t
end

module Make (Config : CONFIG) : sig
  val process_name : ctx:context -> string list -> Ppxlib.Longident.t * bool
  val to_string : ctx:context -> string list -> string
  val to_longident : ctx:context -> string list -> Ppxlib.Longident.t
  val idx_to_longident : ctx:context -> idx -> Ppxlib.Longident.t
  val idx_to_string : ctx:context -> idx -> string
  val process_with_op : ctx:context -> with_op -> string
  val is_valid : ctx:context -> string list -> bool
  val push_context : unit -> note
  val pop_context : note -> unit
  val add_name : ?global:bool -> from:string -> res:string -> unit -> unit
  val get_name : string -> string
  val matches_pattern : string -> bool
end

val context_value : context
val context_type : context
val context_constructor : context
val context_operator : context
val context_label : context
val context_module_value : context
val context_module_type : context
val context_functor : context
val context_pattern_head : context
val context_pattern_tail : context
val context_empty : context
