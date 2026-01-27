(** Centralized name processing for SML to OCaml conversion. *)

module type CONFIG = sig
  val config : Common.options
  val context : Context.t
end

(** Functor to create a name processor with specific configuration *)
module Make (Config : CONFIG) : sig
  val to_longident :
    ctx:Process_names.context -> string list -> Ppxlib.Longident.t
  (** Convert name parts to a Longident.t with context-aware processing *)

  val to_string : ctx:Process_names.context -> string list -> string
  (** Convert name parts to a string (last component only) *)

  val idx_to_longident :
    ctx:Process_names.context -> Ast.idx -> Ppxlib.Longident.t
  (** Convert an AST idx to a Longident.t with context-aware processing *)

  val idx_to_string : ctx:Process_names.context -> Ast.idx -> string
  (** Convert an AST idx to a string (last component only) *)

  val process_with_op : ctx:Process_names.context -> Ast.with_op -> string
  (** Process SML with_op wrapper (handles op prefix for operators) *)

  val is_valid : ctx:Process_names.context -> string list -> bool
  (** Check if a name is valid for the given context *)

  val process_name :
    ctx:Process_names.context -> string list -> Ppxlib.Longident.t * bool
  (** Process a name and return both the result and whether it was changed *)

  val push_context : unit -> Process_names.note
  (** Push a new naming context scope *)

  val pop_context : Process_names.note -> unit
  (** Pop a naming context scope *)

  val add_name : ?global:bool -> from:string -> res:string -> unit -> unit
  (** Add a name mapping in the current scope *)

  val get_name : string -> string
  (** Get the processed name for a given input *)

  val matches_pattern : string -> bool
end

type context = Process_names.context
(** Re-exported context type *)

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
