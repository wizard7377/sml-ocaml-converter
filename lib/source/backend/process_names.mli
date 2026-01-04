type context =
  | TypeVar
  | TypeConst
  | Value
  | Structure
  | Signature 
  | PatternHead
  | PatternTail
  | Empty

module type PROCESS_NAMES = sig
  module Config : Common.CONFIG
  type t
  (** The type of collections of processed names.
      This should include both names that should and {e should not} be changed
          *) 

  val init : unit -> t

  val add_name: string -> string -> t -> t
  (** Add a name mapping to the collection *)

  val get_name : t -> string -> string option
  (** Get the name in context *)

  val process_name : ?ctx:context -> t -> string -> string * t
  (** Process a name according to the context and return the new name.
      If the name was changed, it is added to the collection.
      *)
end

module Make (Config : Common.CONFIG) : PROCESS_NAMES with module Config = Config
