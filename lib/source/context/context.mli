module Info = Info
module Basis = Basis
module Get_context = Get_context
module Constructor_registry = Constructor_registry
module Constructor_manifest = Constructor_manifest

type t = {
  info : Info.t;
  constructor_registry : Constructor_registry.t;
}

val create : Info.t -> t
(** Create a context with the given info and an initialized constructor registry *)

val merge : t -> t -> t
(** Merge two contexts, combining both info and constructor registries *)

val basis_context : t
(** Basis context pre-populated with standard constructors *)

val load_module_constructors : t -> module_name:string -> search_paths:string list -> bool
(** Load constructor information from a module's manifest file.
    Returns true if manifest was found and loaded, false otherwise. *)
