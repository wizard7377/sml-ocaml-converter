module Info = Info
module Basis = Basis
module Get_context = Get_context

type t = {
  info : Info.t;
  constructor_registry : Constructor_registry.t;
}

val create : Info.t -> t
(** Create a context with the given info and an initialized constructor registry *)

val basis_context : t
(** Basis context pre-populated with standard constructors *)
