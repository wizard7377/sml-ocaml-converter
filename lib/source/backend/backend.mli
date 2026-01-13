
(** [Make (Store) (Config)] creates a backend transformation module.

    This functor instantiates the complete SML-to-OCaml transformation
    pipeline with the specified name resolution store and configuration.

    @param Context Implementation of the {!Backend_sig.CONTEXT} signature,
                 providing name resolution and identifier transformation
                 capabilities.
    @param Config Implementation of the {!Common.CONFIG} signature,
                  providing conversion configuration including input/output
                  paths, verbosity settings, and conversion flags.

    @return A module implementing the {!Backend_sig.BACKEND} signature,
            which provides functions for transforming each SML AST node
            type to its OCaml equivalent. *)
module Make (Context : Backend_sig.CONTEXT) (Config : Common.CONFIG) :
  Backend_sig.BACKEND
