(** Utilities for processing SML type variables. *)

(** Strip the leading quote(s) from a type variable name. *)
val strip_type_var_prefix : string -> string

(** Process a type variable name and return an OCaml type variable. *)
val process_type_var_name : string -> Parsetree.core_type

(** Convert a list of SML type variable nodes to OCaml type parameters. *)
val process_type_params : Ast.idx Ast.node list -> 
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
