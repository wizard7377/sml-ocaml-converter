(** Utilities for working with SML identifiers (idx type). *)

(** Extract a string from an idx value *)
val idx_to_string : Ast.idx -> string

(** Convert an idx to a list of name parts for processing *)
val idx_to_name : Ast.idx -> string list
