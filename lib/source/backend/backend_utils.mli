(** Backend utilities for SML to OCaml conversion.

    This module consolidates small utility functions used throughout the
    backend:
    - Identifier conversion (idx_to_string, idx_to_name)
    - Type variable processing
    - Capitalization utilities
    - Constructor name transformation *)

(** {1 Identifier Utilities} *)

val idx_to_string : Ast.idx -> string
(** Extract a string from an idx value *)

val idx_to_name : Ast.idx -> string list
(** Convert an idx to a list of name parts for processing *)

(** {1 Type Variable Utilities} *)

val strip_type_var_prefix : string -> string
(** Strip the leading quote(s) from a type variable name. *)

val process_type_var_name : string -> Parsetree.core_type
(** Process a type variable name and return an OCaml type variable. *)

val process_type_params :
  Ast.idx Ast.node list ->
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
(** Convert a list of SML type variable nodes to OCaml type parameters. *)

(** {1 Capitalization Utilities} *)

val process_lowercase : string -> string
(** Convert string to lowercase (first character) *)

val process_uppercase : string -> string
(** Convert string to uppercase (first character) *)

val process_caps : string -> string
(** Convert string to all caps *)

type capital = Lowercase | Uppercase | Caps  (** Capitalization category *)

val get_capital : string -> capital
(** Determine the capitalization category of a string *)

val is_variable_identifier : string -> bool
(** Check if an identifier represents a variable (starts with lowercase or
    underscore) rather than a constructor (starts with uppercase) *)

val is_operator_name : string -> bool
(** Check if a string is an operator (non-alphanumeric identifier) *)

(** {1 Constructor Name Transformation} *)

val is_all_uppercase : string -> bool
(** Check if all characters in a string are uppercase *)

val transform_constructor : string -> string
(** Transform a constructor name to valid OCaml format. See implementation for
    detailed rules. *)

val transform_to_lowercase : string -> string
(** Transform a variable name to lowercase:
    - SOME -> some_
    - Foo -> foo_
    - bar -> bar (no change) *)

(** {1 Constant Processing} *)

val process_con : Ast.constant Ast.node -> Parsetree.constant
(** Convert an SML constant to an OCaml constant. *)
