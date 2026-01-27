(** Utilities for handling capitalization in SML to OCaml conversion. *)

val process_lowercase : string -> string
(** Convert string to lowercase (first character) *)

val process_uppercase : string -> string
(** Convert string to uppercase (first character) *)

val process_caps : string -> string
(** Convert string to all caps *)

(** Capitalization category *)
type capital = Lowercase | Uppercase | Caps

val get_capital : string -> capital
(** Determine the capitalization category of a string *)

val is_variable_identifier : string -> bool
(** Check if an identifier represents a variable (starts with lowercase or
    underscore) rather than a constructor (starts with uppercase) *)

val is_operator_name : string -> bool
(** Check if a string is an operator (non-alphanumeric identifier) *)
