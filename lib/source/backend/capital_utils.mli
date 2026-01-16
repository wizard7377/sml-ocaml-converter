(** Utilities for handling capitalization in SML to OCaml conversion. *)

(** Convert string to lowercase (first character) *)
val process_lowercase : string -> string

(** Convert string to uppercase (first character) *)
val process_uppercase : string -> string

(** Convert string to all caps *)
val process_caps : string -> string

(** Capitalization category *)
type capital = Lowercase | Uppercase | Caps

(** Determine the capitalization category of a string *)
val get_capital : string -> capital

(** Check if an identifier represents a variable (starts with lowercase or
    underscore) rather than a constructor (starts with uppercase) *)
val is_variable_identifier : string -> bool

(** Check if a string is an operator (non-alphanumeric identifier) *)
val is_operator_name : string -> bool
