(** Utilities for handling capitalization in SML to OCaml conversion.

    SML and OCaml have different conventions for capitalization:
    - SML constructors can be any case (SOME, Some, some)
    - OCaml constructors must start with uppercase
    - SML type names can be any case
    - OCaml type names must start with lowercase *)

(** Convert string to lowercase (first character) *)
let process_lowercase (s : string) : string = String.uncapitalize_ascii s

(** Convert string to uppercase (first character) *)
let process_uppercase (s : string) : string = String.capitalize_ascii s

(** Convert string to all caps *)
let process_caps (s : string) : string = String.uppercase_ascii s

(** Capitalization category *)
type capital = Lowercase | Uppercase | Caps

(** Determine the capitalization category of a string *)
let get_capital (s : string) : capital =
  if s = process_caps s then Caps
  else if s = process_uppercase s then Uppercase
  else Lowercase

(** Check if an identifier represents a variable (starts with lowercase or
    underscore) rather than a constructor (starts with uppercase) *)
let is_variable_identifier (s : string) : bool =
  if String.length s = 0 then false
  else
    let first_char = String.get s 0 in
    (first_char >= 'a' && first_char <= 'z') || first_char = '_'

(** Check if a string is an operator (non-alphanumeric identifier). Returns true
    if the first character is not a letter or underscore. *)
let is_operator_name (s : string) : bool =
  if String.length s = 0 then false
  else
    let c = String.get s 0 in
    not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_')
