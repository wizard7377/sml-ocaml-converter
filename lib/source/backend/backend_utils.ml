(** Backend utilities for SML to OCaml conversion.

    This module consolidates small utility functions used throughout the
    backend:
    - Identifier conversion (idx_to_string, idx_to_name)
    - Type variable processing
    - Capitalization utilities
    - Constructor name transformation *)

include Helpers

(** {1 Identifier Utilities}

    Functions for working with SML identifiers (Ast.idx type). *)

(** Extract a string from an idx value *)
let rec idx_to_string (idx : Ast.idx) : string =
  match idx with
  | Ast.IdxIdx s -> s.value
  | Ast.IdxVar s -> s.value
  | Ast.IdxLab s -> s.value
  | Ast.IdxNum s -> s.value
  | Ast.IdxLong parts ->
      String.concat "."
        (List.map (fun (p : Ast.idx Ast.node) -> idx_to_string p.value) parts)

(** Convert an idx to a list of name parts for processing *)
let rec idx_to_name (idx : Ast.idx) : string list =
  match idx with
  | Ast.IdxIdx s -> [ s.value ]
  | Ast.IdxVar s -> [ s.value ]
  | Ast.IdxLab s -> [ s.value ]
  | Ast.IdxNum s -> [ s.value ]
  | Ast.IdxLong parts ->
      List.flatten
        (List.map (fun (p : Ast.idx Ast.node) -> idx_to_name p.value) parts)

(** {1 Type Variable Utilities}

    Functions for processing SML type variables ('a, ''a). *)

(** Strip the leading quote(s) from a type variable name. Examples: "'a" -> "a",
    "''a" -> "a", "a" -> "a" *)
let strip_type_var_prefix (s : string) : string =
  if String.starts_with ~prefix:"''" s then String.sub s 2 (String.length s - 2)
  else if String.starts_with ~prefix:"'" s then
    String.sub s 1 (String.length s - 1)
  else s

(** Process a type variable name and return an OCaml type variable. *)
let process_type_var_name (s : string) : Parsetree.core_type =
  let stripped = strip_type_var_prefix s in
  Builder.ptyp_var stripped

(** Convert a list of SML type variable nodes to OCaml type parameters. *)
let process_type_params (tvars : Ast.idx Ast.node list) :
    (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list =
  List.map
    (fun (tv : Ast.idx Ast.node) ->
      let tv_str = idx_to_string tv.value in
      let var_name = strip_type_var_prefix tv_str in
      (Builder.ptyp_var var_name, (Asttypes.NoVariance, Asttypes.NoInjectivity)))
    tvars

(** {1 Capitalization Utilities}

    Functions for handling capitalization in SML to OCaml conversion. *)

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

(** Check if a string is an operator (non-alphanumeric identifier). *)
let is_operator_name (s : string) : bool =
  if String.length s = 0 then false
  else
    let c = String.get s 0 in
    not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_')

(** {1 Constructor Name Transformation}

    Functions for transforming SML constructor names to valid OCaml names.

    SML allows constructor names to start with lowercase letters, while OCaml
    requires them to start with uppercase. *)

let is_all_uppercase s = String.for_all (fun c -> Char.uppercase_ascii c = c) s

(** Transform a constructor name to valid OCaml format.

    Transformation rules:
    - Lowercase constructors: append "_" and capitalize (ok -> Ok_, some ->
      Some_)
    - Already uppercase: preserve as-is (Foo -> Foo, Bar -> Bar)
    - All caps: convert to proper case (SOME -> Some, NONE -> None)
    - Trailing underscore: add extra underscore to avoid collision (B_ -> B__)
*)
let transform_constructor name =
  if String.length name = 0 then name
  else if String.ends_with ~suffix:"_" name then
    let base = String.sub name 0 (String.length name - 1) in
    if Ppxlib.Keyword.is_keyword base then
      String.capitalize_ascii base ^ "_"
    else
      name ^ "_"
  else if Char.uppercase_ascii name.[0] = name.[0] then
    if is_all_uppercase name && String.length name > 1 then
      String.capitalize_ascii (String.lowercase_ascii name)
    else name
  else String.capitalize_ascii name ^ "_"

(** Transform a variable name to lowercase:
    - SOME -> some_
    - Foo -> foo_
    - bar -> bar (no change) *)
let transform_to_lowercase name =
  if String.length name = 0 then name
  else if Char.lowercase_ascii name.[0] = name.[0] then name
  else if is_all_uppercase name then String.lowercase_ascii name ^ "_"
  else String.uncapitalize_ascii name ^ "_"

(** {1 Constant Processing}

    Convert SML constants to OCaml constants. *)

(** Convert an SML constant to an OCaml constant.

    Handles:
    - Integer constants (decimal and hexadecimal)
    - Word constants (unsigned integers, SML-specific)
    - Floating-point constants
    - Character constants ([#"a"] → ['a'])
    - String constants *)
let process_con (constant : Ast.constant Ast.node) : Parsetree.constant =
  match constant.value with
  | Ast.ConInt i ->
      let i' = String.map (function '~' -> '-' | c -> c) i.value in
      Pconst_integer (i', None)
  | Ast.ConWord w ->
      let w' =
        if String.starts_with ~prefix:"0wx" w.value then
          "0x" ^ String.sub w.value 3 (String.length w.value - 3)
        else if String.starts_with ~prefix:"0w" w.value then
          String.sub w.value 2 (String.length w.value - 2)
        else w.value
      in
      Pconst_integer (w', None)
  | Ast.ConFloat r ->
      let r' = String.map (function '~' -> '-' | c -> c) r.value in
      Pconst_float (r', None)
  | Ast.ConChar c -> Pconst_char (String.get c.value 0)
  | Ast.ConString s -> Pconst_string (s.value, Location.none, None)
