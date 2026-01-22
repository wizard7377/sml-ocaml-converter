type source = File of string list | StdIn
type target = FileOut of string | StdOut | Silent
type conversions
type convert_flag = Enable | Warn | Note | Disable

val is_flag_enabled : convert_flag -> bool
(** Check if a conversion flag is active (not Disable). *)
val mkConversions :
  ?convert_names:convert_flag ->
  ?convert_comments:convert_flag ->
  ?add_line_numbers:convert_flag ->
  ?convert_keywords:convert_flag ->
  ?rename_types:convert_flag ->
  ?make_make_functor:convert_flag ->
  ?rename_constructors:convert_flag ->
  ?guess_pattern:convert_flag ->
  ?deref_pattern:convert_flag ->
  ?curry_expressions:convert_flag ->
  ?curry_types:convert_flag ->
  ?tuple_select:convert_flag ->
  ?toplevel_names:convert_flag ->
  unit ->
  conversions

type options

val mkOptions :
  ?input_file:source ->
  ?output_file:target ->
  ?verbosity:int option ->
  ?conversions:conversions ->
  ?concat_output:bool ->
  ?force:bool ->
  ?quiet:bool ->
  ?guess_var:string option ->
  ?debug:string list ->
  ?check_ocaml:bool ->
  ?variable_regex:string ->
  ?dash_to_underscore:bool ->
  unit ->
  options

val get_verbosity : options -> int option
val get_verbosity_default : options -> int -> int
val get_conversions : options -> conversions
val get_input_file : options -> source
val get_output_file : options -> target
val get_convert_names : options -> convert_flag
val get_convert_comments : options -> convert_flag
val get_concat_output : options -> bool
val get_force : options -> bool
val get_line_numbers : options -> convert_flag
val get_convert_keywords : options -> convert_flag
val get_rename_types : options -> convert_flag
val get_make_make_functor : options -> convert_flag
val get_guess_var : options -> string option
val get_quiet : options -> bool
val get_debug : options -> string list
val get_rename_constructors : options -> convert_flag
val get_guess_pattern : options -> convert_flag
val get_check_ocaml : options -> bool
val get_variable_regex : options -> string
val get_deref_pattern : options -> convert_flag
val get_curry_expressions : options -> convert_flag
val get_curry_types : options -> convert_flag
val get_tuple_select : options -> convert_flag
val get_toplevel_names : options -> convert_flag
val get_dash_to_underscore : options -> bool
val engaged : convert_flag -> bool
(** [engaged flag] returns true if the conversion flag is not Disable. *)

val noted : convert_flag -> bool
(** [noted flag] returns true if the conversion flag is Warn or Note. *)

module type CONFIG = sig
  val config : options
end
