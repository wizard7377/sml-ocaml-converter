type source = File of string list | StdIn
type target = FileOut of string | StdOut | Silent
type conversions

val mkConversions :
  ?convert_names:bool -> ?convert_comments:bool -> ?add_line_numbers:bool -> ?convert_keywords:bool -> ?rename_types:bool -> unit -> conversions

type options

val mkOptions :
  ?input_file:source ->
  ?output_file:target ->
  ?verbosity:int option ->
  ?conversions:conversions ->
  ?concat_output:bool ->
  ?force:bool ->
  ?quiet:bool ->
  unit ->
  options

val get_verbosity : options -> int option
val get_verbosity_default : options -> int -> int
val get_conversions : options -> conversions
val get_input_file : options -> source
val get_output_file : options -> target
val get_convert_names : options -> bool
val get_convert_comments : options -> bool
val get_concat_output : options -> bool
val get_force : options -> bool
val get_line_numbers : options -> bool
val get_convert_keywords : options -> bool
val get_rename_types : options -> bool
val get_quiet : options -> bool
module type CONFIG = sig
  val config : options
end
