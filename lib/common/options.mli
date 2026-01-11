type source = File of string list | StdIn
type target = FileOut of string | StdOut | Silent
type conversions

val mkConversions :
  ?convert_names:bool -> ?convert_comments:bool -> unit -> conversions

type options

val mkOptions :
  ?input_file:source ->
  ?output_file:target ->
  ?verbosity:int option ->
  ?conversions:conversions ->
  ?concat_output:bool ->
  unit ->
  options

val get_verbosity : options -> int option
val get_conversions : options -> conversions
val get_input_file : options -> source
val get_output_file : options -> target
val get_convert_names : conversions -> bool
val get_convert_comments : conversions -> bool
val get_concat_output : options -> bool
module type CONFIG = sig
  val config : options
end
