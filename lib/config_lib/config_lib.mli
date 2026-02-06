include module type of Config_lib_
type t 

type arg 
type 'a flag = 'a Config_lib_.flag 

val get : 'a flag -> t -> 'a
val set : 'a flag -> 'a -> arg 
val create : arg list -> t 
val make :
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
  ?verbosity:int ->
  ?concat_output:bool ->
  ?force:bool ->
  ?quiet:bool ->
  ?guess_var:string option ->
  ?debug:string list ->
  ?check_ocaml:bool ->
  ?variable_regex:string ->
  ?dash_to_underscore:bool ->
  ?basis_shim:string list ->
  ?input_file:source ->
  ?output_file:target ->
  unit -> t

module type CONFIG = sig
  val config : t
end