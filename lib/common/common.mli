val get_file : string -> string
val write_file : string -> string -> unit
type convert_flag = 
  | Dont_convert
  | Do_convert 
  | Debug_convert

type do_convert = {
    pattern_names : convert_flag ;
    constructor_names_values : convert_flag ;
    function_names : convert_flag ;
    uncurry_types : convert_flag ;
    uncurry_values : convert_flag ;
}
type config = {
    input_file : string ;
    output_file : string option ;
    verbosity : int option ;
    conversions : do_convert ;
}



module type CONFIG = sig 
    val config : config 
end