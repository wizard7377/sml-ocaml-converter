open Ez_file
let get_file filename = FileString.read_file filename
let write_file filename content = () (* TODO *)

type ident = Name of string | Symbol of string 
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
let fst3 (x,_,_) = x
let snd3 (_,y,_) = y
let trd3 (_,_,z) = z