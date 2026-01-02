open Ez_file
let get_file filename = FileString.read_file filename
let write_file filename content = () (* TODO *)

type ident = Name of string | Symbol of string 
type config = {
    input_file : string ;
    output_file : string option ;
    verbosity : int option ;
    guess_names : bool ;
    curry_functions : bool ;
    no_comments : bool ;
}

module type CONFIG = sig 
    val config : config 
end