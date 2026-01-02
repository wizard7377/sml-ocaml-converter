val get_file : string -> string
val write_file : string -> string -> unit
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