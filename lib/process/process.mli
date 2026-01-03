open Common 
type sml_code 
type ocaml_code
class process_file : config -> object
    
    method set_config : config -> unit 
    method parse_sml : string -> sml_code
    method convert_to_ocaml : sml_code -> ocaml_code
    method print_ocaml : ocaml_code -> string

end

val run : process_file ref -> int 