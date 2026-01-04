open Common 
type sml_code 
type ocaml_code

class process_file : ?store:Names.Store.t -> config -> object
    
    method set_config : config -> unit 
    method get_config : unit -> config
    method get_store : unit -> Names.Store.t 
    method set_store : Names.Store.t -> unit
    method parse_sml : string -> sml_code
    method convert_to_ocaml : sml_code -> ocaml_code
    method print_ocaml : ocaml_code -> string

end




