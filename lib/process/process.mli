open Common 
class process_file : config -> object
    method set_config : config -> unit 
    method run : unit -> int
end