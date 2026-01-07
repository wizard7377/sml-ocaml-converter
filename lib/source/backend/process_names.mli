

type context = ..
type context += PatternHead
type context += PatternTail
type context += Value
type context += Type
type context += ModuleValue
type context += ModuleType

class process_names : Context.t ref -> object
  method add_module_alias : name:string -> path:string list -> unit
  method add_open : path:Context.path -> unit
  method get_current_path : unit -> string list
  method set_current_path : path:string list -> unit
  method process_name : ctx:context -> path:string list -> name:string -> string
end