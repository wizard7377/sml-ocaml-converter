type context = ..
type context += PatternHead
type context += PatternTail
type context += Value
type context += Type
type context += ModuleValue
type context += ModuleType
type context += Label
type context += Constructor
type context += Operator
type context += Empty
type context += Functor
type note = int
class process_names :
  Common.options ref
  -> Context.t ref
  -> object
        method is_good : ?ctx:context -> name:string list -> bool
        method process_name : ?ctx:context -> name:string list -> Longident.t * bool
        method push_context : unit -> note 
        method pop_context : note -> unit 
        method add_name : from:string -> res:string -> unit 
        method get_name : string -> string 
     end
