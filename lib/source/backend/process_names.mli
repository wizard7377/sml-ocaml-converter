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
class process_names :
  Common.options ref
  -> Context.t ref
  -> object
        method is_good : ?ctx:context -> name:string list -> bool
       method process_name : ?ctx:context -> name:string list -> Longident.t * bool

     end
