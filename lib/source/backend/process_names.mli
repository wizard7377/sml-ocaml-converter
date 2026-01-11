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

class process_names :
  Common.options ref
  -> Context.t ref
  -> object
       method process_name : ?ctx:context -> name:string list -> Longident.t
     end
