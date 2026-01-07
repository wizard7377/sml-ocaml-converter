type context = .. 
type context += PatternHead 
type context += PatternTail  
type context += Value 
type context += Type 
type context += ModuleValue 
type context += ModuleType

type is_constructor = YesItIs of int | NoItsNot
let process_lowercase (s : string) : string = String.uncapitalize_ascii s ;;
let process_uppercase (s : string) : string = String.capitalize_ascii s ;;
let process_caps (s : string) : string = String.uppercase_ascii s ;;
class process_names (store : Context.t ref) = object (self)
  val store : Context.t ref = store
  val mutable open_paths = [[""]]
  val mutable current_path = [""]
  val mutable module_aliases : (string, string list) Hashtbl.t = Hashtbl.create 0 
  (* TODO Add this to the context instead *)
  method add_module_alias ~name ~path = 
    Hashtbl.add module_aliases name path
  method add_open ~path = 
    open_paths <- path :: open_paths
  method get_current_path () = 
    current_path
  method set_current_path ~path = 
    current_path <- path
  
  method private is_constructor_name ~(path: string list) ~(name: string) : bool =
    let infos = Context.find ~ctx:!store ~opened:open_paths ~root:name in
    match infos with
    | [] -> false
    | _ -> true
 

  method process_name ~(ctx : context) ~(path : string list) ~(name : string) = match ctx with   
    | PatternHead -> 
      process_uppercase name
    | PatternTail -> 
      if self#is_constructor_name ~path ~name then process_uppercase name else process_lowercase name
    | Value -> 
      if self#is_constructor_name ~path ~name then process_uppercase name else process_lowercase name
    | Type -> 
      process_lowercase name
    | ModuleValue -> 
      process_uppercase name
    | ModuleType -> 
      process_caps name
    | _ -> 
      name
    



end 
