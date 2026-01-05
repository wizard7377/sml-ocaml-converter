
exception Unbound_name of Names.Name.t
exception Ambiguous_name of Names.Name.t * Names.Name.t list
class process_names : Names.Name.package -> Names.Store.t -> object
  (** {3 Methods for processing names } 
    This is the highest level processor, it binds names in contexts
  *)


  method open_module : Names.Name.t -> Names.Name.t
  method include_module : Names.Name.t -> Names.Name.t
  method local_bind : 'a . ?ctx:Names.Store.name_context list -> Names.Name.t -> Names.Store.action -> (Names.Name.t -> 'a) -> 'a
  method process_name : ?ctx:Names.Store.name_context list -> Names.Name.path * Names.Name.name -> Names.Name.t
  method name_to_idx : Names.Name.t -> string list 
end




