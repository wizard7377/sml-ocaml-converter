type path = string list
type name_info = .. 

(** 
    Additional information associated with a name.
    This currently includes constructor arity and module mapping.
    The reason for only these two has to do with the differences in SML and OCaml.
*)
type name_info += ConstructorInfo of {
    arity : int option ;
}
type name_info += ModuleInfo of {
    maps_to : path option 
} 

type name = {
    path : path ;
    root : string ;
}

(** {2 Name stores} *)

type t 
(** 
    The core type of name stores. 
    It maps certain kinds of names to their associated information.
*)

(** {3 Name store operations} *)     


val create : (name * name_info) list -> t
(** 
    Create a new name store from a list of (name, name_info) pairs.
*)

val merge : t -> t -> t
(** 
    Merge two name stores into one.
*)

val find : ctx:t -> opened:path list -> root:string -> name_info list 
(** 
    Find all possible names that could be associated with the given root name, with a path
    TODO Module renaming/aliasing not yet implemented.
    
    @param ctx The context to search in
    @param opened The list of opened paths to consider
    @param root The root name to look for
    @return A list of all matching name_info entries
*)