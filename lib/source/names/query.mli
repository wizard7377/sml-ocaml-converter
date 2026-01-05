
type t 


val run_query : query:t -> entry:Store.entry -> bool
val select_query : query:t -> store:Store.t -> Store.t * Store.t
val query_and : t -> t -> t
val query_or : t -> t -> t
val query_not : t -> t
val query_xor : t -> t -> t

val name_query : (Name.name -> bool) -> t
val context_query : (Store.name_context -> bool) -> t
val path_query : (Name.path -> bool) -> t
val package_query : (Name.package -> bool) -> t
val action_query : (Store.action -> bool) -> t

val yes_query : t
val no_query : t

