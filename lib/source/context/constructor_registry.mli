(** Constructor registry for tracking SML constructors across modules *)

type constructor_info = {
  name: string;         (** Original SML name *)
  path: string list;    (** Module path including constructor name *)
  ocaml_name: string;   (** Transformed OCaml name *)
}

type t
(** The constructor registry type *)

val create : unit -> t
(** Create a new empty registry *)

val add_constructor : t -> path:string list -> name:string -> ocaml_name:string -> unit
(** Add a constructor to the registry with its fully qualified path *)

val lookup : t -> path:string list option -> string -> constructor_info option
(** Look up a constructor by name, optionally with module path qualifier *)

val open_module : t -> module_path:string list -> unit
(** Bring all constructors from a module into unqualified scope *)
