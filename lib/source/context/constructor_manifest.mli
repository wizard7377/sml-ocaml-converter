(** Manifest file I/O for constructor information *)

val to_json : Constructor_registry.constructor_info list -> Yojson.Safe.t
(** Convert constructor list to JSON *)

val from_json : Yojson.Safe.t -> Constructor_registry.constructor_info list
(** Parse constructor list from JSON *)

val write_file : string -> Constructor_registry.constructor_info list -> unit
(** Write constructors to a manifest file *)

val read_file : string -> Constructor_registry.constructor_info list
(** Read constructors from a manifest file *)

val find_manifest : search_paths:string list -> module_name:string -> string option
(** Find a manifest file for a module in the search paths *)
