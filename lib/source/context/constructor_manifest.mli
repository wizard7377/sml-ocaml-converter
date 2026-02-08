(** Manifest file I/O for constructor information.

    Manifest files (with .shibboleth-constructors extension) store constructor
    transformation mappings in S-expression format. These files enable
    cross-module constructor resolution by recording all constructors defined in
    a module.

    Manifest file format (S-expression):
    {[
      ((name ok) (path (Result ok)) (ocaml_name Ok_))
      ((name error) (path (Result error)) (ocaml_name Error_))
      ...
    ]}

    Usage pattern:
    {[
      (* Writing manifest after compilation *)
      let constructors = Backend.get_all_constructors () in
      write_file "output.ml.shibboleth-constructors" constructors;

      (* Reading manifest to resolve imported constructors *)
      let imported = read_file "imported.ml.shibboleth-constructors" in
      List.iter
        (fun info ->
          Constructor_registry.add_constructor registry ~path:info.path
            ~name:info.name ~ocaml_name:info.ocaml_name)
        imported
    ]} *)

val to_sexp : Constructor_registry.constructor_info list -> Sexplib0.Sexp.t
(** Convert constructor list to S-expression representation. *)

val from_sexp : Sexplib0.Sexp.t -> Constructor_registry.constructor_info list
(** Parse constructor list from S-expression *)

val write_file : string -> Constructor_registry.constructor_info list -> unit
(** Write constructors to a manifest file *)

val read_file : string -> Constructor_registry.constructor_info list
(** Read constructors from a manifest file *)

val find_manifest :
  search_paths:string list -> module_name:string -> string option
(** Find a manifest file for a module in the search paths *)

(** {1 Combined context format}

    Bundles constructor info from multiple modules into a single .sctx file. *)

type module_context = {
  module_path : string;
  constructors : Constructor_registry.constructor_info list;
}
[@@deriving sexp]

val combined_to_sexp : module_context list -> Sexplib0.Sexp.t
(** Convert a list of module contexts to S-expression. *)

val combined_from_sexp : Sexplib0.Sexp.t -> module_context list
(** Parse a list of module contexts from S-expression. *)

val write_combined_file : string -> module_context list -> unit
(** Write combined module contexts to a file. *)

val read_combined_file : string -> module_context list
(** Read combined module contexts from a file. *)
