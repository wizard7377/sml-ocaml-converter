(** Constructor registry for tracking SML constructors across modules.

    This module maintains a registry of all constructors encountered during
    SML-to-OCaml compilation, enabling correct name resolution even when SML
    uses lowercase constructor names (which are illegal in OCaml).

    The registry supports:
    - Qualified lookups: M.ok looks up constructor "ok" in module M
    - Unqualified lookups: ok looks for constructors named "ok" in scope
    - Open module handling: bringing module constructors into scope

    Example usage:
    {[
      let registry = create () in
      add_constructor registry ~path:["Result"; "ok"] ~name:"ok" ~ocaml_name:"Ok_";
      match lookup registry ~path:None "ok" with
      | Some info -> (* Found: info.ocaml_name = "Ok_" *)
      | None -> (* Not found *)
    ]} *)

type constructor_info = {
  name : string;  (** Original SML name (e.g., "ok") *)
  path : string list;
      (** Module path including constructor name (e.g., ["Result"; "ok"]) *)
  ocaml_name : string;  (** Transformed OCaml name (e.g., "Ok_") *)
}
[@@deriving sexp]

type t
(** The constructor registry type *)

val create : unit -> t
(** Create a new empty registry *)

val add_constructor :
  t -> path:string list -> name:string -> ocaml_name:string -> unit
(** Add a constructor to the registry with its fully qualified path *)

val lookup : t -> path:string list option -> string -> constructor_info option
(** Look up a constructor by name, optionally with module path qualifier *)

val open_module : t -> module_path:string list -> unit
(** Bring all constructors from a module into unqualified scope *)

val add_module_alias : t -> alias:string list -> target:string list -> unit
(** Register a module alias. After
    [add_module_alias t ~alias:["I"] ~target:["M"]], constructors in M become
    accessible via I. For example, M.Root becomes accessible as I.Root. *)

val get_all_constructors : t -> constructor_info list
(** Get all constructors from the registry *)
