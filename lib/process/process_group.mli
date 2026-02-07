(** Multi-file group processing for SML module systems.

    This module handles conversion of SML modules split across multiple files:
    - [.sig] files (module signatures/interfaces)
    - [.fun] files (functor implementations)
    - [.sml] files (structure implementations/exports)

    These files are processed together and concatenated into a single OCaml
    output. *)

open Common

type sml_code
(** Internal representation of SML code. *)

type ocaml_code
(** Internal representation of OCaml code. *)

type t = {
  interface : string option;  (** Path to .sig file, if present *)
  implementation : string option;  (** Path to .fun file, if present *)
  exports : string option;  (** Path to .sml file, if present *)
}
(** Group member files record. *)

(** Group processor for related SML files.

    Processes a set of related files (.sig/.fun/.sml) that together define a
    single SML module. All files are converted and concatenated in the correct
    order: signature → functor → structure. *)
class process_group : string -> Common.t -> object
  method get_store : unit -> Context.t
  (** Get the accumulated name resolution context. *)

  method set_store : Context.t -> unit
  (** Set the name resolution context (for pre-populating or reusing). *)

  method get_config : unit -> Common.t
  (** Get current conversion configuration. *)

  method set_config : Common.t -> unit
  (** Update conversion configuration. *)

  method find_members : string -> t
  (** [find_members base_path] discovers related files.

      Given a base path (e.g., ["mymodule"]), searches for:
      - [mymodule.sig] (interface)
      - [mymodule.fun] (functor implementation)
      - [mymodule.sml] (structure implementation)

      @param base_path Path without extension
      @return Record of found file paths (None for missing files) *)

  method set_members : t -> unit
  (** Manually set the group members to process. *)

  method parse_members : t -> string
  (** [parse_members members] converts all member files to OCaml.

      Processes each file through the full pipeline and concatenates results in
      the standard order: interface, implementation, exports.

      @param members Record of file paths to process
      @return Concatenated OCaml output from all files
      @raise Failure if any file fails to convert *)
end
