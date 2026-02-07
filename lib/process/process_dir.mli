(** Directory-level processing (UNIMPLEMENTED).

    {b Warning:} This module is currently a stub. Directory processing is
    handled by {!Process_group} and the CLI layer instead.

    This module is kept for potential future expansion of directory-level batch
    processing features. *)

exception DirectoryExists
(** Raised when setup attempts to use an existing directory. *)

(** Directory processor class.

    {b Note:} Most methods are unimplemented and will raise [assert false]. Use
    {!Process_group} for actual multi-file processing. *)
class process_dir : string -> Common.t -> object
  method set_dir : string -> unit
  (** Change the working directory. *)

  method setup_dir : string -> unit
  (** Setup/move to a new directory.
      @raise DirectoryExists if target exists *)

  method copy_other_files : string -> unit
  (** Copy non-SML files (unimplemented).
      @raise Assert_failure *)

  method process_sml : string -> unit
  (** Process SML files in directory (unimplemented).
      @raise Assert_failure *)
end
