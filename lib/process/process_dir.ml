(** Directory-level processing (UNIMPLEMENTED).

    This module is a placeholder for future directory-level conversion functionality.
    Currently, directory processing is handled by the CLI layer using process_group.

    {b Note:} This module is not currently used and methods raise assertions.
    It's kept for potential future directory-level processing features. *)

open Common

(** Raised when attempting to set up a directory that already exists. *)
exception DirectoryExists

(** Directory processor class (currently unimplemented).

    This class was intended for batch directory operations but is superseded
    by the CLI's direct file discovery and group processing. *)
class process_dir (dir : string) (config : Common.t) =
  object
    val mutable dir : string = dir
    val mutable cfg : Common.t = config

    method set_dir (d : string) : unit = dir <- d
    (** Change the working directory path. *)

    method setup_dir (d : string) : unit =
      (** Move/rename the processing directory.
          @raise DirectoryExists if target directory already exists *)
      if Sys.file_exists d then raise DirectoryExists
      else Bos.OS.Path.move (Fpath.v dir) (Fpath.v d) |> ignore;
      dir <- d

    method copy_other_files (_dest : string) : unit = assert false
    (** Copy non-SML files to destination (unimplemented). *)

    method process_sml (_dest : string) : unit = assert false
    (** Process all SML files in directory (unimplemented). *)
  end
