(** Common types and utilities for the SML-to-OCaml converter.

    {1 Synopsis}

    This module provides core types, configuration structures, and utility
    functions used throughout the Shibboleth converter. It defines the
    configuration interface and common file I/O operations.

    {1 Overview}

    The module is organized into:
    - File I/O utilities for reading and writing source files
    - Conversion flag types for controlling transformation behavior
    - Configuration types for pipeline settings
    - Utility functions for tuple manipulation

    {1 File I/O} *)

val get_file : string -> string

(** [get_file path] reads the entire contents of a file.

    @param path Path to the file to read
    @return String containing the complete file contents
    @raise Sys_error if the file cannot be read *)

val write_file : string -> string -> unit

(** [write_file path contents] writes a string to a file.

    Creates the file if it doesn't exist, or overwrites it if it does.

    @param path Path to the output file
    @param contents String to write to the file
    @raise Sys_error if the file cannot be written *)

(** {1 Tuple Utilities}

    Helper functions for accessing triple components. *)

val fst3 : 'a * 'b * 'c -> 'a

(** [fst3 (a, b, c)] extracts the first component of a triple.

    Example: [fst3 (1, 2, 3) = 1]

    @param triple A 3-tuple
    @return The first element *)

val snd3 : 'a * 'b * 'c -> 'b

(** [snd3 (a, b, c)] extracts the second component of a triple.

    Example: [snd3 (1, 2, 3) = 2]

    @param triple A 3-tuple
    @return The second element *)

val trd3 : 'a * 'b * 'c -> 'c

(** [trd3 (a, b, c)] extracts the third component of a triple.

    Example: [trd3 (1, 2, 3) = 3]

    @param triple A 3-tuple
    @return The third element *)

val last : 'a list -> 'a

(** [last lst] returns the last element of a list.

    @param lst The input list
    @return The last element of the list
    @raise Failure if the list is empty *)

val map_last : ('a -> 'a) -> 'a list -> 'a list

val convert_path_dashes_to_underscores : Fpath.t -> Fpath.t
(** [convert_path_dashes_to_underscores path] converts dashes to underscores in
    each path component (directory names and filenames) without affecting path
    separators.

    Example: [convert_path_dashes_to_underscores (Fpath.v "foo-bar/baz-qux.ml")]
    returns a path equivalent to ["foo_bar/baz_qux.ml"]

    @param path The input path
    @return A new path with dashes replaced by underscores in each component *)

include module type of Config_lib

(** Logger types *)
type level = Logger.level = High | Medium | Low | Debug

type kind = Logger.kind = Positive | Negative | Neutral | Warning

module type LOG = sig 
    val log :
    ?subgroup:string -> ?level:level -> ?kind:kind -> msg:string -> unit -> unit

  val log_with :
    cfg:t ->
    ?subgroup:string ->
    ?level:level ->
    ?kind:kind ->
    msg:string ->
    unit ->
    unit
  val log_fmt :
    ?subgroup:string ->
    ?level:level ->
    ?kind:kind ->
    msg:(unit Fmt.t) ->
    unit ->
    unit
    end

module type S = sig
  val config : t
  val group : string
end

module Make (C : S) : LOG

(** {1 Pretty Printing}

    OCaml Parsetree pretty-printing using Fmt-style interfaces. *)

module Format = Format
