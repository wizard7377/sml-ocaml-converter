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

include module type of Options
