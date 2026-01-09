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

(** [get_file path] reads the entire contents of a file.

    @param path Path to the file to read
    @return String containing the complete file contents
    @raise Sys_error if the file cannot be read *)
val get_file : string -> string

(** [write_file path contents] writes a string to a file.

    Creates the file if it doesn't exist, or overwrites it if it does.

    @param path Path to the output file
    @param contents String to write to the file
    @raise Sys_error if the file cannot be written *)
val write_file : string -> string -> unit

(** {1 Conversion Configuration}

    These types control which transformations are applied during conversion.
    Each transformation can be disabled, enabled, or run in debug mode. *)

(** Conversion flag controlling whether a specific transformation is applied.

    - [Dont_convert]: Skip this transformation entirely
    - [Do_convert]: Apply the transformation normally
    - [Debug_convert]: Apply the transformation with verbose debug output *)
type convert_flag =
  | Dont_convert
      (** Disable this specific transformation. The original SML construct
          will be translated literally without semantic adaptation. *)
  | Do_convert
      (** Enable this transformation. Apply the standard SML-to-OCaml
          semantic conversion for this construct. *)
  | Debug_convert
      (** Enable with debugging. Apply the transformation while emitting
          detailed trace information about the conversion process. *)

(** Conversion policy configuration.

    This record specifies which semantic transformations should be applied
    during conversion. Each field controls a different aspect of the
    transformation:

    {2 Name Transformations}
    - {!pattern_names}: Transform pattern variable names (e.g., snake_case)
    - {!constructor_names_values}: Transform constructor names (e.g., SOME → Some)
    - {!function_names}: Transform function names (e.g., camelCase adjustments)

    {2 Type Transformations}
    - {!uncurry_types}: Convert curried function types to tupled types where
      appropriate (e.g., [int -> int -> int] to [int * int -> int])
    - {!uncurry_values}: Convert curried function applications to tupled
      applications to match uncurried types

    {3 Example Configuration}

    Standard conversion with all transformations enabled:
    {[
      {
        pattern_names = Do_convert;
        constructor_names_values = Do_convert;
        function_names = Do_convert;
        uncurry_types = Dont_convert;  (* Preserve currying *)
        uncurry_values = Dont_convert;
      }
    ]}

    Debug mode for name transformations:
    {[
      {
        pattern_names = Debug_convert;
        constructor_names_values = Debug_convert;
        function_names = Debug_convert;
        uncurry_types = Dont_convert;
        uncurry_values = Dont_convert;
      }
    ]} *)
type do_convert = {
  convert_names : convert_flag ;
}

(** Main configuration for the conversion process.

    This record contains all settings needed to perform SML-to-OCaml
    conversion, including input/output paths, verbosity level, and
    transformation policies.

    {3 Example}

    {[
      let config = {
        input_file = "src/mymodule.sml";
        output_file = Some "lib/mymodule.ml";
        verbosity = Some 2;  (* Detailed output *)
        conversions = {
          pattern_names = Do_convert;
          constructor_names_values = Do_convert;
          function_names = Do_convert;
          uncurry_types = Dont_convert;
          uncurry_values = Dont_convert;
        }
      }
    ]} *)
type config = {
  input_file : string;
      (** Path to the input SML source file. *)
  output_file : string option;
      (** Optional path to the output OCaml file. If [None], output is
          written to stdout. *)
  verbosity : int option;
      (** Optional verbosity level:
          - [None] or [Some 0]: Minimal output (errors only)
          - [Some 1]: Normal output (warnings and progress)
          - [Some 2]: Verbose output (detailed transformation info)
          - [Some 3+]: Debug output (all intermediate representations) *)
  conversions : do_convert;
      (** Transformation policy specifying which conversions to apply. *)
}

(** {1 Configuration Interface}

    Module type for providing configuration to functors. *)

(** Module signature for configuration providers.

    Functors requiring configuration (like the backend) expect modules
    matching this signature. This allows configuration to be statically
    embedded in the functor application.

    {3 Example Implementation}

    {[
      module MyConfig : CONFIG = struct
        let config = {
          input_file = "input.sml";
          output_file = Some "output.ml";
          verbosity = Some 1;
          conversions = default_conversions;
        }
      end
    ]} *)
module type CONFIG = sig
  (** The configuration value to use for conversion. *)
  val config : config
end

(** {1 Tuple Utilities}

    Helper functions for accessing triple components. *)

(** [fst3 (a, b, c)] extracts the first component of a triple.

    Example: [fst3 (1, 2, 3) = 1]

    @param triple A 3-tuple
    @return The first element *)
val fst3 : ('a * 'b * 'c) -> 'a

(** [snd3 (a, b, c)] extracts the second component of a triple.

    Example: [snd3 (1, 2, 3) = 2]

    @param triple A 3-tuple
    @return The second element *)
val snd3 : ('a * 'b * 'c) -> 'b

(** [trd3 (a, b, c)] extracts the third component of a triple.

    Example: [trd3 (1, 2, 3) = 3]

    @param triple A 3-tuple
    @return The third element *)
val trd3 : ('a * 'b * 'c) -> 'c
