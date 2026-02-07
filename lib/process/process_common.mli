(** Common utilities for output validation and error reporting.

    This module provides functionality for validating generated OCaml code and
    formatting error messages with source context. *)

(** Result of validating generated OCaml syntax. *)
type check_result =
  | Good  (** Code is syntactically valid OCaml *)
  | Bad of Syntaxerr.error  (** Syntax error found in generated code *)
  | Err of exn  (** Other error during validation *)

val check_output :
  config:Common.t -> ?output_file:string -> string -> check_result
(** [check_output ~config ?output_file ocaml_code] validates OCaml syntax.

    Parses the generated OCaml code using the OCaml compiler's parser to detect
    syntax errors. If errors are found, provides detailed context showing the
    problematic lines.

    @param config
      Conversion configuration (for input file names in error messages)
    @param output_file Optional path for error reporting (defaults from config)
    @param ocaml_code Generated OCaml source code to validate
    @return
      Validation result: Good, Bad with error details, or Err for other failures
*)
