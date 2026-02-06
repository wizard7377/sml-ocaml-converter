type path

exception Dir_exists of path
exception Input_output_same_dir of path
exception Dir_create_error of path

val path_to_string : path -> string
val string_to_path : string -> path

val convert_file :
  input_files:path list -> ?output_file:path -> options:Common.t -> int
(** Convert a single SML file to OCaml.

    This function handles the end-to-end conversion of a single SML source file
    to an OCaml source file. It reads the input file, processes it through
    parsing, conversion, and pretty-printing, and writes the output to the
    specified location.

    {3 Parameters}

    - [input_files]: List of input SML file paths (should contain exactly one
      file)
    - [output_file]: Optional output OCaml file path. If not provided, the
      output will be written to the same directory as the input file with a .ml
      extension.
    - [options]: Conversion options including verbosity and transformation flags

    {3 Return Value}

    Returns an integer status code:
    - [0] on successful conversion
    - Non-zero on failure with appropriate error messages printed to stderr

    {1 Usage Example}

    {[
      let status =
        convert_file [ "input.sml" ] ~output_file:"output.ml" options
      in
      if status <> 0 then prerr_endline "Conversion failed."
    ]} *)

val convert_group : input_dir:path -> output_dir:path -> options:Common.t -> int
(** Convert a group of SML files in a directory to OCaml.

    This function processes all SML source files within the specified input
    directory, converting each to OCaml and writing the results to the specified
    output directory. It maintains the directory structure and handles multiple
    files in a batch operation.

    {3 Parameters}

    - [input_dir]: Path to the input directory containing SML files
    - [output_dir]: Path to the output directory for OCaml files
    - [options]: Conversion options including verbosity and transformation flags

    {3 Return Value}

    Returns an integer status code:
    - [0] if all files were converted successfully
    - Non-zero if any file failed to convert, with error messages printed to
      stderr

    {1 Usage Example}

    {[
      let status =
        convert_group (Fpath.v "sml_sources") (Fpath.v "ocaml_outputs") options
      in
      if status <> 0 then prerr_endline "One or more conversions failed."
    ]} *)
