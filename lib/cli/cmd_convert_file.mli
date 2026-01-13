(** File conversion CLI command.

    {1 Synopsis}

    This module implements the [file] subcommand for converting individual
    SML source files to OCaml. It is the primary command for single-file
    conversion operations.

    {1 Overview}

    The [file] command handles:
    - Parsing SML source from a file
    - Applying name transformations
    - Converting to OCaml Parsetree
    - Writing output to a file or stdout

    {1 Usage}

    From the command line:
    {v
    shibboleth file input.sml -o output.ml
    shibboleth file input.sml --verbose
    shibboleth file input.sml --no-constructor-names
    v}

    {1 Command Arguments}

    - Positional argument: Input SML file path
    - [-o FILE], [--output FILE]: Output file (default: stdout)
    - Common options from {!Cmd_common_options} *)

val cmd_convert_file : int Cmdliner.Cmd.t

(** [cmd_convert_file] is the Cmdliner command definition for file conversion.

    This command can be added to the CLI's command group to enable
    single-file conversion functionality.

    @return A Cmdliner command that returns an exit code (0 for success) *)


val cmd_convert_group : int Cmdliner.Cmd.t

(** [cmd_convert_group] is the Cmdliner command definition for group conversion.

    This command can be added to the CLI's command group to enable
    directory-based batch conversion functionality.

    @return A Cmdliner command that returns an exit code (0 for success) *)