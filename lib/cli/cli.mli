(** Command-line interface entry point.

    {1 Synopsis}

    This module provides the main command-line interface for the Shibboleth
    SML-to-OCaml converter. It integrates all CLI commands and handles the
    program's entry point when invoked from the terminal.

    {1 Overview}

    The CLI is built using the Cmdliner library and supports multiple subcommands
    for different conversion operations. The primary command is [file] which
    converts individual SML source files to OCaml.

    {1 Usage Example}

    From the command line:
    {v
    shibboleth file input.sml -o output.ml
    shibboleth file --help
    v}

    {1 Architecture}

    The CLI module orchestrates:
    - Command parsing and validation (via Cmdliner)
    - Option processing and configuration setup
    - Delegation to the appropriate conversion pipeline
    - Exit code reporting

    @see <https://github.com/dbuenzli/cmdliner> Cmdliner documentation *)

(** [entrypoint ()] is the main entry point for the CLI application.

    This function:
    - Parses command-line arguments
    - Executes the requested command
    - Returns the exit code

    It should be called once from the executable's main function.

    @return Unit. The function will call [exit] with the appropriate exit code. *)
val entrypoint : unit -> unit
