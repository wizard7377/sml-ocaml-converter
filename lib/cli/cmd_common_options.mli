(** Common command-line options shared across CLI commands.

    {1 Synopsis}

    This module defines command-line options that are common to multiple
    subcommands in the CLI interface. It provides a Cmdliner term for
    parsing these shared options.

    {1 Overview}

    Common options include:
    - Verbosity level ([--verbose], [-v])
    - Conversion flags controlling which transformations to apply

    These options are combined with command-specific options to create
    complete command-line interfaces.

    {1 Usage Example}

    In a Cmdliner command definition:
    {[
      let cmd =
        let open Cmdliner in
        let doc = "Convert SML file to OCaml" in
        let info = Cmd.info "convert" ~doc in
        Cmd.v info Term.(const convert_file $ common_options $ file_arg)
    ]}

    @see <https://github.com/dbuenzli/cmdliner> Cmdliner documentation *)

(** Common options record.

    This record collects all command-line options that are shared across
    multiple commands. It provides a convenient way to pass these options
    to command handlers.

    {3 Fields}

    - [verbose]: Verbosity level (0 = quiet, 1 = normal, 2+ = verbose)
    - [conversions]: Flags controlling which semantic transformations to apply *)

val common_options : Common.options Cmdliner.Term.t
