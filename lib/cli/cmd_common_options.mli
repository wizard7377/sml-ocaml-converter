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
type common_options = {
  verbose : int option;
      (** Optional verbosity level. [None] uses default verbosity.
          Higher numbers produce more detailed output. *)
  conversions : Common.do_convert;
      (** Conversion policy specifying which transformations to enable.
          See {!Common.do_convert} for details on each flag. *)
}

(** [common_options] is a Cmdliner term for parsing common options.

    This term can be combined with other terms to build complete command
    definitions. It handles parsing of:
    - [-v], [--verbose]: Increase verbosity (can be repeated)
    - [--no-pattern-names]: Disable pattern name conversion
    - [--no-constructor-names]: Disable constructor name conversion
    - [--no-function-names]: Disable function name conversion
    - [--uncurry-types]: Enable type uncurrying
    - [--uncurry-values]: Enable value uncurrying

    @return A Cmdliner term that produces a {!common_options} value *)
val common_options : common_options Cmdliner.Term.t
