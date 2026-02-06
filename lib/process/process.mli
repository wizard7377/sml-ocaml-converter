(** Conversion pipeline orchestration.

    {1 Synopsis}

    This module provides the high-level orchestration of the SML-to-OCaml
    conversion process. It coordinates the lexer, parser, backend
    transformation, and output generation stages.

    {1 Overview}

    The conversion pipeline consists of:
    - Reading input (individual files or CM compilation manager files)
    - Parsing SML source code to AST
    - Name resolution and context building
    - Backend transformation to OCaml Parsetree
    - Pretty-printing and output writing

    {1 Architecture}

    The {!process} class encapsulates the entire conversion state:
    - Configuration settings ({!Common.t})
    - Name store for tracking identifiers ({!Names.Store.t})
    - Input/output handling

    {1 Usage Example}

    {[
      let config =
        {
          Common.input_file = "example.sml";
          output_file = Some "example.ml";
          verbosity = Some 1;
          conversions = default_conversions;
        }
      in
      let processor = new process config in
      let exit_code = processor#run (File [ "example.sml" ]) in
      exit exit_code
    ]}

    {1 Type Definitions} *)

open Common

type sml_code
(** Internal representation of SML source code.

    This type is used during the parsing phase to hold the raw SML source text
    before it's transformed into an AST. *)

type ocaml_code
(** Internal representation of OCaml source code.

    This type is used during the output phase to hold the pretty-printed OCaml
    source text before it's written to a file. *)

type input = Common.source
(** Input specification for the conversion process.

    The converter can process either:
    - Individual SML source files
    - CM (Compilation Manager) files that specify multiple source files

    {3 Examples}

    Process a single file:
    {[
      File [ "mymodule.sml" ]
    ]}

    Process multiple files:
    {[
      File [ "module1.sml"; "module2.sml"; "module3.sml" ]
    ]}

    Process a CM file:
    {[
      CM "sources.cm"
    ]} *)

type output = Common.target
(** Output result of the conversion process.

    Returns an exit code:
    - [0] indicates successful conversion
    - Non-zero indicates an error occurred *)

(** Main conversion processor class.

    This class provides stateful management of the conversion pipeline,
    including configuration, name resolution context, and execution.

    {3 Parameters}

    @param store
      Optional initial name store. If not provided, a fresh store is created.
      Useful for pre-populating with standard library names or for incremental
      compilation.
    @param config
      Conversion configuration including input/output paths, verbosity level,
      and conversion flags. *)
class process : ?store:Context.t -> Common.t -> object
  method set_config : Common.t -> unit
  (** [set_config cfg] updates the processor's configuration.

      @param cfg New configuration to use for subsequent conversions. *)

  method get_config : unit -> Common.t
  (** [get_config ()] retrieves the current configuration.

      @return The active configuration settings. *)

  method get_store : unit -> Context.t
  (** [get_store ()] retrieves the current name store.

      The name store contains all discovered identifiers and their contexts from
      the conversion process. This is useful for:
      - Debugging name resolution
      - Incremental compilation
      - Analyzing identifier transformations

      @return The accumulated name store. *)

  method set_store : Context.t -> unit
  (** [set_store store] replaces the processor's name store.

      This allows:
      - Reusing name context across multiple conversions
      - Pre-populating with standard library identifiers
      - Testing specific name resolution scenarios

      @param store New name store to use. *)

  method run : input -> int
  (** [run input] executes the conversion pipeline.

      This is the main entry point for performing SML-to-OCaml conversion. The
      method will:
      - Parse the input SML source(s)
      - Extract and resolve names
      - Transform to OCaml Parsetree
      - Pretty-print and write output
      - Report errors if any stage fails

      @param input Input specification (files or CM file)
      @return Exit code: 0 for success, non-zero for failure *)
end
