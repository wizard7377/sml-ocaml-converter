(** Single-file conversion processing.

    {1 Synopsis}

    This module provides the core pipeline for converting individual SML
    source files to OCaml. It handles the complete transformation from
    raw SML text to formatted OCaml output.

    {1 Overview}

    The file processing pipeline consists of three main phases:
    - {b Parsing}: SML source text → SML AST
    - {b Conversion}: SML AST → OCaml Parsetree
    - {b Pretty-printing}: OCaml Parsetree → OCaml source text

    Each phase is exposed as a separate method, allowing fine-grained
    control over the conversion process.

    {1 Architecture}

    The {!process_file} class extends the general processing framework
    with file-specific operations. It maintains:
    - Configuration state
    - Name resolution context
    - Parsing and conversion state

    {1 Usage Example}

    {[
      let config = {
        Common.input_file = "example.sml";
        output_file = Some "example.ml";
        verbosity = Some 1;
        conversions = default_conversions;
      } in
      let processor = new process_file config in

      (* Read and parse *)
      let sml_code = processor#parse_sml (Common.get_file "example.sml") in

      (* Convert *)
      let ocaml_code = processor#convert_to_ocaml sml_code in

      (* Pretty-print *)
      let output = processor#print_ocaml ocaml_code in
      Common.write_file "example.ml" output
    ]}

    {1 Type Definitions} *)

open Common

type sml_code

(** Internal representation of SML source code.

    This abstract type holds the parsed SML AST along with any
    metadata needed for conversion. *)

type ocaml_code

(** Internal representation of OCaml source code.

    This abstract type holds the OCaml Parsetree representation
    ready for pretty-printing. *)

(** File conversion processor class.

    This class provides a stateful pipeline for converting individual
    SML files to OCaml, with separate methods for each conversion phase.

    {3 Constructor Parameters}

    @param store Optional initial name store for pre-populating with
                 known identifiers (e.g., standard library names)
    @param config Conversion configuration including input/output paths
                  and transformation flags *)
class process_file :
  ?store:Context.t
  -> options
  -> object
       method set_config : options -> unit
       (** [set_config cfg] updates the processor's configuration.

      This allows reusing a processor instance with different settings.

      @param cfg New configuration to apply *)

       method get_config : unit -> options
       (** [get_config ()] retrieves the current configuration.

      @return The active configuration settings *)

       method get_store : unit -> Context.t
       (** [get_store ()] retrieves the accumulated name store.

      The store contains all identifiers discovered during parsing
      and their syntactic contexts. Useful for:
      - Debugging name resolution
      - Understanding identifier transformations
      - Incremental compilation scenarios

      @return The current name store *)

       method set_store : Context.t -> unit
       (** [set_store store] replaces the processor's name store.

      This enables:
      - Pre-populating with standard library names
      - Reusing name context across multiple files
      - Testing specific name resolution scenarios

      @param store New name store to use *)

       method parse_sml : string -> sml_code
       (** [parse_sml source] parses SML source text into an internal AST.

      This phase:
      - Lexes the input using ocamllex
      - Parses with Menhir to produce {!Ast.prog}
      - Extracts names into the name store
      - Validates syntactic correctness

      @param source SML source text (typically from a file)
      @return Parsed SML code representation
      @raise Parsing.Parse_error if the source is syntactically invalid *)

       method convert_to_ocaml : sml_code -> ocaml_code
       (** [convert_to_ocaml sml] transforms SML AST to OCaml Parsetree.

      This phase:
      - Resolves names using the name store
      - Applies semantic transformations (records→objects, etc.)
      - Handles name conversions (SOME→Some, etc.)
      - Generates OCaml Parsetree nodes using Ppxlib

      The conversion respects the configuration flags to enable/disable
      specific transformations.

      @param sml Parsed SML code from {!parse_sml}
      @return OCaml code representation
      @raise Failure if conversion encounters unresolvable constructs *)

       method print_ocaml : ocaml_code -> string
       (** [print_ocaml ocaml] pretty-prints OCaml Parsetree to source text.

      This phase:
      - Formats the OCaml AST using the standard pretty-printer
      - Applies consistent indentation and spacing
      - Generates compilable OCaml source code

      @param ocaml OCaml code from {!convert_to_ocaml}
      @return Formatted OCaml source text ready to write to a file *)
     end
