open Cmdliner
open Cmdliner.Term.Syntax
include Process
open Cmd_convert_file
include Test_cmd

let cmd_convert : int Cmd.t =
  let doc : Cmd.info =
    Cmd.info "shibboleth"
      ~doc:
        {|
    A Standard ML to OCaml source-to-source converter.

    Shibboleth translates Standard ML (SML '97) source code into OCaml using a multi-phase
    compilation pipeline: lexical analysis → parsing → AST transformation → OCaml code generation.

    INTENDED USE:
    This tool performs syntactic conversion from SML to OCaml. It is designed as a minimal converter
    that preserves as much of the original structure as possible, requiring manual intervention for
    semantic differences between the languages. The tool has been primarily tested against the Twelf
    project but aims to support any valid Standard ML (SML '97 specification).

    WORKFLOW:
    1. Convert SML files to OCaml using 'shibboleth file' or 'shibboleth group'
    2. Review generated OCaml code for type errors and semantic issues
    3. Use conversion flags to handle identifier conflicts and naming conventions
    4. Manually adjust code where SML and OCaml semantics differ significantly

    CONVERSION APPROACH:
    - Minimal transformation: Only syntax is changed, structure is preserved
    - No automatic renaming: Invalid OCaml identifiers are flagged with attributes (when enabled)
    - Semantic preservation: Most SML constructs map directly to OCaml equivalents
    - Manual refinement expected: Type signatures and module interfaces may need adjustment

    TYPICAL USE CASES:
    - Migrating legacy SML codebases to OCaml
    - Prototyping OCaml implementations from SML specifications
    - Cross-language learning and comparison
    - Automated initial conversion followed by manual refinement
    |}
      ~man:
        [
          `S "EXAMPLES";
          `P "Convert a single SML file to OCaml:";
          `Pre "  shibboleth file input.sml -o output.ml";
          `P
            "Convert multiple related SML files (signature, functor, \
             structure) in recommended order:";
          `Pre "  shibboleth file module.sig module.fun module.sml -o module.ml";
          `P "Convert an entire directory of SML files:";
          `Pre "  shibboleth group --input ./sml_src --output ./ocaml_src";
          `P "Enable name conflict detection with attributes:";
          `Pre "  shibboleth file input.sml --convert-names=enable -o output.ml";
          `P "Disable comment conversion and use quiet mode:";
          `Pre
            "  shibboleth file input.sml --convert-comments=disable --quiet -o \
             output.ml";
          `P "Convert with variable pattern guessing for uppercase variables:";
          `Pre
            "  shibboleth file input.sml --guess-var=\"[A-Z]'?\" -o output.ml";
          `S "CONVERSION FLAGS";
          `P "Most conversion features use a four-level flag system:";
          `I ("$(b,enable)", "Apply the conversion and proceed without warnings");
          `I ("$(b,warn)", "Apply the conversion and emit warnings to stderr");
          `I ("$(b,note)", "Apply the conversion and emit informational notes");
          `I ("$(b,disable)", "Skip the conversion entirely");
          `P
            "Different flags have different defaults based on their \
             reliability and impact.";
          `S "FILE ORDERING";
          `P
            "When converting multiple related SML files, provide them in \
             dependency order:";
          `Pre "  %.sig %.fun %.sml";
          `P
            "This ensures that signature definitions are processed before \
             functor applications and structure implementations, which helps \
             the converter properly resolve names and maintain correct \
             scoping.";
          `S "SEE ALSO";
          `P "For detailed options for each subcommand:";
          `Pre "  shibboleth file --help\n  shibboleth group --help";
        ]
  in
  Cmd.group doc [ cmd_convert_file; cmd_convert_group ]

let main () = Cmd.eval' cmd_convert
let entrypoint () = if !Sys.interactive then () else exit (main ())
