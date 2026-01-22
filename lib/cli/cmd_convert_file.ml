open Cmdliner
open Cmdliner.Term.Syntax
include Cmd_common_options
include Process

module type Command_S = sig
  val run_cmd : int Cmd.t
end

module Convert_File : Command_S = struct
  let output : string option Term.t =
    let doc = {|
  Output path for the converted OCaml file.
  If not specified, output is written to stdout.
  When converting multiple input files, they are typically concatenated into a single output file
  (controlled by --concat-output flag).
  |} in
    Arg.(
      value
      & opt (some string) None
      & info [ "o"; "output" ] ~doc ~docv:"OUTPUT")

  let input : string list Term.t =
    let doc =
      {|
  Path(s) to Standard ML source file(s) to convert.

  MULTIPLE FILES:
  Multiple files can be specified by separating them with spaces. When converting a module
  split across multiple files (signature, functor, structure), it is HIGHLY RECOMMENDED to
  provide them in dependency order: %.sig %.fun %.sml

  This ordering ensures:
  - Signature definitions are processed before implementations
  - Functor parameters are known before functor applications
  - Name resolution follows the correct scoping rules
  - Type information flows correctly through the conversion

  SINGLE FILE:
  For standalone SML files, simply provide the path. The converter will process the entire
  file and generate equivalent OCaml code.
  |}
    in
    Arg.(non_empty (pos_all string [] & info [] ~doc ~docv:"INPUT"))

  let run_cmd : int Cmd.t =
    let doc =
      {|
  Convert one or more Standard ML source files to OCaml.

  This command performs a multi-phase source-to-source transformation:
  1. Lexical analysis and parsing of SML source(s)
  2. Construction of SML abstract syntax tree (AST)
  3. AST transformation and conversion to OCaml Parsetree
  4. Pretty-printing to OCaml source code

  The converter applies various transformations controlled by conversion flags, including:
  - Identifier renaming for OCaml keyword conflicts
  - Pattern constructor/variable disambiguation
  - Type and expression currying
  - Comment conversion
  - Reference pattern dereferencing

  OUTPUT BEHAVIOR:
  By default, all input files are concatenated into a single output file. This reflects the
  difference in module systems between SML and OCaml. Use --concat-output=false to generate
  separate files (though this may require manual module system adjustments).
  |}
    in
    Cmd.v (Cmd.info "file" ~doc ~docs:"SML Converter"
      ~man:[
        `S "EXAMPLES";
        `P "Convert a single file to stdout:";
        `Pre "  shibboleth file input.sml";
        `P "Convert to a specific output file:";
        `Pre "  shibboleth file input.sml -o output.ml";
        `P "Convert multiple related files in recommended order:";
        `Pre "  shibboleth file types.sig module.fun impl.sml -o combined.ml";
        `P "Convert with all name warnings enabled:";
        `Pre "  shibboleth file input.sml --convert-names=warn --guess-pattern=warn -o output.ml";
        `P "Quiet conversion with syntax checking:";
        `Pre "  shibboleth file input.sml -o output.ml --quiet --check-ocaml";
        `S "FILE ORDERING";
        `P "When providing multiple files that form a single logical module, use this order:";
        `Pre "  1. Signature files (%.sig)\n  2. Functor files (%.fun)\n  3. Structure files (%.sml)";
        `P "This ensures proper name resolution and type information flow during conversion.";
        `S "OUTPUT CONTROL";
        `P "Control output behavior with these flags:";
        `I ("--concat-output=true", "Merge all inputs into one file (default)");
        `I ("--concat-output=false", "Generate separate output files");
        `I ("--quiet", "Suppress progress messages, show only errors");
        `I ("--check-ocaml", "Validate generated OCaml with the compiler");
      ])
    @@ let+ output = output
       and+ input = input
       and+ common_options = common_options in
       Toplevel.convert_file ~options:common_options
         ?output_file:(Option.map Toplevel.string_to_path output)
         ~input_files:(List.map Toplevel.string_to_path input)
end

module Group_Convert : Command_S = struct
  

  let output_dir : string Term.t =
    let doc = {|
  Output directory for converted OCaml files.

  The directory structure of the input will be preserved in the output directory.
  If the output directory does not exist, it will be created automatically.
  If the output directory exists and contains files, use --force to overwrite.

  Generated filenames are derived from input filenames with .ml extension.
  Use --dash-to-underscore to convert dashes in filenames to underscores for
  OCaml module name compatibility.
  |} in
    Arg.(
      required
      & opt (some string) None
      & info [ "output" ] ~doc ~docv:"OUTPUT_DIR")

  let input_dir : string Term.t =
    let doc =
      {|
  Input directory containing Standard ML source files.

  RECURSIVE PROCESSING:
  All SML source files in this directory and its subdirectories will be discovered and converted.
  The directory structure is preserved in the output, maintaining relative paths.

  RECOGNIZED EXTENSIONS:
  The converter automatically identifies SML source files by extension:
  - .sml (structure implementations)
  - .sig (signatures)
  - .fun (functors)

  FILE GROUPING:
  Related files (e.g., module.sig, module.fun, module.sml) are automatically grouped
  and processed together when possible, ensuring proper name resolution.
  |}
    in
    Arg.(
      required
      & opt (some string) None
      & info [ "input" ] ~doc ~docv:"INPUT_DIR")

  let run_cmd : int Cmd.t =
    let doc =
      {|
  Batch convert a directory of Standard ML source files to OCaml.

  This command performs recursive directory traversal to find all SML source files
  (.sml, .sig, .fun) and converts them to OCaml while preserving the directory structure.

  BATCH PROCESSING:
  - Discovers all SML files recursively in the input directory
  - Groups related files (signature, functor, implementation) automatically
  - Preserves directory structure in the output
  - Applies consistent conversion flags to all files
  - Reports progress and errors for each file conversion

  DIRECTORY STRUCTURE:
  Input structure:
    input_dir/
      module1/
        types.sig
        impl.sml
      module2/
        functor.fun

  Output structure:
    output_dir/
      module1/
        types.ml    (or combined file, depending on --concat-output)
        impl.ml
      module2/
        functor.ml

  SAFETY:
  By default, the command will fail if the output directory exists and contains files.
  Use --force to overwrite existing files.
  |}
    in
    Cmd.v (Cmd.info "group" ~doc ~docs:"SML Converter"
      ~man:[
        `S "EXAMPLES";
        `P "Convert an entire SML project directory:";
        `Pre "  shibboleth group --input ./twelf-src --output ./twelf-ocaml";
        `P "Force overwrite existing output directory:";
        `Pre "  shibboleth group --input ./src --output ./out --force";
        `P "Convert with filename normalization:";
        `Pre "  shibboleth group --input ./src --output ./out --dash-to-underscore";
        `P "Batch convert with custom conversion flags:";
        `Pre "  shibboleth group --input ./src --output ./out \\\n    --convert-names=enable --guess-pattern=warn";
        `P "Silent batch conversion with syntax validation:";
        `Pre "  shibboleth group --input ./src --output ./out --quiet --check-ocaml";
        `S "FILE DISCOVERY";
        `P "The converter recursively searches for files with these extensions:";
        `I (".sml", "Standard ML structure implementations");
        `I (".sig", "Standard ML signatures");
        `I (".fun", "Standard ML functors");
        `P "Files with the same base name are automatically grouped for conversion.";
        `S "OUTPUT CONTROL";
        `P "Control directory and file creation:";
        `I ("--force", "Overwrite existing output directory");
        `I ("--dash-to-underscore", "Replace dashes with underscores in filenames");
        `I ("--concat-output=true/false", "Combine grouped files or keep separate");
        `S "NOTES";
        `P "Large codebases may take significant time to convert. Use --quiet to reduce \
            output verbosity, or -v to increase it for debugging failed conversions.";
      ])
    @@ let+ output_dir = output_dir
       and+ input_dir = input_dir
       and+ common_options = common_options in
       Toplevel.convert_group ~options:common_options
         ~output_dir:(Toplevel.string_to_path output_dir)
         ~input_dir:(Toplevel.string_to_path input_dir)
end

let cmd_convert_file : int Cmd.t = Convert_File.run_cmd
let cmd_convert_group : int Cmd.t = Group_Convert.run_cmd
