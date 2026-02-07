open Cmdliner
open Cmdliner.Term.Syntax
open Cmdliner.Term

let convert_flag_conv : Common.convert_flag Cmdliner.Arg.conv =
  let flag = function
    | "enable" -> Ok Common.Enable
    | "warn" -> Ok Common.Warn
    | "note" -> Ok Common.Note
    | "disable" -> Ok Common.Disable
    | s ->
        Error
          (`Msg
             (Printf.sprintf
                "Invalid convert flag: %s. Expected one of: enable, warn, \
                 note, disable."
                s))
  in
  let print fmt v =
    let s =
      match v with
      | Common.Enable -> "enable"
      | Common.Warn -> "warn"
      | Common.Note -> "note"
      | Common.Disable -> "disable"
    in
    Format.fprintf fmt "%s" s
  in
  Arg.conv (flag, print)

let convert_flag_arg :
    Common.convert_flag ->
    Cmdliner.Arg.info ->
    Common.convert_flag Cmdliner.Arg.t =
 fun d info -> Arg.(opt convert_flag_conv d) info

let verb : int Term.t =
  let doc =
    {|
  Control output verbosity level (0-3).

  Verbosity levels:
  - 0 (High): Only show important messages and errors (default)
  - 1 (Medium): Include warnings and conversion notes
  - 2 (Low): Show detailed conversion progress
  - 3 (Debug): Enable full debug output with internal details

  Higher verbosity levels are useful for diagnosing conversion issues or understanding
  what transformations are being applied to your code.
  |}
  in
  Arg.(value & opt int 0 & info [ "v"; "verbose" ] ~doc)

let conversion_flags : (bool * Common.t) Term.t =
  let convert_names_doc =
    {|
    Flag conversion of identifiers that are invalid in OCaml with attributes.

    BEHAVIOR:
    Shibboleth is a minimal converter that preserves original identifier names. This means
    that SML identifiers which are invalid in OCaml (e.g., names with special characters,
    or names that don't match OCaml's lexical rules) are left unchanged, which may cause
    OCaml compilation errors.

    When enabled, this flag attaches attributes to such identifiers:
      [@sml.bad_name "ORIGINAL_NAME" "SUGGESTED_NAME"]

    These attributes serve as markers for post-processing tools or manual review,
    indicating which names need to be changed for valid OCaml code.

    RECOMMENDATION:
    Enable this when converting large codebases to identify all problematic names
    systematically, then use a separate renaming pass or tool to fix them.

    Default: disable (no attributes are added)
    |}
  in
  let convert_names_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Disable
         (Cmdliner.Arg.info [ "convert-names" ] ~doc:convert_names_doc)
  in
  let convert_keywords_doc =
    {|
  Handle SML identifiers that conflict with OCaml reserved keywords.

  BEHAVIOR:
  SML and OCaml have different sets of reserved keywords. An identifier that's
  valid in SML might be a keyword in OCaml. This flag handles such conflicts by
  appending an underscore to the identifier.

  Examples:
  - 'method' → 'method_' (OCaml keyword)
  - 'object' → 'object_' (OCaml keyword)
  - 'private' → 'private_' (OCaml keyword)

  RECOMMENDATION:
  Keep enabled (warn) to automatically handle keyword conflicts while being notified
  of the transformations. The renamed identifiers remain semantically equivalent.

  Default: warn (convert keywords and report changes)
  |}
  in
  let convert_keywords_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Warn
         (Cmdliner.Arg.info [ "convert-keywords" ] ~doc:convert_keywords_doc)
  in
  let rename_types_doc =
    {|
  Transform type names to follow OCaml naming conventions.

  TRANSFORMATIONS:
  - Type variable handling: SML uses 'a, ''a vs OCaml 'a
  - Equality type variables: SML ''a → OCaml 'a (with warning about semantics)
  - Type constructor capitalization adjustments
  - Built-in type mappings (e.g., SML 'int' may need adjustment in context)

  DIFFERENCES:
  SML and OCaml have subtle differences in type system features. This flag helps
  bridge syntactic differences but cannot resolve semantic incompatibilities
  (e.g., SML's equality types have no direct OCaml equivalent).

  RECOMMENDATION:
  Enable with warnings to catch type naming issues that might cause compilation errors.

  Default: warn (rename types and report changes)
  |}
  in
  let rename_types_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Warn
         (Cmdliner.Arg.info [ "rename-types" ] ~doc:rename_types_doc)
  in
  let make_make_functor_doc =
    {|
  Transform SML functors to use idiomatic OCaml 'Make' naming pattern.

  BEHAVIOR:
  OCaml convention is to name functors 'Make' when they create modules from parameters.
  This flag renames SML functors to follow this convention where appropriate.

  Example transformation:
    SML: functor MyFunctor(X: SIG) = ...
    OCaml: module Make(X: SIG) = ...

  WHEN TO USE:
  Enable for codebases that will be maintained as OCaml projects and should follow
  OCaml idioms. Disable if preserving original SML functor names is important for
  cross-reference or documentation purposes.

  Default: note (apply transformation and log informational notes)
  |}
  in
  let make_make_functor_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Note
         (Cmdliner.Arg.info [ "make-make-functor" ] ~doc:make_make_functor_doc)
  in
  let guess_pattern_doc =
    {|
  Use heuristics to classify pattern identifiers as constructors or variables.

  THE PROBLEM:
  In patterns, it's sometimes ambiguous whether an identifier is a constructor or a variable:
    case x of Foo => ...  (* Is Foo a constructor or variable binding? *)

  SML uses capitalization as a hint, but not a rule. OCaml requires constructors to be
  capitalized and variables to be lowercase.

  HEURISTICS APPLIED:
  - Starts with uppercase → likely constructor
  - Previously seen as constructor → constructor
  - Appears in a datatype definition → constructor
  - Context-based disambiguation using local definitions

  LIMITATIONS:
  Heuristics are not perfect. Review generated code for incorrect classifications,
  especially for identifiers that violate typical naming conventions.

  RECOMMENDATION:
  Enable with warnings to catch ambiguous cases that need manual review.

  Default: warn (apply heuristics and report uncertain cases)
  |}
  in
  let guess_pattern_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Warn
         (Cmdliner.Arg.info [ "guess-pattern" ] ~doc:guess_pattern_doc)
  in
  let curry_expressions_doc =
    {|
  Convert tuple-argument functions to curried form.

  DIFFERENCE:
  - SML: Functions often take a single tuple argument: fun f(x, y) = ...
  - OCaml: Functions typically use currying: fun f x y = ...

  TRANSFORMATION:
  This flag converts SML tuple-argument functions to OCaml's curried style:
    SML: fun add(x, y) = x + y
    OCaml: let add x y = x + y

  BENEFITS:
  - Enables partial application
  - Follows OCaml conventions
  - Simplifies function composition

  TRADE-OFFS:
  May change the calling convention for some functions. Review generated code
  to ensure call sites are updated correctly.

  Default: enable (apply transformation silently)
  |}
  in
  let curry_expressions_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Enable
         (Cmdliner.Arg.info [ "curry-expressions" ] ~doc:curry_expressions_doc)
  in
  let curry_types_doc =
    {|
  Convert tuple-argument function types to curried form.

  DIFFERENCE:
  - SML: Function types with tuple arguments: (int * string) -> result
  - OCaml: Curried function types: int -> string -> result

  TRANSFORMATION:
  This flag converts function type signatures from tuple form to curried form:
    SML: val f : int * string -> result
    OCaml: val f : int -> string -> result

  This works in conjunction with --curry-expressions to ensure type signatures
  match the converted function implementations.

  RECOMMENDATION:
  Keep enabled when using --curry-expressions to maintain type consistency.

  Default: enable (apply transformation silently)
  |}
  in
  let curry_types_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Enable
         (Cmdliner.Arg.info [ "curry-types" ] ~doc:curry_types_doc)
  in
  let guess_pattern_doc =
    {|
  Use heuristics to classify pattern identifiers as constructors or variables.

  THE PROBLEM:
  In patterns, it's sometimes ambiguous whether an identifier is a constructor or a variable:
    case x of Foo => ...  (* Is Foo a constructor or variable binding? *)

  SML uses capitalization as a hint, but not a rule. OCaml requires constructors to be
  capitalized and variables to be lowercase.

  HEURISTICS APPLIED:
  - Starts with uppercase → likely constructor
  - Previously seen as constructor → constructor
  - Appears in a datatype definition → constructor
  - Context-based disambiguation using local definitions

  LIMITATIONS:
  Heuristics are not perfect. Review generated code for incorrect classifications,
  especially for identifiers that violate typical naming conventions.

  RECOMMENDATION:
  Enable with warnings to catch ambiguous cases that need manual review.

  Default: warn (apply heuristics and report uncertain cases)
  |}
  in
  let guess_pattern_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Warn
         (Cmdliner.Arg.info [ "guess-pattern" ] ~doc:guess_pattern_doc)
  in
  let convert_force_flag : bool Term.t =
    let doc =
      {|
    Force overwrite of existing output directory and files.

    BEHAVIOR:
    By default, the converter refuses to write to an existing output directory to prevent
    accidental data loss. Enable this flag to bypass the safety check and overwrite any
    existing files.

    CAUTION:
    This will irreversibly replace existing files. Use with care, especially when working
    with the 'group' command that processes entire directories.

    RECOMMENDATION:
    Only use --force when you're certain you want to replace existing converted files,
    or when re-running conversion after adjusting flags.
    |}
    in
    Arg.(value & flag & info [ "force" ] ~doc)
  in

  let+ convert_names = convert_names_flag
  and+ convert_keywords = convert_keywords_flag
  and+ rename_types = rename_types_flag
  and+ make_make_functor = make_make_functor_flag
  and+ guess_pattern = guess_pattern_flag
  and+ force = convert_force_flag
  and+ curry_expressions = curry_expressions_flag
  and+ curry_types = curry_types_flag in
  ( force,
    Common.create
      Common.
        [
          set Convert_names convert_names;
          set Convert_keywords convert_keywords;
          set Rename_types rename_types;
          set Make_make_functor make_make_functor;
          set Guess_pattern guess_pattern;
          set Curry_expressions curry_expressions;
          set Curry_types curry_types;
        ] )

let dash_to_underscore_doc =
  {|
  Replace dashes with underscores in generated OCaml filenames.

  BEHAVIOR:
  OCaml module names are derived from filenames and cannot contain dashes.
  If your SML files use dashes in names (e.g., parser-utils.sml), the generated
  OCaml filename will automatically convert them:
    parser-utils.sml → parser_utils.ml

  MODULE NAMES:
  The OCaml module name will be: Parser_utils

  WHEN TO USE:
  Enable this when converting SML projects that use dashed filenames.
  Disable if you want to manually handle filename transformations or if your
  SML files already follow underscore conventions.

  Default: disabled (preserve original filename patterns)
  |}

let dash_to_underscore_flag : bool Term.t =
  Arg.(value & flag & info [ "dash-to-underscore" ] ~doc:dash_to_underscore_doc)

let concat_output : bool Term.t =
  let doc =
    {|
  Concatenate multiple input files into a single output file.

  RATIONALE:
  SML and OCaml have fundamentally different module systems:
  - SML: Allows spreading definitions across multiple files with implicit linking
  - OCaml: Each file defines exactly one module; cross-file references require explicit module paths

  When enabled (default), all input files are merged into one output file, preserving
  the SML code's structure without requiring extensive module system refactoring.

  WHEN TO DISABLE:
  Set to false (--concat-output=false) if:
  - You want separate OCaml files for each SML file
  - You plan to manually restructure the module system
  - You're converting independent SML files that don't reference each other

  WARNING:
  Disabling concatenation may require manual addition of module paths and open statements
  to maintain the original code's semantics.

  Default: true (concatenate for easier initial conversion)
  |}
  in
  Arg.(value & opt bool true & info [ "concat-output" ] ~doc)

let quiet : bool Term.t =
  let doc =
    {|
  Suppress all output except for errors.

  BEHAVIOR:
  Silences informational messages, progress indicators, and warnings.
  Only critical errors that prevent conversion are displayed.

  USE CASES:
  - Scripted batch processing where output is unwanted
  - CI/CD pipelines where only failures matter
  - Large conversions where progress messages are excessive

  NOTE:
  Quiet mode doesn't suppress conversion flag warnings (enable/warn/note).
  To reduce those messages, adjust individual flag levels to 'enable' or 'disable'.

  Default: disabled (show normal output)
  |}
  in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

let guess_var : string option Term.t =
  let doc =
    {|
  Regular expression pattern for converting certain variable names to __<NAME> format.

  PURPOSE:
  SML often uses single uppercase letters or primed variables (X, Y, X', etc.) as generic
  variable names. In OCaml, variables must start with lowercase or underscore. This flag
  helps systematically convert such variables.

  BEHAVIOR:
  Variables matching the regex are prefixed with double underscores:
    --guess-var="[A-Z]'?"
      X → __X
      Y' → __Y'
      foo → foo (no match, unchanged)

  SCOPE:
  - Applied only to variable names in patterns and expressions
  - Module names and qualified identifiers (Module.x) are not affected
  - The regex matches the entire variable name, not just a substring

  EXAMPLE PATTERNS:
  - "[A-Z]'?" - Single uppercase letters with optional prime
  - "[A-Z][0-9]*" - Uppercase letter followed by digits (X1, Y2)
  - "[a-z]'" - Lowercase letter with prime

  Default: none (no automatic variable conversion)
  |}
  in
  Arg.(value & opt (some string) None & info [ "guess-var" ] ~doc)

let debug : string list Term.t =
  let doc =
    {|
  Enable detailed debug output for specific conversion subsystems.

  USAGE:
  Specify multiple times to enable multiple debug categories:
    --debug=parser --debug=backend

  AVAILABLE CATEGORIES:
  - 'parser' - Show parse tree construction and syntax analysis
  - 'lexer' - Display tokenization process
  - 'backend' - Trace AST-to-Parsetree conversion steps
  - 'names' - Show name resolution and transformation decisions
  - 'types' - Display type conversion and inference details

  OUTPUT:
  Debug messages are written to stderr with category prefixes:
    [parser] Parsing declaration...
    [backend] Converting expression...

  USE CASES:
  - Diagnosing conversion failures
  - Understanding why certain transformations occurred
  - Debugging unexpected output
  - Development and testing of the converter itself

  NOTE:
  Debug output can be very verbose. Use specific categories rather than enabling all.

  Default: none (no debug output)
  |}
  in
  Arg.(value & opt (list string) [] & info [ "debug" ] ~docv:"CATEGORY" ~doc)

let check_ocaml_doc =
  {|
  Validate generated OCaml code for syntax errors using the OCaml compiler.

  BEHAVIOR:
  After conversion, passes the generated OCaml code through the OCaml compiler's
  syntax checker (equivalent to: ocamlc -stop-after parsing).

  VALIDATION:
  - Checks for syntax errors only (not type errors)
  - Reports parse failures with line and column numbers
  - Does not compile or generate bytecode
  - Exits with error if syntax validation fails

  USE CASES:
  - Catch conversion bugs that produce invalid OCaml syntax
  - Verify that all transformations preserve syntactic validity
  - Quality assurance during batch conversions
  - Integration into CI/CD pipelines for automated testing

  REQUIREMENTS:
  Requires 'ocamlc' to be available in PATH.

  LIMITATIONS:
  - Only checks syntax, not semantics or types
  - Type errors and semantic issues require manual review
  - Generated code may be syntactically valid but semantically incorrect

  Default: disabled (no automatic validation)
  |}

let check_ocaml_flag : bool Term.t =
  Arg.(value & flag & info [ "check-ocaml" ] ~doc:check_ocaml_doc)

let remove_constructor_manifest_doc =
  {|
  Control generation of constructor manifest files (.ctx) alongside OCaml output.

  BEHAVIOR:
  When converting SML datatypes, the converter generates a constructor manifest file
  (with .ctx extension) that lists all constructors and their arities. This is used
  for post-processing and tooling support.

  This flag controls whether the manifest file is generated:
    --remove-constructor-manifest=true (default): Do not generate .ctx files.
    --remove-constructor-manifest=false: Generate .ctx files as usual.

  USE CASES:
  - Enable (true) to skip generating manifest files if you don't need them or want to
    manage constructor information manually.
  - Disable (false) to keep the default behavior of generating manifest files for tooling.

  Default: true (do not generate constructor manifest files)
  |}

let remove_constructor_manifest_flag : bool Term.t =
  Arg.(
    value & opt bool true
    & info
        [ "remove-constructor-manifest" ]
        ~doc:remove_constructor_manifest_doc)

let common_options : Common.t Cmdliner.Term.t =
  let+ v = verb
  and+ force, c = conversion_flags
  and+ co = concat_output
  and+ q = quiet
  and+ gv = guess_var
  and+ dbg = debug
  and+ check_ocaml = check_ocaml_flag
  and+ dash_to_underscore = dash_to_underscore_flag in
  Common.create
    Common.
      [
        set Verbosity v;
        set Convert_names (Common.get Convert_names c);
        set Convert_keywords (Common.get Convert_keywords c);
        set Rename_types (Common.get Rename_types c);
        set Make_make_functor (Common.get Make_make_functor c);
        set Guess_pattern (Common.get Guess_pattern c);
        set Curry_expressions (Common.get Curry_expressions c);
        set Curry_types (Common.get Curry_types c);
        set Concat_output (Common.get Concat_output c);
        set Force force;
        set Quiet q;
        set Guess_var gv;
        set Debug dbg;
        set Check_ocaml check_ocaml;
        set Dash_to_underscore dash_to_underscore;
      ]
