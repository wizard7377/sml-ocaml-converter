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
  let convert_comments_doc =
    {|
  Convert SML comments to OCaml comments.

  PROCESS:
  Standard ML uses (* ... *) style comments (same as OCaml), but comment placement
  and semantics may differ. The converter:
  1. Extracts comments from SML AST nodes
  2. Temporarily converts them to attributes for AST transformation
  3. Replaces attributes with OCaml comments in the final output

  LIMITATIONS:
  - Comment position may shift slightly due to AST transformations
  - Nested comment handling may differ between SML and OCaml
  - Some comment-doc conventions may not translate directly

  WHEN TO DISABLE:
  Disable if comment conversion causes issues or if you plan to re-document
  the converted code manually.

  Default: warn (convert comments and report any issues)
  |}
  in
  let convert_comments_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Warn
         (Cmdliner.Arg.info [ "convert-comments" ] ~doc:convert_comments_doc)
  in
  let add_line_numbers_doc =
    {|
  Annotate converted code with original SML line numbers.

  BEHAVIOR:
  Adds attributes indicating the source line number from the original SML file:
    [@sml.line 42]

  USE CASES:
  - Debugging: Trace OCaml errors back to original SML source
  - Code review: Understand correspondence between SML and OCaml code
  - Incremental migration: Track which parts of converted code map to original
  - Comparison: Side-by-side analysis of SML vs OCaml

  NOTE:
  These annotations are purely informational and don't affect code semantics.
  They can be removed after conversion if no longer needed.

  Default: disable (no line number annotations)
  |}
  in
  let add_line_numbers_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Disable
         (Cmdliner.Arg.info [ "add-line-numbers" ] ~doc:add_line_numbers_doc)
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
  let rename_constructors_doc =
    {|
  Adjust datatype constructor names for OCaml naming conventions.

  CONVENTIONS:
  - Both SML and OCaml require constructors to start with uppercase
  - However, specific naming patterns may differ (e.g., CONSTANT vs Constant)
  - Some SML constructors may use naming styles uncommon in OCaml

  TRANSFORMATIONS:
  This flag applies OCaml-idiomatic naming patterns to constructors, such as:
  - Converting ALL_CAPS to PascalCase
  - Adjusting constructor names that might conflict with types
  - Standardizing multi-word constructor naming

  CAUTION:
  Renaming constructors changes the API. Only enable if you're willing to update
  pattern matches and constructor calls throughout the codebase.

  Default: note (apply transformation and log informational notes)
  |}
  in
  let rename_constructors_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Note
         (Cmdliner.Arg.info [ "rename-constructors" ]
            ~doc:rename_constructors_doc)
  in
  let deref_pattern_doc =
    {|
  Transform reference patterns to match OCaml's reference handling.

  BEHAVIOR:
  SML and OCaml handle references differently in pattern matching:
  - SML allows direct pattern matching on ref cells: ref x
  - OCaml typically requires explicit dereferencing: !x

  This flag automatically transforms SML reference patterns to OCaml equivalents,
  inserting dereferencing operations where needed.

  Example transformation:
    SML: case !r of ref x => ...
    OCaml: match !r with {contents = x} -> ...

  RECOMMENDATION:
  Keep enabled unless you're manually handling reference semantics.

  Default: enable (apply transformation silently)
  |}
  in
  let deref_pattern_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Enable
         (Cmdliner.Arg.info [ "deref-pattern" ] ~doc:deref_pattern_doc)
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
  Convert tuple-argument type constructors to curried form.

  DIFFERENCE:
  - SML: Multi-argument type constructors use tuples: (int * string) list
  - OCaml: Type constructors are curried: (int, string) list

  TRANSFORMATION:
  This flag adjusts type constructor syntax to match OCaml's expectations:
    SML: (int * string) list
    OCaml: (int, string) list  [when used with multi-param type constructors]

  Note: This is separate from currying function types, which use -> in both languages.

  RECOMMENDATION:
  Keep enabled for correct OCaml type syntax.

  Default: enable (apply transformation silently)
  |}
  in
  let curry_types_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Enable
         (Cmdliner.Arg.info [ "curry-types" ] ~doc:curry_types_doc)
  in
  let tuple_select_doc =
    {|
  Transform SML tuple selector syntax to OCaml pattern matching.

  BEHAVIOR:
  SML has built-in tuple selectors #1, #2, etc. to extract tuple components:
    #1 (a, b)  (* returns a *)

  OCaml lacks built-in tuple selectors. This flag converts them to pattern matching:
    fun (a, _) -> a

  TRANSFORMATION:
  SML: #1 tuple_expr
  OCaml: match tuple_expr with (x, _) -> x

  RECOMMENDATION:
  Keep enabled to handle SML tuple selector syntax automatically.

  Default: enable (apply transformation silently)
  |}
  in
  let tuple_select_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Enable
         (Cmdliner.Arg.info [ "tuple-select" ] ~doc:tuple_select_doc)
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
  and+ convert_comments = convert_comments_flag
  and+ add_line_numbers = add_line_numbers_flag
  and+ convert_keywords = convert_keywords_flag
  and+ rename_types = rename_types_flag
  and+ make_make_functor = make_make_functor_flag
  and+ rename_constructors = rename_constructors_flag
  and+ guess_pattern = guess_pattern_flag
  and+ force = convert_force_flag
  and+ deref_pattern = deref_pattern_flag
  and+ curry_expressions = curry_expressions_flag
  and+ curry_types = curry_types_flag
  and+ tuple_select = tuple_select_flag in
  ( force,
    Common.make ~convert_names ~convert_comments ~add_line_numbers
      ~convert_keywords ~rename_types ~make_make_functor ~rename_constructors
      ~guess_pattern ~deref_pattern ~curry_expressions ~curry_types
      ~tuple_select () )

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

let variable_regex_doc =
  {|
  Regular expression pattern for forcing variable names to lowercase in patterns.

  PURPOSE:
  Helps disambiguate SML code where variables start with uppercase (violating OCaml conventions).
  Works in conjunction with pattern heuristics to classify identifiers correctly.

  BEHAVIOR:
  Pattern identifiers matching this regex are treated as variables and converted to lowercase:
    Pattern: Con
    With --variable-regex="Con" → pattern variable: con

  SCOPE:
  - Applied to pattern identifiers only (not expressions)
  - The regex matches the last component of the name (after any module qualification)
  - Works with the pattern guessing system to resolve ambiguities

  DIFFERENCE FROM --guess-var:
  - --variable-regex: Forces lowercase conversion in patterns
  - --guess-var: Adds __ prefix while preserving case

  EXAMPLE:
  SML code with uppercase variables in patterns:
    case x of X => ...
  With --variable-regex="X":
    match x with x -> ...

  Default: empty (no regex-based variable forcing)
  |}

let variable_regex_flag : string Term.t =
  Arg.(
    value & opt string "" & info [ "variable-regex" ] ~doc:variable_regex_doc)

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

let common_options : Common.t Cmdliner.Term.t =
  let+ v = verb
  and+ force, c = conversion_flags
  and+ co = concat_output
  and+ q = quiet
  and+ gv = guess_var
  and+ dbg = debug
  and+ var_reg = variable_regex_flag
  and+ check_ocaml = check_ocaml_flag
  and+ dash_to_underscore = dash_to_underscore_flag in
  Common.make 
    ~verbosity:v 
    ~convert_names:(Common.get Convert_names c)
    ~convert_comments:(Common.get Convert_comments c)
    ~add_line_numbers:(Common.get Add_line_numbers c)
    ~convert_keywords:(Common.get Convert_keywords c)
    ~rename_types:(Common.get Rename_types c)
    ~make_make_functor:(Common.get Make_make_functor c)
    ~rename_constructors:(Common.get Rename_constructors c)
    ~guess_pattern:(Common.get Guess_pattern c)
    ~deref_pattern:(Common.get Deref_pattern c)
    ~curry_expressions:(Common.get Curry_expressions c)
    ~curry_types:(Common.get Curry_types c)
    ~tuple_select:(Common.get Tuple_select c)
    ~concat_output:co 
    ~force
    ~quiet:q 
    ~guess_var:gv 
    ~debug:dbg 
    ~variable_regex:var_reg 
    ~check_ocaml
    ~dash_to_underscore 
    ()
