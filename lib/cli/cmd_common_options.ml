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
  0, the default, is High, 1 is Medium, 2 is Low, and 3 is Debug.
  Increase verbosity for more detailed output.
  |}
  in
  Arg.(value & opt int 0 & info [ "v"; "verbose" ] ~doc)

let conversion_flags : (bool * Common.conversions) Term.t =
  let convert_names_doc =
    {|
    Enable the attaching of attributes to names that are likely invalid in OCaml.
    This tool is a minimal converter: it merely tries to change the syntax in as few ways as possible.
    Most notably, we do *not* try to rename variables or functions that have names invalid in OCaml.
    Enabling this flag `[@sml.bad_name OLD SUGGESTED]` will attach attributes to such definitions of names, which can then be processed by a subsequent renaming tool.
    |}
  in
  let convert_names_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Disable
         (Cmdliner.Arg.info [ "convert-names" ] ~doc:convert_names_doc)
  in
  let convert_comments_doc =
    {|
  Enables conversion of SML comments to OCaml comments.
  The process works by converting SML comments to attributes, which are then replaced with OCaml comments.
  |}
  in
  let convert_comments_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Warn
         (Cmdliner.Arg.info [ "convert-comments" ] ~doc:convert_comments_doc)
  in
  let add_line_numbers_doc =
    {|
  Add line number annotations to the converted code.
  Useful for tracking the correspondence between source and target code.
  |}
  in
  let add_line_numbers_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Disable
         (Cmdliner.Arg.info [ "add-line-numbers" ] ~doc:add_line_numbers_doc)
  in
  let convert_keywords_doc =
    {|
  Convert SML keywords that conflict with OCaml reserved words.
  For example, renaming 'open' to 'open_' when used as an identifier.
  |}
  in
  let convert_keywords_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Warn
         (Cmdliner.Arg.info [ "convert-keywords" ] ~doc:convert_keywords_doc)
  in
  let rename_types_doc =
    {|
  Rename types to follow OCaml naming conventions.
  For example, converting type names to lowercase or handling type variable differences.
  |}
  in
  let rename_types_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Warn
         (Cmdliner.Arg.info [ "rename-types" ] ~doc:rename_types_doc)
  in
  let make_make_functor_doc =
    {|
  Generate 'Make' functor patterns for SML functor conversions.
  This creates idiomatic OCaml functor naming conventions.
  |}
  in
  let make_make_functor_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Note
         (Cmdliner.Arg.info [ "make-make-functor" ] ~doc:make_make_functor_doc)
  in
  let rename_constructors_doc =
    {|
  Rename datatype constructors to follow OCaml conventions.
  SML and OCaml have different capitalization conventions for constructors.
  |}
  in
  let rename_constructors_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Note
         (Cmdliner.Arg.info [ "rename-constructors" ]
            ~doc:rename_constructors_doc) in
  let deref_pattern_doc =
    {|
  Dereference patterns to match OCaml's pattern matching semantics.
  This helps in converting SML patterns that involve references.
  |}  
  in
  let deref_pattern_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Enable
         (Cmdliner.Arg.info [ "deref-pattern" ] ~doc:deref_pattern_doc)
  in let curry_expressions_doc =
    {|
  Curry expressions to match OCaml's function application style.
  This transforms multi-argument functions into curried functions.
  |}  
  in
  let curry_expressions_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Enable
          (Cmdliner.Arg.info [ "curry-expressions" ] ~doc:curry_expressions_doc)
  in let curry_types_doc =
    {|
  Curry types to match OCaml's type system.
  This transforms multi-argument type constructors into curried types.
  |}  
  in
  let curry_types_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Enable
          (Cmdliner.Arg.info [ "curry-types" ] ~doc:curry_types_doc)
  in let tuple_select_doc =
    {|
  Enable tuple selection transformations.
  This allows for converting SML tuple selections to OCaml's equivalent.
  |}  
  in
  let tuple_select_flag : Common.convert_flag Term.t =
    Arg.value
    @@ convert_flag_arg Common.Enable
          (Cmdliner.Arg.info [ "tuple-select" ] ~doc:tuple_select_doc)
  in
  let guess_pattern_doc =
    {|
  Apply heuristics to guess whether identifiers in patterns are constructors or variables.
  This helps distinguish between constructor patterns and variable bindings.
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
    Force overwrite of output directory if it already exists.
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
    Common.mkConversions ~convert_names ~convert_comments ~add_line_numbers
      ~convert_keywords ~rename_types ~make_make_functor ~rename_constructors
      ~guess_pattern ~deref_pattern ~curry_expressions ~curry_types
      ~tuple_select () )

let concat_output : bool Term.t =
  let doc =
    {|
  Concatenate all output into a single file.
  This is enabled by default, due to differences between how SML and OCaml handle modules across multiple files.
  Use --concat-output=false to disable.
  |}
  in
  Arg.(value & opt bool true & info [ "concat-output" ] ~doc)

let quiet : bool Term.t =
  let doc = {|
  Suppress all output except for errors.
  |} in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

let guess_var : string option Term.t =
  let doc =
    {|
  A regular expression pattern to identify variable names that should be converted to __<NAME> format.
  For example, --guess-var="[A-Z]'?" will convert variables like X, Y', etc. to __X, __Y'.
  Module names and qualified names (Module.x) are not affected.
  The regex is applied to the entire variable name.
  |}
  in
  Arg.(value & opt (some string) None & info [ "guess-var" ] ~doc)

let debug : string list Term.t =
  let doc =
    {|
  Enable debug output for specific subsystems.
  Can be specified multiple times to enable multiple debug categories.
  Common categories: 'parser', 'lexer', 'backend', 'names', 'types'.
  |}
  in
  Arg.(value & opt (list string) [] & info [ "debug" ] ~docv:"CATEGORY" ~doc)
let variable_regex_doc =
    {|
  A regular expression pattern to identify variable names that should be converted to lowercase in patterns.
  This is useful for converting SML variable names (which often start with uppercase letters) to OCaml conventions.
  The regex is applied to the last part of the variable name.
  |} 
let variable_regex_flag : string Term.t =
    Arg.(value & opt string "" & info [ "variable-regex" ] ~doc:variable_regex_doc)

let check_ocaml_doc =
  {|
  Enable checking of the generated OCaml code for syntax errors using the OCaml compiler.
  This helps ensure that the converted code is syntactically valid OCaml.
  |}
let check_ocaml_flag : bool Term.t =
  Arg.(value & flag & info [ "check-ocaml" ] ~doc:check_ocaml_doc)  
let common_options : Common.options Cmdliner.Term.t =
  let+ v = verb
  and+ force, c = conversion_flags
  and+ co = concat_output
  and+ q = quiet
  and+ gv = guess_var
  and+ dbg = debug
  and+ var_reg = variable_regex_flag
  and+ check_ocaml = check_ocaml_flag in
  Common.mkOptions ~verbosity:(Some v) ~conversions:c ~concat_output:co ~force
    ~quiet:q ~guess_var:gv ~debug:dbg ~variable_regex:var_reg ~check_ocaml:check_ocaml ()
