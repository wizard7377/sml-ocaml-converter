open Cmdliner
open Cmdliner.Term.Syntax
open Cmdliner.Term

let verb : int Term.t =
  let doc = {|
  0, the default, is High, 1 is Medium, 2 is Low, and 3 is Debug.
  Increase verbosity for more detailed output.
  |} in
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
  let convert_names_flag : bool Term.t =
    Arg.(value & flag & info [ "convert-names" ] ~doc:convert_names_doc)
  in
  let convert_comments_doc = {|
  Enables conversion of SML comments to OCaml comments.
  The process works by converting SML comments to attributes, which are then replaced with OCaml comments. 
  |}
  in
  let convert_comments_flag : bool Term.t =
    Arg.(value & flag & info [ "convert-comments" ] ~doc:convert_comments_doc)
  in
  let convert_force_flag : bool Term.t =
    let doc = {|
    Force overwrite of output directory if it already exists.
    |} in
    Arg.(value & flag & info [ "force" ] ~doc)
  in
  let+ convert_names = convert_names_flag
  and+ convert_comments = convert_comments_flag
  and+ force = convert_force_flag in
  (force, Common.mkConversions ~convert_names ~convert_comments ())

let concat_output : bool Term.t =
  let doc = {|
  Concatenate all output into a single file.
  This is enabled by default, due to differences between how SML and OCaml handle modules across multiple files.
  Use --concat-output=false to disable.
  |}
in
  Arg.(value & opt bool true & info [ "concat-output" ] ~doc)
let quiet : bool Term.t =
  let doc = {|
  Suppress all output except for errors.
  |} 
in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)
let infer_pattern : string option Term.t =
  let doc = {|
  A regular expression pattern to identify variable names that should be converted to lowercase in patterns.
  This is useful for converting SML variable names (which often start with uppercase letters) to OCaml conventions.
  The regex is applied to the last part of the variable name.
  |}
in
  Arg.(value & opt (some string) None & info [ "infer-pattern" ] ~doc)
let common_options : Common.options Cmdliner.Term.t =
  let+ v = verb and+ (force, c) = conversion_flags and+ co = concat_output and+ q = quiet and+ ip = infer_pattern in
  Common.mkOptions ~verbosity:(Some v) ~conversions:c ~concat_output:co ~force ~quiet:q ~guess_var:ip ()