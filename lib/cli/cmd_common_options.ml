open Cmdliner
open Cmdliner.Term.Syntax
open Cmdliner.Term

let verb : int Term.t =
  let doc = "Verbosity level" in
  Arg.(value & opt int 0 & info [ "v"; "verbose" ] ~doc)

let conversion_flags : Common.conversions Term.t =
  let convert_names_doc =
    "Enable conversion of names from SML to OCaml style"
  in
  let convert_names_flag : bool Term.t =
    Arg.(value & flag & info [ "convert-names" ] ~doc:convert_names_doc)
  in
  let convert_comments_doc =
    "Enable conversion of comments from SML to OCaml style"
  in
  let convert_comments_flag : bool Term.t =
    Arg.(value & flag & info [ "convert-comments" ] ~doc:convert_comments_doc)
  in
  let+ convert_names = convert_names_flag
  and+ convert_comments = convert_comments_flag in
  Common.mkConversions ~convert_names ~convert_comments ()

let common_options : Common.options Cmdliner.Term.t =
  let+ v = verb and+ c = conversion_flags in
  Common.mkOptions ~verbosity:(Some v) ~conversions:c ()
