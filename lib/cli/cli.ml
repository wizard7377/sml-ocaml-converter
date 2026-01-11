open Cmdliner
open Cmdliner.Term.Syntax
include Process
open Cmd_convert_file
include Test_cmd

let cmd_convert : int Cmd.t =
  let doc : Cmd.info =
    Cmd.info "shibboleth" ~doc:{|
    A Standard ML to OCaml Converter.

    This tool provides functionality to convert Standard ML source files into OCaml source files.
    This is intended to work for any valid Standard ML (see The Definition of Standard ML, 1997), but was tested primarly against the Twelf project.
    |}
  in
  Cmd.group doc [ cmd_convert_file; cmd_convert_group ]

let main () = Cmd.eval' cmd_convert
let entrypoint () = if !Sys.interactive then () else exit (main ())
