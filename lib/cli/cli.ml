open Cmdliner
open Cmdliner.Term.Syntax
include Process 
open Cmd_convert_file



let cmd_convert : int Cmd.t = let 
  doc : Cmd.info = Cmd.info "sml-ocaml-converter" ~doc:"Convert Standard ML code to OCaml"
in Cmd.group doc [cmd_convert_file]
let main () = Cmd.eval' cmd_convert
let entrypoint () = if !Sys.interactive then () else exit (main ())
