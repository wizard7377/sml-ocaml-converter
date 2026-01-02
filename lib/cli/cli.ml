open Cmdliner
open Cmdliner.Term.Syntax
include Process 


let convert_file ~(input_file:string) ?(output_file:string option) ?(verb:int option) ~(guess:bool) ~(curry:bool) ~(no_comments:bool) : int =  
  let cfg : Common.config = {
    input_file = input_file ;
    output_file = output_file ;
    verbosity = verb ;
  } in
  let process = new process_file cfg in
  process#run ()

let output : string option Term.t = 
  let doc = "Output path for converted files" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~doc)

let input : string Term.t =
  let doc = "Input path (file or directory) to convert" in
  Arg.(required & pos 0 (some file) None & info [] ~doc)
let verb : int Term.t =
  let doc = "Verbosity level" in
  Arg.(value & opt int 0 & info ["v"; "verbose"] ~doc)
let guess_names : bool Term.t = 
  let doc = "Try and guess capitilization in patterns" in 
  Arg.(value & flag & info ["g"; "guess"] ~doc)
let try_curry : bool Term.t = 
  let doc = "Try and convert functions to curried form where possible" in 
  Arg.(value & flag & info ["c"; "curry"] ~doc)

let no_comments : bool Term.t = 
  let doc = "Do not include comments in the output OCaml code" in 
  Arg.(value & flag & info ["n"; "no-comments"] ~doc)
let cmd_convert_file : int Cmd.t = 
  let doc = "Convert Standard ML code to OCaml" in 
  Cmd.v (Cmd.info "file" ~doc ~docs:"SML Converter") @@
  let+ verb and+ output and+ input and+ guess_names and+ try_curry and+ no_comments in
    convert_file 
      ~verb:verb 
      ?output_file:output 
      ~input_file:input 
      ~guess:guess_names 
      ~curry:try_curry 
      ~no_comments:no_comments 

let cmd_convert : int Cmd.t = let 
  doc : Cmd.info = Cmd.info "sml-ocaml-converter" ~doc:"Convert Standard ML code to OCaml"
in Cmd.group doc [cmd_convert_file]
let main () = Cmd.eval' cmd_convert
let entrypoint () = if !Sys.interactive then () else exit (main ())