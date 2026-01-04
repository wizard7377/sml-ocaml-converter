open Cmdliner
open Cmdliner.Term.Syntax
include Cmd_common_options
include Process 

let convert_file ~(input_files:string list) ?(output_file:string option) ~(common_options:common_options) : int =  
  let input_files' = List.flatten (List.map (Re.split (Re.compile (Re.rep1 Re.space))) input_files) in
    let common_options = common_options in
    let cfg : Common.config = {
      input_file = String.concat " " input_files' ;
      output_file = output_file ;
      verbosity = common_options.verbose ;
      conversions = common_options.conversions 
    } in
    let process = new process cfg in
    let res = process#run (File input_files') in
    let _ = res in (); 0


    
let output : string option Term.t = 
  let doc = "Output path for converted files" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~doc)

let input : string list Term.t =
  let doc = "Input path (file or directory) to convert" in
  Arg.(non_empty (pos_all string [] & info [] ~doc))


let cmd_convert_file : int Cmd.t = 
  let doc = "Convert Standard ML code to OCaml" in 
  Cmd.v (Cmd.info "file" ~doc ~docs:"SML Converter") @@
  let+ output and+ input and+ common_options in
    convert_file 
      ~common_options:common_options
      ?output_file:output 
      ~input_files:input 
