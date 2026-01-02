open Cmdliner
open Cmdliner.Term.Syntax
include Cmd_common_options
include Process 
let convert_file ~(input_file:string) ?(output_file:string option) ~(common_options:common_options) : int =  
  let common_options = common_options in
  let cfg : Common.config = {
    input_file = input_file ;
    output_file = output_file ;
    verbosity = common_options.verbose ;
    conversions = common_options.conversions 
  } in
  let process = new process_file cfg in
  let res = process#run () in 
  res 



let output : string option Term.t = 
  let doc = "Output path for converted files" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~doc)

let input : string Term.t =
  let doc = "Input path (file or directory) to convert" in
  Arg.(required & pos 0 (some file) None & info [] ~doc)


let cmd_convert_file : int Cmd.t = 
  let doc = "Convert Standard ML code to OCaml" in 
  Cmd.v (Cmd.info "file" ~doc ~docs:"SML Converter") @@
  let+ output and+ input and+ common_options in
    convert_file 
      ~common_options:common_options
      ?output_file:output 
      ~input_file:input 
