
open Common 
open Astlib.Pprintast

type sml_code = Ast.prog
type ocaml_code = Parsetree.toplevel_phrase list

class process_file cfg_init = 
  object (self) 
    val mutable cfg = cfg_init
    method get_config : config = cfg
    method set_config (c : config) = 
      cfg <- c
    method get_fmt = match cfg.output_file with 
      | Some path -> 
          let oc = open_out path in
          Format.formatter_of_out_channel oc
      | None -> Format.std_formatter
    method parse_sml (s : string) : sml_code = 
      Frontend.parse s
    method convert_to_ocaml (sml:sml_code) : ocaml_code = 
      let module Backend = Backend.Make(struct let config = self#get_config end) in
      Backend.process_sml ~prog:sml
    method print_ocaml (ocaml_code:ocaml_code) : string = 
      let buffer = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buffer in
      List.iter (Astlib.Pprintast.top_phrase fmt) ocaml_code ;
      Format.pp_print_flush fmt () ;
      Buffer.contents buffer
    
        
  end ;;

  let run (self:process_file) : int = try 
        let module Backend = Backend.Make(struct let config = self#get_config end) in
        let file_content : string = get_file self#get_config.input_file in
        let () = Printf.printf "Read file: %s\n" self#get_config.input_file in
        let parsed_sml = self#parse_sml file_content in
        let () = Printf.printf "Parsed SML program\n" in
        let ocaml_code = self#convert_to_ocaml parsed_sml in
        let () = Printf.printf "Processed to OCaml code\n" in
        let () = List.iter (Pprintast.top_phrase (self#get_fmt)) ocaml_code in
        0
      with 
        e -> 
          Printf.eprintf "Error processing file: %s\n" self#get_config.input_file ;
          Printexc.print_backtrace stderr ;
          Printexc.to_string e |> Printf.eprintf "%s\n" ;
          raise e 