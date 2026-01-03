
open Common 
open Astlib.Pprintast


class process_file cfg_init = 
  object (self) 
    val mutable cfg = cfg_init
    method set_config (c : config) = 
      cfg <- c
    method private get_fmt = match cfg.output_file with 
      | Some path -> 
          let oc = open_out path in
          Format.formatter_of_out_channel oc
      | None -> Format.std_formatter
    method run (x : unit) : int = try 
        let module Backend = Backend.Make(struct let config = cfg end) in
        let file_content : string = get_file cfg.input_file in
        let () = Printf.printf "Read file: %s\n" cfg.input_file in
        let parsed_sml = Frontend.parse file_content in 
        let () = Printf.printf "Parsed SML program\n" in
        let ocaml_code = Backend.process_sml ~prog:parsed_sml in
        let () = Printf.printf "Processed to OCaml code\n" in
        let () = List.iter (Pprintast.top_phrase (self#get_fmt)) ocaml_code in
        0
      with 
        e -> 
          Printf.eprintf "Error processing file: %s\n" cfg.input_file ;
          Printexc.print_backtrace stderr ;
          Printexc.to_string e |> Printf.eprintf "%s\n" ;
          1
        
  end ;;

  