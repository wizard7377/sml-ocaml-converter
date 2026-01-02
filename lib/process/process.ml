
open Common 
open Astlib.Pprintast


class process_file cfg_init = 
  object (self) 
    val mutable cfg = cfg_init
    method set_config (c : config) = 
      cfg <- c

    method run (x : unit) : int = 
        let module Backend = Backend.Make(struct let config = cfg end) in
        let file_content : string = get_file cfg.input_file in
        let () = Printf.printf "Read file: %s\n" cfg.input_file in
        let parsed_sml = Frontend.parse file_content in 
        let () = Printf.printf "Parsed SML program\n" in
        let ocaml_code = Backend.process_sml ~prog:parsed_sml in
        let () = Printf.printf "Processed to OCaml code\n" in
        let output : Format.formatter = match cfg.output_file with 
          | Some path -> 
              let oc = open_out path in
              Format.formatter_of_out_channel oc
          | None -> Format.std_formatter
          in 
        let () = Pprintast.top_phrase output ocaml_code in
        0
        
  end ;;

  