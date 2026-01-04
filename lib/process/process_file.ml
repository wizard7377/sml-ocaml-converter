
open Common 
open Astlib.Pprintast

type sml_code = Ast.prog
type ocaml_code = Parsetree.toplevel_phrase list
class process_file ?(store = Names.Store.create []) cfg_init = 
  object (self) 
    val mutable cfg = cfg_init
    val mutable store = store
    method get_store () : Names.Store.t = store
    method set_store (s : Names.Store.t) = 
      store <- s
    method get_config () : config = cfg
    method set_config (c : config) = 
      cfg <- c
    method private get_fmt = match cfg.output_file with 
      | Some path -> 
          let oc = open_out path in
          Format.formatter_of_out_channel oc
      | None -> Format.std_formatter
    method parse_sml (s : string) : sml_code = 
      Frontend.parse s
    method convert_to_ocaml (sml:sml_code) : ocaml_code = 
      let module Backend = Backend.Make(struct let config = self#get_config () end) in
      Backend.process_sml ~prog:sml
    method print_ocaml (ocaml_code:ocaml_code) : string = 
      let buffer = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buffer in
      List.iter (Astlib.Pprintast.top_phrase fmt) ocaml_code ;
      Format.pp_print_flush fmt () ;
      Buffer.contents buffer
    
        

    
  end ;;
