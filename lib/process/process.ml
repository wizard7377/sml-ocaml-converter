open Common
include Process_file
open Astlib.Pprintast

type input = Common.source
type output = Common.target

class process ?(store = Context.create []) cfg_init =
  object (self)
    val mutable cfg : options = cfg_init
    val mutable store : Context.t = store
    method set_config c = cfg <- c
    method get_config () = cfg
    method get_store () = store
    method set_store s = store <- s

    method run (input : input) : int =
      match input with
      | File files ->
          assert (List.length files > 0);
          self#run_files files
      | _ -> assert false

    method private run_files (files : string list) : int =
      List.iter
        (fun file ->
          Format.eprintf "Processing file: %s@." file;
          let ic = open_in file in
          let len = in_channel_length ic in
          let content = really_input_string ic len in
          let process = new process_file cfg in
          close_in ic;
          let sml_ast = process#parse_sml content in
          let ocaml_ast = process#convert_to_ocaml sml_ast in
          let ocaml_code' = process#print_ocaml ocaml_ast in
          let ocaml_code = Polish.polish ocaml_code' in
          match get_output_file cfg with
          | FileOut path ->
              let oc = open_out path in
              output_string oc ocaml_code;
              close_out oc
          | StdOut -> print_endline ocaml_code)
        files;
      0
  end
