open Common
include Process_file
open Astlib.Pprintast

type input = Common.source
type output = Common.target

class process ?(store = Context.create []) cfg_init =
  object (self)
    val mutable cfg : options = cfg_init
    val mutable store : Context.t = store
    val mutable successes : int = 0
    val mutable failures : int = 0
    val mutable total : int = 0
    method set_config c = cfg <- c
    method get_config () = cfg
    method get_store () = store
    method set_store s = store <- s
    method private write_output (content : string) : bool = 
      match get_output_file cfg with
      | FileOut path ->
          let oc = open_out path in
          output_string oc content;
          close_out oc;
          true
      | StdOut ->
          print_string content;
          true
      | Silent -> true
    method run (input : input) : int =
      try 
      match input with
      | File files ->
          total <- List.length files;
          self#run_files files
      | StdIn -> assert false
      with 
        e -> 
          Format.eprintf "Error: %s in %s@." (Printexc.to_string e) (match input with File files -> String.concat ", " files | _ -> "standard input");
          1

    method private run_files (files : string list) : int =
      let res = List.mapi (fun idx file ->
        Format.eprintf "Processing file %d of %d: %s@." (idx + 1) (List.length files) file;
        try 
          let ocaml_code = self#run_single_file file in
          let output_target = get_output_file cfg in
          (match output_target with
          | FileOut path ->
              let oc = open_out path in
              output_string oc ocaml_code;
              close_out oc
          | StdOut ->
              print_string ocaml_code
              | Silent -> ());
          successes <- successes + 1;
          ocaml_code
        with 
          e -> 
            Format.eprintf "Error processing file %s: %s@." file (Printexc.to_string e);
            failures <- failures + 1;
            ""
      ) files in 
    let _ = if Common.get_concat_output cfg then (let _ = self#write_output @@ String.concat "\n\n\n" res in 0) else (List.iter (fun code -> let _ = self#write_output code in ()) res; 0) in
    Format.eprintf "Processing complete: %d successes, %d failures out of %d files.@." successes failures total;
    if failures = 0 then 0 else 1

    method private run_single_file (file : string) : string =
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
          ocaml_code 
  end

