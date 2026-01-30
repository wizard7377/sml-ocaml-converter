open Common
include Process_file
open Astlib.Pprintast

type input = Common.source
type output = Common.target

module Log = Common.Make (struct
  let config = Common.mkOptions ()
  let group = "process"
end)

class process ?(store = Context.create []) cfg_init =
  object (self)
    val mutable cfg : options = cfg_init
    val mutable store : Context.t = store
    val mutable successes : int = 0
    val mutable failures : int = 0
    val mutable warnings : int = 0
    val mutable total : int = 0
    val mutable has_errors : bool = false
    val mutable errors_store : (string * string) list = []
    val mutable warning_store : (string * string) list = []
    method set_config c = cfg <- c
    method get_config () = cfg
    method get_store () = store
    method set_store s = store <- s

    method private write_output (content : string) : bool =
      match get_output_file cfg with
      | FileOut path ->
          let fpath = Fpath.v path in
          let path' =
            if Common.get_dash_to_underscore cfg then
              Common.convert_path_dashes_to_underscores fpath
            else fpath
          in
          (* Ensure parent directory exists *)
          let parent_dir = Fpath.parent path' in
          let _ = Bos.OS.Dir.create ~path:true parent_dir in
          let _ = Bos.OS.File.delete path' in
          let oc = open_out (Fpath.to_string path') in
          output_string oc content;
          close_out oc;
          true
      | StdOut ->
          print_string content;
          true
      | Silent ->
          print_string content;
          true

    method run (input : input) : int =
      try
        match input with
        | File files ->
            total <- List.length files;
            let res = self#run_files files in
            if failures > 0 then
              (List.iteri (fun idx (file, err) ->
                   Log.log ~level:High ~kind:Negative
                     ~msg:
                       (Printf.sprintf "#%d: %s in file %s" (idx + 1) err file)
                     ()))
                errors_store
            else ();
            res
        | StdIn -> assert false
      with e ->
        let err =
          begin match e with
          | Frontend.FrontendError msg -> msg
          | _ -> Printexc.to_string e
          end
        in
        Log.log ~level:High ~kind:Negative
          ~msg:
            (Printf.sprintf "%s in %s" err
               (match input with
               | File files -> String.concat ", " files
               | _ -> "standard input"))
          ();
        has_errors <- true;
        1

    method private run_files (files : string list) : int =
      let res =
        List.mapi
          (fun idx file ->
            Log.log ~level:Medium ~kind:Neutral
              ~msg:
                (Printf.sprintf "Processing file %d of %d: %s" (idx + 1)
                   (List.length files) file)
              ();
            try
              let ocaml_code, checked = self#run_single_file file in
              let output_target = get_output_file cfg in

              (match output_target with
              | FileOut path ->
                  let fpath = Fpath.v path in
                  let path' =
                    if Common.get_dash_to_underscore cfg then
                      Common.convert_path_dashes_to_underscores fpath
                    else fpath
                  in
                  (* Ensure parent directory exists *)
                  let parent_dir = Fpath.parent path' in
                  let _ = Bos.OS.Dir.create ~path:true parent_dir in
                  let _ = Bos.OS.File.delete path' in
                  let contents = Bos.OS.File.read path' in
                  let new_contents =
                    match contents with
                    | Error _ -> ocaml_code
                    | Ok existing_content ->
                        existing_content ^ "\n\n" ^ ocaml_code
                  in
                  Bos.OS.File.write path' new_contents |> ignore
              | StdOut -> print_string ocaml_code
              | Silent -> print_string ocaml_code);
              begin match checked with
              | Process_common.Good -> successes <- successes + 1
              | Process_common.Bad err ->
                  warning_store <-
                    (file, Printexc.to_string (Syntaxerr.Error err))
                    :: warning_store;
                  warnings <- warnings + 1
              | Process_common.Err e ->
                  errors_store <- (file, Printexc.to_string e) :: errors_store;
                  has_errors <- true;
                  failures <- failures + 1
              end;
              ocaml_code
            with e ->
              errors_store <- (file, Printexc.to_string e) :: errors_store;
              has_errors <- true;
              failures <- failures + 1;
              begin match get_output_file cfg with
              | FileOut out_path -> begin
                  let content = Bos.OS.File.read (Fpath.v file) in
                  let error_fpath = Fpath.v (out_path ^ ".error") in
                  let error_path' =
                    if Common.get_dash_to_underscore cfg then
                      Common.convert_path_dashes_to_underscores error_fpath
                    else error_fpath
                  in
                  (* Ensure parent directory exists *)
                  let parent_dir = Fpath.parent error_path' in
                  let _ = Bos.OS.Dir.create ~path:true parent_dir in
                  match content with
                  | Ok existing_content ->
                      Bos.OS.File.write error_path' existing_content |> ignore
                  | Error _ -> ()
                end
              | _ -> ()
              end;
              raise e)
          files
      in
      let _ =
        if Common.get_concat_output cfg then
          let _ = self#write_output @@ String.concat "\n\n\n" res in
          0
        else (
          List.iter
            (fun code ->
              let _ = self#write_output code in
              ())
            res;
          0)
      in
      Log.log ~level:High
        ~kind:
          (if failures == 0 then if warnings == 0 then Positive else Warning
           else Negative)
        ~msg:
          (Printf.sprintf
             "Processing complete: %d successes, %d warnings, %d failures out \
              of %d files."
             successes warnings failures total)
        ();
      Log.log ~level:Medium ~kind:Neutral
        ~msg:
          (Printf.sprintf "Processed these files: %s" (String.concat ", " files))
        ();
      if failures = 0 then if warnings = 0 then 0 else 2 else 1

    method private run_single_file (file : string) :
        string * Process_common.check_result =
      Log.log ~level:Medium ~kind:Positive
        ~msg:(Printf.sprintf "Processing file: %s" file)
        ();
      let ic = open_in file in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      let process = new process_file cfg in
      close_in ic;
      let sml_ast = process#parse_sml content in
      if Common.get_verbosity_default cfg 0 > 2 then
        Log.log_with ~cfg ~level:Low ~kind:Neutral
          ~msg:"Finished parsing SML AST." ();
      let ocaml_ast = process#convert_to_ocaml sml_ast in
      if Common.get_verbosity_default cfg 0 > 2 then
        Log.log_with ~cfg ~level:Low ~kind:Neutral
          ~msg:"Finished converting to OCaml AST." ();
      let ocaml_code' = process#print_ocaml ocaml_ast in
      if Common.get_verbosity_default cfg 0 > 2 then
        Log.log_with ~cfg ~level:Low ~kind:Neutral
          ~msg:"Finished printing OCaml code." ();
      let ocaml_code = Polish.polish ocaml_code' in
      if Common.get_verbosity_default cfg 0 > 2 then
        Log.log_with ~cfg ~level:Low ~kind:Neutral
          ~msg:"Finished polishing OCaml code." ();
      let checked =
        if Common.get_check_ocaml cfg then
          Process_common.check_output ~config:cfg ocaml_code
        else Process_common.Good
      in
      let _ = checked in
      (ocaml_code, checked)
  end
