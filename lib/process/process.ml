open Common
include Process_file
open Astlib.Pprintast

type input = Common.source
type output = Common.target

module Log = Common.Make (struct
  let config = Common.create []
  let group = "process"
end)

class process ?(store = Context.create (Context.Info.create [])) cfg_init =
  object (self)
    val mutable cfg : t = cfg_init
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

    method private get_output_path : Fpath.t option =
      match Common.get Output_file cfg with
      | FileOut path ->
          let fpath = Fpath.v path in
          let path' =
            if Common.get Dash_to_underscore cfg then
              Common.convert_path_dashes_to_underscores fpath
            else fpath
          in
          Some path'
      | _ -> None
    (** Convert output file path, applying dash-to-underscore transformation if
        enabled. Returns None if output mode is not FileOut. *)

    method private ensure_parent_dir (path : Fpath.t) : unit =
      let parent_dir = Fpath.parent path in
      let _ = Bos.OS.Dir.create ~path:true parent_dir in
      ()
    (** Ensure parent directory exists for a file path. *)

    method private write_output (content : string) : bool =
      match self#get_output_path with
      | Some path ->
          self#ensure_parent_dir path;
          let _ = Bos.OS.File.delete path in
          let oc = open_out (Fpath.to_string path) in
          output_string oc content;
          close_out oc;
          true
      | None ->
          print_string content;
          true
    (** Write content to the configured output target. Handles file path
        transformations (dash→underscore), directory creation, and different
        output modes (file, stdout, silent). *)

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
        Log.log ~level:High ~kind:Negative
          ~msg:
            (Printf.sprintf "%s in %s" (Printexc.to_string e)
               (match input with
               | File files -> String.concat ", " files
               | _ -> "standard input"))
          ();
        has_errors <- true;
        1

    method private append_to_output (content : string) : unit =
      match self#get_output_path with
      | Some path ->
          self#ensure_parent_dir path;
          let _ = Bos.OS.File.delete path in
          let existing = Bos.OS.File.read path in
          let new_content =
            match existing with
            | Error _ -> content
            | Ok prev -> prev ^ "\n\n" ^ content
          in
          Bos.OS.File.write path new_content |> ignore
      | None -> print_string content
    (** Append content to output file, or overwrite if file doesn't exist. *)

    method private write_error_file (source_file : string) : unit =
      match self#get_output_path with
      | Some base_path -> (
          let error_path = Fpath.(base_path + ".error") in
          self#ensure_parent_dir error_path;
          match Bos.OS.File.read (Fpath.v source_file) with
          | Ok content -> Bos.OS.File.write error_path content |> ignore
          | Error _ -> ())
      | None -> ()
    (** Write error file for failed conversions (contains original SML source).
    *)

    method private record_check_result (file : string)
        (checked : Process_common.check_result) : unit =
      match checked with
      | Process_common.Good -> successes <- successes + 1
      | Process_common.Bad err ->
          warning_store <-
            (file, Printexc.to_string (Syntaxerr.Error err)) :: warning_store;
          warnings <- warnings + 1
      | Process_common.Err e ->
          errors_store <- (file, Printexc.to_string e) :: errors_store;
          has_errors <- true;
          failures <- failures + 1
    (** Record check result in statistics. *)

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
              (* Record success/warning/failure *)
              self#record_check_result file checked;
              (* Write immediately if not concat mode *)
              if not (Common.get Concat_output cfg) then
                ignore (self#write_output ocaml_code);
              ocaml_code
            with e ->
              errors_store <- (file, Printexc.to_string e) :: errors_store;
              has_errors <- true;
              failures <- failures + 1;
              self#write_error_file file;
              raise e)
          files
      in
      (* In concat mode, write all results at once *)
      if Common.get Concat_output cfg then
        ignore (self#write_output @@ String.concat "\n\n\n" res);

      (* Log summary *)
      let summary_kind =
        if failures > 0 then Negative
        else if warnings > 0 then Warning
        else Positive
      in
      Log.log ~level:High ~kind:summary_kind
        ~msg:
          (Printf.sprintf
             "Processing complete: %d successes, %d warnings, %d failures out \
              of %d files."
             successes warnings failures total)
        ();
      Log.log ~level:Medium ~kind:Neutral
        ~msg:(Printf.sprintf "Processed files: %s" (String.concat ", " files))
        ();

      (* Return exit code: 0=success, 1=errors, 2=warnings *)
      if failures > 0 then 1 else if warnings > 0 then 2 else 0
    (** Process multiple files, collecting their outputs. *)

    method private log_verbose (msg : string) : unit =
      if Common.get Verbosity cfg > 2 then
        Log.log_with ~cfg ~level:Low ~kind:Neutral ~msg ()
    (** Log verbose message if verbosity level is high enough. *)

    method private run_single_file (file : string) :
        string * Process_common.check_result =
      Log.log ~level:Medium ~kind:Positive
        ~msg:(Printf.sprintf "Processing file: %s" file)
        ();

      (* Read source file *)
      let ic = open_in file in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;

      (* Create processor and run pipeline *)
      let process = new process_file cfg in
      let sml_ast = process#parse_sml content in
      self#log_verbose "Finished parsing SML AST.";

      let ocaml_ast = process#convert_to_ocaml sml_ast in
      self#log_verbose "Finished converting to OCaml AST.";

      let ocaml_code' = process#print_ocaml ocaml_ast in
      self#log_verbose "Finished printing OCaml code.";

      let ocaml_code = Polish.polish ocaml_code' in
      self#log_verbose "Finished polishing OCaml code.";

      (* Validate if requested *)
      let checked =
        if Common.get Check_ocaml cfg then
          Process_common.check_output ~config:cfg ocaml_code
        else Process_common.Good
      in
      (ocaml_code, checked)
    (** Process a single SML file through the complete pipeline.
        @param file Path to SML source file
        @return Tuple of (OCaml code, validation result) *)
  end
