open Common

type sml_code = Ast.prog
type ocaml_code = Parsetree.toplevel_phrase list

module Log = Common.Make (struct
  let config = Common.create []
  let group = "process_file"
end)

class process_file ?(store = Context.create (Context.Info.create [])) cfg_init =
  object (self)
    val mutable cfg = cfg_init
    val mutable store = store
    val mutable lexbuf : string = ""
    method get_store () : Context.t = store
    method set_store (s : Context.t) = store <- s
    method get_config () : t = cfg
    method set_config (c : t) = cfg <- c

    method private get_fmt =
      match Common.get Output_file cfg with
      | FileOut path ->
          let oc = open_out path in
          Stdlib.Format.formatter_of_out_channel oc
      | StdOut -> Stdlib.Format.std_formatter
      | Silent -> Stdlib.Format.std_formatter

    method parse_sml (s : string) : sml_code =
      lexbuf <- s;
      Frontend.parse s

    method convert_to_ocaml (sml : sml_code) : ocaml_code =
      Log.log_with ~cfg ~level:Low ~kind:Neutral
        ~msg:"Starting conversion from SML to OCaml..." ();
      let ctx = Context.create (Context.Info.create []) in
      Log.log_with ~cfg ~level:Debug ~kind:Neutral
        ~msg:"Building initial context..." ();
      let ctx0 = Context.merge ctx (self#get_store ()) in
      let ctx1 = Context.merge ctx0 Context.basis_context in
      (* TODO Make this a flag *)
      Log.log_with ~cfg ~level:Low ~kind:Neutral
        ~msg:"Converting SML to OCaml (backend phase)..." ();
      let module BackendContext = struct
        let lexbuf = lexbuf
        let context = ctx1
      end in
      let module BackendConfig = struct
        let config = self#get_config ()
      end in
      let module Backend = Backend.Make (BackendContext) (BackendConfig) in
      let raw_ocaml = Backend.process_sml ~prog:sml in
      (* Generate constructor manifest if output file is specified *)
      (match Common.get Output_file cfg with
      | FileOut output_path -> (
          let manifest_path = output_path ^ ".sctx" in
          let constructors = Backend.get_all_constructors () in
          try
            Context.Constructor_manifest.write_file manifest_path constructors;
            Log.log_with ~cfg ~level:Low ~kind:Neutral
              ~msg:
                (Printf.sprintf "Wrote constructor manifest to %s" manifest_path)
              ()
          with e ->
            Log.log_with ~cfg ~level:Low ~kind:Warning
              ~msg:
                (Printf.sprintf "Failed to write constructor manifest: %s"
                   (Printexc.to_string e))
              ())
      | _ -> ());
      Log.log_with ~cfg ~level:Low ~kind:Neutral
        ~msg:"Post-processing names (ocaml phase)..." ();
      let post_processor = new Ocaml.process_ocaml ~opts:(self#get_config ()) in
      post_processor#run_process raw_ocaml

    method print_ocaml (ocaml_code : ocaml_code) : string =
      let buffer = Buffer.create 256 in
      let fmt = Stdlib.Format.formatter_of_buffer buffer in
      List.iter (Common.Format.toplevel_phrase fmt) ocaml_code;
      Stdlib.Format.pp_print_flush fmt ();
      Buffer.contents buffer

    method process_file (input : string) : string =
      let sml_code = self#parse_sml input in
      let ocaml_code = self#convert_to_ocaml sml_code in
      let ocaml_output' = self#print_ocaml ocaml_code in
      let ocaml_output = Polish.polish ocaml_output' in
      let checked = Process_common.check_output ocaml_output in
      ocaml_output
  end
