open Common
open Astlib.Pprintast

type sml_code = Ast.prog
type ocaml_code = Parsetree.toplevel_phrase list

module Log = Common.Make (struct
  let config = Common.mkOptions ()
  let group = "process_file"
end)

class process_file ?(store = Context.create []) cfg_init =
  object (self)
    val mutable cfg = cfg_init
    val mutable store = store
    val mutable lexbuf : string = ""
    method get_store () : Context.t = store
    method set_store (s : Context.t) = store <- s
    method get_config () : options = cfg
    method set_config (c : options) = cfg <- c

    method private get_fmt =
      match get_output_file cfg with
      | FileOut path ->
          let oc = open_out path in
          Format.formatter_of_out_channel oc
      | StdOut -> Format.std_formatter
      | Silent -> Format.std_formatter

    method parse_sml (s : string) : sml_code =
      lexbuf <- s;
      Frontend.parse s

    method convert_to_ocaml (sml : sml_code) : ocaml_code =
      Log.log_with ~cfg ~level:Low ~kind:Neutral
        ~msg:"Starting conversion from SML to OCaml..." ();
      let ctx = Context.create [] in
      Log.log_with ~cfg ~level:Debug ~kind:Neutral
        ~msg:"Building initial context..." ();
      let ctx0 = Context.merge ctx (self#get_store ()) in
      let ctx1 = Context.merge ctx0 Context.basis_context in
      (* TODO Make this a flag *)
      Log.log_with ~cfg ~level:Low ~kind:Neutral
        ~msg:"Converting SML to OCaml..." ();
      let module BackendContext = struct
        let lexbuf = lexbuf
        let context = ctx1
      end in
      let module BackendConfig = struct
        let config = self#get_config ()
      end in
      let module Backend = Backend.Make (BackendContext) (BackendConfig) in
      Backend.process_sml ~prog:sml

    method print_ocaml (ocaml_code : ocaml_code) : string =
      let buffer = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buffer in
      List.iter (Astlib.Pprintast.top_phrase fmt) ocaml_code;
      Format.pp_print_flush fmt ();
      Buffer.contents buffer

    method process_file (input : string) : string =
      let sml_code = self#parse_sml input in
      let ocaml_code = self#convert_to_ocaml sml_code in
      let ocaml_output' = self#print_ocaml ocaml_code in
      let ocaml_output = Polish.polish ocaml_output' in
      let checked = Process_common.check_output ocaml_output in
      ocaml_output
  end
