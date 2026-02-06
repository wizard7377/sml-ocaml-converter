(** Multi-file group processing for SML module systems. *)

open Common

type sml_code
type ocaml_code

(** Group member files: .sig (interface), .fun (functor), .sml (structure). *)
type t = {
  interface : string option;
  implementation : string option;
  exports : string option;
}

(** Group processor for related SML files (.sig/.fun/.sml). *)
class process_group path cfg_init =
  object (_self)
    val mutable cfg = cfg_init
    val mutable store : Context.t = Context.create (Context.Info.create [])
    val mutable path : string = path
    val mutable members : t = { interface = None; implementation = None; exports = None }

    method get_store () : Context.t = store
    method set_store (s : Context.t) = store <- s
    method get_config () : Common.t = cfg
    method set_config (c : Common.t) = cfg <- c

    method find_members (group_file : string) : t =
      (** Discover related files by checking for .sig, .fun, .sml extensions. *)
      {
        interface = if Sys.file_exists (group_file ^ ".sig") then Some (group_file ^ ".sig") else None;
        implementation = if Sys.file_exists (group_file ^ ".fun") then Some (group_file ^ ".fun") else None;
        exports = if Sys.file_exists (group_file ^ ".sml") then Some (group_file ^ ".sml") else None;
      }

    method set_members (members' : t) = members <- members'

    (** Helper to process a single member file. *)
    method private process_member (file_path : string option) (processor : Process_file.process_file) : string =
      match file_path with
      | Some path ->
          let content = Result.get_ok (Bos.OS.File.read @@ Fpath.v path) in
          processor#process_file content
      | None -> ""

    method parse_members (members : t) : string =
      (** Process all member files and concatenate in order: interface → implementation → exports. *)
      try
        (* Create separate processors for each file to maintain independent state *)
        let p0 = new Process_file.process_file cfg_init in
        let p1 = new Process_file.process_file cfg_init in
        let p2 = new Process_file.process_file cfg_init in

        (* Process each member in order *)
        let interface_output = _self#process_member members.interface p0 in
        let implementation_output = _self#process_member members.implementation p1 in
        let exports_output = _self#process_member members.exports p2 in

        (* Concatenate with newline separators *)
        String.concat "\n" [interface_output; implementation_output; exports_output]
      with e ->
        Stdlib.Format.eprintf "Error: %s in processing group members.@." (Printexc.to_string e);
        raise e
  end

