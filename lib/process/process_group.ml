open Common

type sml_code
type ocaml_code

type t = {
  interface : string option;
  implementation : string option;
  exports : string option;
}

class process_group path cfg_init =
  object (self)
    val mutable cfg = cfg_init
    val mutable store : Context.t = Context.create []
    val mutable path : string = path

    val mutable members : t =
      { interface = None; implementation = None; exports = None }

    method get_store () : Context.t = store
    method set_store (s : Context.t) = store <- s
    method get_config () : options = cfg
    method set_config (c : options) = cfg <- c

    method find_members (group_file : string) : t =
      {
        interface =
          (if Sys.file_exists (group_file ^ ".sig") then
             Some (group_file ^ ".sig")
           else None);
        implementation =
          (if Sys.file_exists (group_file ^ ".fun") then
             Some (group_file ^ ".fun")
           else None);
        exports =
          (if Sys.file_exists (group_file ^ ".sml") then
             Some (group_file ^ ".sml")
           else None);
      }

    method set_members (members' : t) =
      members <- members';
      ()

    method parse_members (members : t) : string =
      try
        let p0, p1, p2 =
          ( new Process_file.process_file cfg_init,
            new Process_file.process_file cfg_init,
            new Process_file.process_file cfg_init )
        in
        let interface_output =
          match members.interface with
          | Some path ->
              p0#process_file (Result.get_ok (Bos.OS.File.read @@ Fpath.v path))
          | None -> ""
        in
        let implementation_output =
          match members.implementation with
          | Some path ->
              p1#process_file (Result.get_ok (Bos.OS.File.read @@ Fpath.v path))
          | None -> ""
        in
        let exports_output =
          match members.exports with
          | Some path ->
              p2#process_file (Result.get_ok (Bos.OS.File.read @@ Fpath.v path))
          | None -> ""
        in
        interface_output ^ "\n" ^ implementation_output ^ "\n" ^ exports_output
      with e ->
        Stdlib.Format.eprintf "Error: %s in processing group members.@."
          (Printexc.to_string e);
        raise e
  end
