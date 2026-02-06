open Common

type sml_code
type ocaml_code

type t = {
  interface : string option;
  implementation : string option;
  exports : string option;
}

class process_group : string -> Common.t -> object
  method set_config : Common.t -> unit
  method get_config : unit -> Common.t
  method get_store : unit -> Context.t
  method set_store : Context.t -> unit
  method find_members : string -> t
  method set_members : t -> unit
  method parse_members : t -> string
end
