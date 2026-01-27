(** Common backend context shared across all sub-modules.

    This module defines the types and signatures needed for the backend functor
    components to share state and dependencies. *)

open Process_names

(** Module type for the shared backend context *)
module type BACKEND_CONTEXT = sig
  val config : Common.options
  val lexbuf : string
  val labeller : Process_label.process_label
  val namer : Process_names.process_names

  val ghost : 'a -> 'a Location.loc
  (** Helper to create a located value with no source location *)

  val process_name_to_longident :
    ctx:Process_names.context -> string list -> Ppxlib.Longident.t
  (** Helper to get a Ppxlib.Longident.t from the name processor *)

  val process_name_to_string :
    ctx:Process_names.context -> string list -> string
  (** Helper to get a string from the name processor *)

  (** Logging function *)
  module Log : sig
    val log :
      level:Common.level -> kind:Common.kind -> msg:string -> unit -> unit
  end
end

exception BadAst of (Lexing.position * Lexing.position) option * string
(** Common exception for AST processing errors *)

(** Create a BadAst exception with an optional location *)
let mkBadAst ?loc (msg : string) : exn = BadAst (loc, msg)
