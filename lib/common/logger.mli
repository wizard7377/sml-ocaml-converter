type level = High | Medium | Low | Debug
type kind = Positive | Negative | Neutral | Warning
type group = string

module type LOG = sig
  val log :
    ?subgroup:string -> ?level:level -> ?kind:kind -> msg:string -> unit -> unit

  val log_with :
    cfg:Config_lib.t ->
    ?subgroup:string ->
    ?level:level ->
    ?kind:kind ->
    msg:string ->
    unit ->
    unit
end

module type S = sig
  val config : Config_lib.t
  val group : string
end

module Make (C : S) : LOG
