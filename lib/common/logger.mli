type level = High | Medium | Low | Debug
type kind = Positive | Negative | Neutral | Warning
type group = string

module type LOG = sig
  val log : ?level:level -> ?kind:kind -> msg:string -> unit -> unit

  val log_with :
    cfg:Options.options ->
    ?level:level ->
    ?kind:kind ->
    msg:string ->
    unit ->
    unit
end

module type S = sig
  include Options.CONFIG

  val group : string
end

module Make (C : S) : LOG
