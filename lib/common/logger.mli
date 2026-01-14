type level = High | Medium | Low | Debug

type kind = Positive | Negative | Neutral | Warning
type group = string

val log : cfg:Options.options -> ?level:level -> ?kind:kind -> ?group:group -> msg:string -> unit -> unit

