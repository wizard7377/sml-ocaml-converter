type level = High | Medium | Low | Debug

type kind = Positive | Negative | Neutral | Warning


val log : cfg:Options.options -> ?level:level -> ?kind:kind -> msg:string -> unit

