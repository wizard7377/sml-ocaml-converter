type level = High | Medium | Low | Debug
type kind = Positive | Negative | Neutral | Warning
type group = string

let () = Fmt_tty.setup_std_outputs ()

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

module Make (C : S) : LOG = struct
  let get_should_print ?(cfg : Options.options = C.config) (level : level) :
      bool =
    if Options.get_debug cfg |> List.mem C.group then true
    else
      let verbosity = Options.get_verbosity_default cfg 0 in
      let level_value =
        match level with High -> 0 | Medium -> 1 | Low -> 2 | Debug -> 3
      in
      verbosity >= level_value

  let log_with ~cfg ?(level = High) ?(kind = Negative) ~(msg : string)
      (() : unit) =
    let verbosity = Options.get_verbosity_default cfg 0 in
    let level_value =
      match level with High -> 0 | Medium -> 1 | Low -> 2 | Debug -> 3
    in
    let is_quiet = Options.get_quiet cfg == false || kind == Negative in
    if (get_should_print ~cfg level && is_quiet) || (List.mem C.group (Options.get_debug cfg)) then begin
      let prefix_msg =
        match kind with
        | Positive -> "SUCCESS "
        | Negative -> "ERROR "
        | Neutral -> "INFO "
        | Warning -> "WARNING "
      in
      let fmt_style =
        `Fg
          Fmt.(
            match kind with
            | Positive -> `Green
            | Negative -> `Red
            | Neutral -> `Blue
            | Warning -> `Yellow)
      in
      let format_bold = Fmt.(styled `Bold string) in
      Fmt.epr "%a" Fmt.(styled fmt_style format_bold) prefix_msg;
      Fmt.epr "%s@." msg
    end else ()

  let log ?(level = High) ?(kind = Negative) ~(msg : string) (() : unit) =
    log_with ~cfg:C.config ~level ~kind ~msg ()
end
