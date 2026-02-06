type level = High | Medium | Low | Debug
type kind = Positive | Negative | Neutral | Warning
type group = string

let () = Fmt_tty.setup_std_outputs ()

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

module Make (C : S) : LOG = struct
  (* Helper function to match debug flags against logger group/subgroup *)
  let matches_debug_flag ~(group : string) ~(subgroup : string) (flag : string)
      : bool =
    let full_name = if subgroup = "" then group else group ^ ":" ^ subgroup in
    String.equal flag group || String.equal flag full_name

  (* Determine if a message should be printed based on filtering rules.
     Priority hierarchy:
     1. Debug flags bypass everything (always print if matched)
     2. Quiet mode suppresses all except errors (kind=Negative)
     3. Verbosity level filtering (based on message importance) *)
  let get_should_print ?(subgroup = "") ?(cfg : Config_lib.t = C.config)
      (level : level) (kind : kind) : bool =
    let debug_flags = Config_lib.get Debug cfg in

    (* Priority 1: Debug flags bypass everything *)
    if List.exists (matches_debug_flag ~group:C.group ~subgroup) debug_flags
    then true (* Priority 2: Quiet mode suppresses non-errors *)
    else if Config_lib.get Quiet cfg && kind <> Negative then false
    (* Priority 3: Verbosity level filtering *)
      else
      let verbosity = Config_lib.get Verbosity cfg in
      let level_value =
        match level with High -> 0 | Medium -> 1 | Low -> 2 | Debug -> 3
      in
      verbosity >= level_value

  let log_with ~cfg ?(subgroup = "") ?(level = High) ?(kind = Negative)
      ~(msg : string) (() : unit) =
    if get_should_print ~cfg ~subgroup level kind then begin
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
    end

  let log ?(subgroup = "") ?(level = High) ?(kind = Negative) ~(msg : string)
      (() : unit) =
    log_with ~cfg:C.config ~subgroup ~level ~kind ~msg ()
end
