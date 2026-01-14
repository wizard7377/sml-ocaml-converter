type level = High | Medium | Low | Debug

type kind = Positive | Negative | Neutral | Warning
type group = string
let () = Fmt_tty.setup_std_outputs ()
let log ~(cfg:Options.options) ?(level=High) ?(kind=Negative) ?(group="default") ~(msg:string) (() : unit) =
  let verbosity = Options.get_verbosity_default cfg 0 in
  let level_value = match level with
    | High -> 0
    | Medium -> 1
    | Low -> 2
    | Debug -> 3
  in
  let is_quiet = (Options.get_quiet cfg == false) || (kind == Negative) in
  if (verbosity >= level_value) && is_quiet then
    let prefix_msg = match kind with
      | Positive -> "SUCCESS "
      | Negative -> "ERROR "
      | Neutral -> "INFO "
      | Warning -> "WARNING "
  in let fmt_style = `Fg (Fmt.(match kind with
      | Positive -> `Green 
      | Negative -> `Red
      | Neutral -> `Blue
      | Warning -> `Yellow))
in let 
  format_bold = Fmt.(styled `Bold string)
in 
  Fmt.epr "%a" Fmt.(styled fmt_style format_bold) prefix_msg ;
  Fmt.epr "%s@." msg
  else
    ()
