(** Constant conversion - SML constants to OCaml Parsetree.constant *)

open Ast

module type CONVERTER_SIG = sig
  type env = Backend_common.env
  type state = Backend_common.state
  type input = constant node
  type output = Parsetree.constant

  val convert : env -> state -> input -> output
end

module Make (Env : sig
  type env = Backend_common.env
  type state = Backend_common.state
end) : CONVERTER_SIG = struct
  type env = Env.env
  type state = Env.state
  type input = constant node
  type output = Parsetree.constant

  open Ppxlib

  let convert _env _state const =
    match const.value with
    | ConInt i ->
        let i' = String.map (function '~' -> '-' | c -> c) i.value in
        Parsetree.Pconst_integer (i', None)
    | ConWord w ->
        let w' =
          if String.starts_with ~prefix:"0wx" w.value then
            "0x" ^ String.sub w.value 3 (String.length w.value - 3)
          else if String.starts_with ~prefix:"0w" w.value then
            String.sub w.value 2 (String.length w.value - 2)
          else w.value
        in
        Parsetree.Pconst_integer (w', None)
    | ConFloat r ->
        let r' = String.map (function '~' -> '-' | c -> c) r.value in
        Parsetree.Pconst_float (r', None)
    | ConChar c ->
        Parsetree.Pconst_char (String.get c.value 0)
    | ConString s ->
        Parsetree.Pconst_string (s.value, Location.none, None)
end
