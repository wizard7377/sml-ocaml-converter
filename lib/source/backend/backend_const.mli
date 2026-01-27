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
end) : CONVERTER_SIG
