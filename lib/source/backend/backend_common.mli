(** Shared types and utilities for backend converters *)

type env = {
  config : Common.options ref;
  labeller : Process_label.process_label;
  name_processor : Process_names.process_names;
  lexbuf : Lexing.lexbuf;
}
(** Converter environment - immutable context *)

type state = {
  mutable temp_counter : int;
  mutable current_path : string list;
  mutable trace_depth : int;
}
(** Converter state - mutable state *)

val create_state : unit -> state
(** Create initial state *)

val get_temp : state -> int
(** Get next temp variable number *)

val fresh_var : state -> string -> string
(** Generate fresh variable name with prefix *)
