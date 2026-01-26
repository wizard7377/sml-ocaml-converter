(** Shared types and utilities for backend converters *)

(** Converter environment - immutable context *)
type env = {
  config : Common.options ref;
  labeller : Process_label.process_label;
  name_processor : Process_names.process_names;
  lexbuf : Lexing.lexbuf;
}

(** Converter state - mutable state *)
type state = {
  mutable temp_counter : int;
  mutable current_path : string list;
  mutable trace_depth : int;
}

(** Create initial state *)
val create_state : unit -> state

(** Get next temp variable number *)
val get_temp : state -> int

(** Generate fresh variable name with prefix *)
val fresh_var : state -> string -> string
