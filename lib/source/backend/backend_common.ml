(** Shared types and utilities for backend converters *)

type env = {
  config : Common.t ref;
  labeller : Process_label.process_label;
  name_processor : Process_names.process_names;
  lexbuf : Lexing.lexbuf;
}

type state = {
  mutable temp_counter : int;
  mutable current_path : string list;
  mutable trace_depth : int;
}

let create_state () = { temp_counter = 0; current_path = []; trace_depth = 0 }

let get_temp state =
  let t = state.temp_counter in
  state.temp_counter <- t + 1;
  t

let fresh_var state prefix = Printf.sprintf "%s__%d" prefix (get_temp state)
