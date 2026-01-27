(** Utilities for processing SML type variables.

    Type variables in SML can be:
    - 'a (regular type variable)
    - ''a (equality type variable)

    This module provides functions to strip prefixes and convert to OCaml. *)

include Helpers

(** Strip the leading quote(s) from a type variable name.

    Examples:
    - "'a" -> "a"
    - "''a" -> "a"
    - "a" -> "a" (already stripped) *)
let strip_type_var_prefix (s : string) : string =
  if String.starts_with ~prefix:"''" s then String.sub s 2 (String.length s - 2)
  else if String.starts_with ~prefix:"'" s then
    String.sub s 1 (String.length s - 1)
  else s

(** Process a type variable name and return an OCaml type variable.

    Converts SML type variables ('a, ''a) to OCaml Parsetree type variables. The
    leading quotes are stripped since OCaml's ptyp_var expects just the variable
    name without the quote. *)
let process_type_var_name (s : string) : Parsetree.core_type =
  let stripped = strip_type_var_prefix s in
  Builder.ptyp_var stripped

(** Convert a list of SML type variable nodes to OCaml type parameters.

    Returns pairs of (core_type, variance/injectivity) suitable for use in type
    declarations. *)
let process_type_params (tvars : Ast.idx Ast.node list) :
    (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list =
  List.map
    (fun (tv : Ast.idx Ast.node) ->
      let tv_str = Idx_utils.idx_to_string tv.value in
      let var_name = strip_type_var_prefix tv_str in
      (Builder.ptyp_var var_name, (Asttypes.NoVariance, Asttypes.NoInjectivity)))
    tvars
