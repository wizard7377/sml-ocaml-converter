(** Backend constant processing - converts SML constants to OCaml constants.
    
    SML and OCaml have different constant syntaxes:
    - SML uses [~] for negation, OCaml uses [-]
    - SML has word literals ([0w42]), OCaml doesn't (need conversion)
    - SML character literals use [#"c"], OCaml uses ['c'] *)

include Helpers

(** Convert an SML constant to an OCaml constant.

    Handles:
    - Integer constants (decimal and hexadecimal)
    - Word constants (unsigned integers, SML-specific)
    - Floating-point constants
    - Character constants ([#"a"] → ['a'])
    - String constants *)
let process_con (constant : Ast.constant Ast.node) : Parsetree.constant =
  match constant.value with
  | ConInt i ->
      (* SML uses ~ for negation, OCaml uses - *)
      let i' = String.map (function '~' -> '-' | c -> c) i.value in
      Pconst_integer (i', None)
  | ConWord w ->
      (* Words are unsigned integers in SML, not directly supported in OCaml *)
      (* Convert to regular integer, stripping 0w or 0wx prefix *)
      let w' =
        if String.starts_with ~prefix:"0wx" w.value then
          "0x" ^ String.sub w.value 3 (String.length w.value - 3)
        else if String.starts_with ~prefix:"0w" w.value then
          String.sub w.value 2 (String.length w.value - 2)
        else w.value
      in
      Pconst_integer (w', None)
  | ConFloat r ->
      (* SML uses ~ for negation, OCaml uses - *)
      let r' = String.map (function '~' -> '-' | c -> c) r.value in
      Pconst_float (r', None)
  | ConChar c ->
      (* SML: #"a", OCaml: 'a' - the string should already be the character *)
      Pconst_char (String.get c.value 0)
  | ConString s -> Pconst_string (s.value, Location.none, None)
