(** Backend constant processing - converts SML constants to OCaml constants. *)

val process_con : Ast.constant Ast.node -> Parsetree.constant
(** Convert an SML constant to an OCaml constant. *)
