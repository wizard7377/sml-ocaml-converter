(** Backend constant processing - converts SML constants to OCaml constants. *)

(** Convert an SML constant to an OCaml constant. *)
val process_con : Ast.constant Ast.node -> Parsetree.constant
