type sml = Ast.prog
type ocaml = Parsetree.toplevel_phrase list

val read_to_sml : string -> sml
val sml_to_ppx : sml -> ocaml
val debug_sml : sml -> string
val ppx_to_ocaml : ocaml -> string
val sml_to_ocaml : string -> string