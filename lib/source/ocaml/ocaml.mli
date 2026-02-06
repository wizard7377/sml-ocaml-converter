type context

val default_context : context

class process_ocaml : opts:Common.t -> object
  inherit [context] Ppxlib.Ast_traverse.map_with_context

  method run_process :
    Parsetree.toplevel_phrase list -> Parsetree.toplevel_phrase list
end
