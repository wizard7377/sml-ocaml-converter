val get_context : Ast.prog -> Info.t

(** Extract the context from a given AST program. This involves traversing the
    AST and collecting all relevant name information into a Context.t structure.

    @param prog The AST program to extract context from
    @return The extracted context as a Context.t *)
