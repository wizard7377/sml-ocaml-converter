(** AST traversal for extracting names from SML programs.

    This module provides functionality to traverse an SML {!Ast.prog} and
    extract all identifiers along with their syntactic contexts. The result
    is a list of {!Store.t} values that can be used by the backend to
    determine how names should be transformed during conversion.

    {2 Purpose}

    Before converting SML to OCaml, we need to understand what names exist
    and in what contexts. For example:
    - [SOME] as a constructor needs to become [Some] in OCaml
    - [myFun] as a value should become [my_fun] (if using snake_case)
    - [MyStruct] as a structure name is already valid in OCaml

    This module performs a complete traversal of the AST to collect all
    such names for later processing.

    {2 Design Note}

    Returns a list of stores rather than a single store to allow for
    scoped name handling (e.g., local bindings that shadow outer names).
*)

(** [get_names prog] extracts all names from an SML program AST.

    Traverses the entire program structure including:
    - Top-level declarations and bindings
    - Nested let expressions and local declarations
    - Pattern bindings (introducing new names)
    - Type declarations (type names and constructors)
    - Structure and signature declarations
    - Expression identifiers (references to existing names)

    @param prog The parsed SML program AST
    @return A list of name stores, one per scope level *)
val get_names : Ast.prog -> Store.t
