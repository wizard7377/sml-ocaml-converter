(** Shibboleth: SML to OCaml converter library.

    {1 Synopsis}

    Shibboleth is a source-to-source compiler library that converts Standard ML (SML)
    programs to OCaml. It provides the complete conversion pipeline from parsing SML
    source code to generating equivalent OCaml code.

    {1 Overview}

    This library implements a classic multi-phase compiler architecture:

    {v
    SML source → Lexer → Parser → SML AST → Backend → OCaml Parsetree → OCaml source
    v}

    The conversion process:
    - Parses SML source using ocamllex and Menhir
    - Produces a complete SML abstract syntax tree ({!Ast.prog})
    - Transforms the AST to OCaml's Parsetree representation
    - Pretty-prints the result as OCaml source code

    {1 Type Aliases}

    This module exposes the primary data types used throughout the conversion pipeline. *)

(** Type alias for SML abstract syntax trees.

    Represents a complete parsed SML program. See {!Ast} module for the
    full AST definition covering expressions, patterns, types, declarations,
    structures, signatures, and functors. *)
type sml = Ast.prog

(** Type alias for OCaml abstract syntax trees.

    Represents OCaml code as a list of top-level phrases (declarations,
    expressions, directives). Uses the compiler-libs Parsetree representation
    which can be pretty-printed to generate OCaml source code.

    Note: This is a list because a single SML program may generate multiple
    OCaml top-level declarations. *)
type ocaml = Parsetree.toplevel_phrase list
