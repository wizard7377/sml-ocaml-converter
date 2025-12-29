(** Token types shared between lexer and parser *)

(** Identifier type - either alphanumeric (Name) or symbolic (Symbol) *)
type ident = Name of string | Symbol of string
