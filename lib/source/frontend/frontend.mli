type 'a grammar

exception FrontendError of string

val expression_grammar : Ast.expression grammar
val main_grammar : Ast.prog grammar
val pat_grammar : Ast.pat grammar
val typ_grammar : Ast.typ grammar
val debug_parse : ('a -> string) -> 'a grammar -> string -> unit
val parse_with : grammar:'a grammar -> string -> 'a
val parse : string -> Ast.prog
