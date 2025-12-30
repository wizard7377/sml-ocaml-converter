type sml = Ast.prog
type ocaml = Parsetree.toplevel_phrase list

let lexer (lexbuf : Lexing.lexbuf) : Frontend.Parser.token =
  let res = Frontend.Lexer.token lexbuf in
  res
let read_to_sml (input : string) : Ast.prog =
  let res = Frontend.Parser.program lexer (Lexing.from_string input) in
  let output = Ast.show_prog res in
  print_endline output ;
  res
let sml_to_ppx (prog : Ast.prog) = assert false
let ppx_to_ocaml (ppx : ocaml) = assert false

let debug_sml (prog : sml) = Ast.show_prog prog
let rec sml_to_ocaml (input : string) = let _ = read_to_sml input in input