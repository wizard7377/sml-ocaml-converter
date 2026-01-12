include Parser
include Lexer
include Ast

let parse (s : string) : Ast.prog =
  let lexbuf = Lexing.from_string s in
  try
    let res = Parser.main Lexer.token lexbuf in
    fst res (* TODO Make this better *)
  with _ ->
    let pos = Lexing.lexeme_start_p lexbuf in
    let line = pos.Lexing.pos_lnum in
    let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    let msg = Printf.sprintf "Parse error at line %d, character %d" line cnum in
    failwith msg
