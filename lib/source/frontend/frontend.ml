include Parser 
include Lexer 
include Ast
let parse (s : string) : Ast.prog =
  let lexbuf = Lexing.from_string s in
  let res = Parser.file Lexer.token lexbuf in 
  fst res  (* TODO Make this better *)
