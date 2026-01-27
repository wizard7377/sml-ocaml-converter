type 'a grammar = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a

let expression_grammar : Ast.expression grammar = Parser.expression_top
let pat_grammar : Ast.pat grammar = Parser.pat_top
let typ_grammar : Ast.typ grammar = Parser.typ_top
let main_grammar : Ast.prog grammar = fun f b -> fst @@ Parser.main f b

let parse_with : grammar:'a grammar -> string -> 'a =
 fun ~grammar s ->
  let lexbuf = Lexing.from_string s in
  try
    let res = grammar Lexer.token lexbuf in
    res
  with _ ->
    let pos = Lexing.lexeme_start_p lexbuf in
    let line = pos.Lexing.pos_lnum in
    let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    let msg = Printf.sprintf "Parse error at line %d, character %d" line cnum in
    failwith msg

let parse (s : string) : Ast.prog =
  let res = parse_with ~grammar:main_grammar s in
  res

let debug_parse fmt grammar s =
  let ast = parse_with ~grammar s in
  print_endline (fmt ast);
  ()
