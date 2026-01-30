type 'a grammar = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a

exception FrontendError of string

let expression_grammar : Ast.expression grammar = Parser.expression_top
let pat_grammar : Ast.pat grammar = Parser.pat_top
let typ_grammar : Ast.typ grammar = Parser.typ_top
let main_grammar : Ast.prog grammar = fun f b -> fst @@ Parser.main f b
let context_lines = 1

let parse_with : grammar:'a grammar -> string -> 'a =
 fun ~grammar s ->
  let lexbuf = Lexing.from_string s in
  try
    let res = grammar Lexer.token lexbuf in
    res
  with
  | _ ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      let lines = String.split_on_char '\n' s in
      let start_line = max 0 (line - 1 - context_lines) in
      let end_line = min (List.length lines) (line + context_lines) in
      let txt =
        List.mapi
          (fun i s ->
            (if i + 1 == line then ">> " else "   ") ^ string_of_int (i + 1) ^ s)
          lines
        |> List.filteri (fun i _ -> i >= start_line && i < end_line)
        |> String.concat "\n"
      in

      let msg =
        Printf.sprintf "Parse error at line %d, character %d:" line cnum
        ^ "\n\n======\n\n" ^ txt ^ "\n"
      in
      raise (FrontendError msg)
  | _ -> failwith "Unknown parsing error"

let parse (s : string) : Ast.prog =
  let res = parse_with ~grammar:main_grammar s in
  res

let debug_parse fmt grammar s =
  let ast = parse_with ~grammar s in
  print_endline (fmt ast);
  ()
