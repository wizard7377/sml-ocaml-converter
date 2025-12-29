type sml = Ast.prog list
type ocaml = Parsetree.toplevel_phrase list


let read_to_sml (input : string) : Ast.prog list = let 
  res = Frontend.Parser.program Frontend.Lexer.token (Lexing.from_string input) in 
  let output = List.map (Ast.show_prog) res in  
  print_endline (String.concat "\n" output) ; 
  res
let sml_to_ppx (prog : Ast.prog list) = assert false
let sml_to_ppx_elem (prog : Ast.prog) = assert false
let ppx_to_ocaml (ppx : ocaml) = assert false

let debug_sml (prog : sml) = String.concat "\n" (List.map Ast.show_prog prog)
let rec sml_to_ocaml (input : string) = let _ = read_to_sml input in input