{
  open Parser
  open Tokens

  exception Lexer_error of string

  let keywords = Hashtbl.create 50
  let () = List.iter (fun (kw, tok) -> Hashtbl.add keywords kw tok) [
    ("abstype", ABSTYPE);
    ("and", AND);
    ("andalso", ANDALSO);
    ("as", AS);
    ("case", CASE);
    ("datatype", DATATYPE);
    ("do", DO);
    ("else", ELSE);
    ("end", END);
    ("eqtype", EQTYPE);
    ("exception", EXCEPTION);
    ("fn", FN);
    ("fun", FUN);
    ("functor", FUNCTOR);
    ("handle", HANDLE);
    ("if", IF);
    ("in", IN);
    ("include", INCLUDE);
    ("infix", INFIX);
    ("infixr", INFIXR);
    ("let", LET);
    ("local", LOCAL);
    ("nonfix", NONFIX);
    ("of", OF);
    ("op", OP);
    ("open", OPEN);
    ("orelse", ORELSE);
    ("raise", RAISE);
    ("rec", REC);
    ("sharing", SHARING);
    ("sig", SIG);
    ("signature", SIGNATURE);
    ("struct", STRUCT);
    ("structure", STRUCTURE);
    ("then", THEN);
    ("type", TYPE);
    ("val", VAL);
    ("where", WHERE);
    ("with", WITH);
    ("withtype", WITHTYPE);
    ("while", WHILE);
  ]

  let lookup_keyword s =
    try Hashtbl.find keywords s
    with Not_found -> SHORT_IDENT (Name s)

  (* Buffer for string literals *)
  let string_buf = Buffer.create 256

  (* Track comment nesting depth *)
  let comment_depth = ref 0
  let comment_buf = Buffer.create 256
}

(* Character classes *)
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'-'z' 'A'-'Z']
let alphanumeric = letter | digit | ['\''] | ['_']
let symbolic = ['!' '%' '&' '$' '#' '+' '-' '/' ':' '<' '=' '>' '?' '@' '\\' '~' '`' '^' '|' '*']

(* Whitespace *)
let whitespace = [' ' '\t' '\r']
let newline = '\n'

(* Numbers *)
let num = digit+
let hexnum = hexdigit+

(* Identifiers *)
let alphanum_id = letter alphanumeric*
let symbolic_id = symbolic+

(* Type variables *)
let tyvar = '\'' alphanumeric*
let eqtyvar = '\'' '\'' alphanumeric*

rule token = parse
  (* Whitespace - skip *)
  | whitespace+  { token lexbuf }
  | newline      { Lexing.new_line lexbuf; token lexbuf }

  (* Comments *)
  | "(*"         { comment_depth := 1;
                   Buffer.clear comment_buf;
                   Buffer.add_string comment_buf "(*";
                   comment lexbuf }

  (* String literals *)
  | '"'          { Buffer.clear string_buf;
                   string lexbuf }

  (* Character literals: #"c" *)
  | '#' '"'      { Buffer.clear string_buf;
                   char_lit lexbuf }

  (* Hexadecimal integers with optional negation: ~0xhex or 0xhex *)
  | '~' "0x" hexnum as s { HEX_LIT s }
  | "0x" hexnum as s     { HEX_LIT s }

  (* Word literals: 0w for decimal, 0wx for hex *)
  | "0wx" hexnum as s    { HEX_LIT s }
  | "0w" num as s        { INT_LIT s }

  (* Floating point literals with optional negation *)
  | '~' num '.' num 'e' '~' num as s  { FLOAT_LIT s }
  | '~' num '.' num 'e' num as s      { FLOAT_LIT s }
  | '~' num 'e' '~' num as s          { FLOAT_LIT s }
  | '~' num 'e' num as s              { FLOAT_LIT s }
  | '~' num '.' num as s              { FLOAT_LIT s }
  | num '.' num 'e' '~' num as s      { FLOAT_LIT s }
  | num '.' num 'e' num as s          { FLOAT_LIT s }
  | num 'e' '~' num as s              { FLOAT_LIT s }
  | num 'e' num as s                  { FLOAT_LIT s }
  | num '.' num as s                  { FLOAT_LIT s }

  (* Decimal integers with optional negation: ~num or num *)
  | '~' num as s  { INT_LIT s }
  | num as s      { INT_LIT s }

  (* Punctuation and operators *)
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '{'           { LBRACE }
  | '}'           { RBRACE }
  | '['           { LBRACKET }
  | ']'           { RBRACKET }
  | ','           { COMMA }
  | ';'           { SEMICOLON }
  | ":>"          { COLON_GT }
  | ':'           { COLON }
  | '='           { EQUAL }
  | '|'           { BAR }
  | "..."         { ELLIPSIS }
  | "=>"          { BIGARROW }
  | "->"          { ARROW }
  | '_'           { UNDERSCORE }
  | '#'           { HASH }

  (* Type variables: 'a, ''a (equality type variables) *)
  | eqtyvar as s  { TYVAR s }
  | tyvar as s    { TYVAR s }

  (* Long identifiers: id1.id2...idn
     We need to distinguish between long and short identifiers *)
  | alphanum_id ('.' alphanum_id)+
    {
      (* Parse the full long identifier *)
      let full = Lexing.lexeme lexbuf in
      let parts = String.split_on_char '.' full in
      LONG_IDENT (List.map (fun s -> Name s) parts)
    }

  (* Alphanumeric identifiers - check for keywords *)
  | alphanum_id as s { lookup_keyword s }

  (* Star needs to be separate for tuple types *)
  | '*'         { STAR }

  (* Symbolic identifiers - these can never be keywords *)
  | symbolic_id as s { SHORT_IDENT (Symbol s) }

  (* End of file *)
  | eof           { EOF }

  (* Error case *)
  | _ as c        { raise (Lexer_error (Printf.sprintf "Unexpected character: '%c'" c)) }

(* Nested comment lexer *)
and comment = parse
  | "(*"          { incr comment_depth;
                    Buffer.add_string comment_buf "(*";
                    comment lexbuf }
  | "*)"          { decr comment_depth;
                    Buffer.add_string comment_buf "*)";
                    if !comment_depth = 0 then
                      COMMENT (Buffer.contents comment_buf)
                    else
                      comment lexbuf }
  | newline       { Lexing.new_line lexbuf;
                    Buffer.add_char comment_buf '\n';
                    comment lexbuf }
  | _ as c        { Buffer.add_char comment_buf c;
                    comment lexbuf }
  | eof           { raise (Lexer_error "Unterminated comment") }

(* String literal lexer *)
and string = parse
  | '"'           { STRING_LIT (Buffer.contents string_buf) }
  | "\\n"         { Buffer.add_char string_buf '\n'; string lexbuf }
  | "\\t"         { Buffer.add_char string_buf '\t'; string lexbuf }
  | "\\r"         { Buffer.add_char string_buf '\r'; string lexbuf }
  | "\\\\"        { Buffer.add_char string_buf '\\'; string lexbuf }
  | "\\\""        { Buffer.add_char string_buf '"'; string lexbuf }
  (* Escape sequence: \ddd (3 decimal digits) *)
  | '\\' (digit digit digit as d)
    { let code = int_of_string d in
      if code > 255 then
        raise (Lexer_error (Printf.sprintf "Invalid escape sequence: \\%s" d))
      else begin
        Buffer.add_char string_buf (Char.chr code);
        string lexbuf
      end }
  (* Escape sequence: \uxxxx (4 hex digits) - SML/NJ extension *)
  | "\\u" (hexdigit hexdigit hexdigit hexdigit as h)
    { let code = int_of_string ("0x" ^ h) in
      (* For simplicity, only handle codes <= 255 *)
      if code > 255 then
        raise (Lexer_error "Unicode escape sequences above \\u00FF not supported")
      else begin
        Buffer.add_char string_buf (Char.chr code);
        string lexbuf
      end }
  (* String gap: \ followed by whitespace and newlines, ending with \ *)
  | '\\' whitespace* newline whitespace* '\\'
    { Lexing.new_line lexbuf;
      string lexbuf }
  (* Caret notation: \^c where c is @-_ (ASCII 64-95) *)
  | "\\^" (['@'-'_'] as c)
    { Buffer.add_char string_buf (Char.chr (Char.code c - 64));
      string lexbuf }
  | newline       { raise (Lexer_error "Newline in string literal (use \\n or string gap)") }
  | _ as c        { Buffer.add_char string_buf c; string lexbuf }
  | eof           { raise (Lexer_error "Unterminated string literal") }

(* Character literal lexer: expects content after #" and ending with " *)
and char_lit = parse
  | "\\n" '"'     { CHAR_LIT "\n" }
  | "\\t" '"'     { CHAR_LIT "\t" }
  | "\\r" '"'     { CHAR_LIT "\r" }
  | "\\\\" '"'    { CHAR_LIT "\\" }
  | "\\\"" '"'    { CHAR_LIT "\"" }
  | '\\' (digit digit digit as d) '"'
    { let code = int_of_string d in
      if code > 255 then
        raise (Lexer_error (Printf.sprintf "Invalid character escape: \\%s" d))
      else
        CHAR_LIT (String.make 1 (Char.chr code)) }
  | "\\^" (['@'-'_'] as c) '"'
    { CHAR_LIT (String.make 1 (Char.chr (Char.code c - 64))) }
  | ([^ '\\' '"'] as c) '"'
    { CHAR_LIT (String.make 1 c) }
  | eof           { raise (Lexer_error "Unterminated character literal") }
  | _             { raise (Lexer_error "Invalid character literal") }
