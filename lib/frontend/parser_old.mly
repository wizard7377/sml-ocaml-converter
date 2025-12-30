%{
    open Tokens
    open Ast
    open Utils
%}

(* ========================================================================= *)
(* Token declarations                                                        *)
(* ========================================================================= *)

(* Keywords *)
%token ABSTYPE "abstype"
%token AND "and"
%token ANDALSO "andalso"
%token AS "as"
%token CASE "case"
%token DATATYPE "datatype"
%token DO "do"
%token ELSE "else"
%token END "end"
%token EQTYPE "eqtype"
%token EXCEPTION "exception"
%token FN "fn"
%token FUN "fun"
%token FUNCTOR "functor"
%token HANDLE "handle"
%token IF "if"
%token IN "in"
%token INCLUDE "include"
%token INFIX "infix"
%token INFIXR "infixr"
%token LET "let"
%token LOCAL "local"
%token NONFIX "nonfix"
%token OF "of"
%token OP "op"
%token OPEN "open"
%token ORELSE "orelse"
%token RAISE "raise"
%token REC "rec"
%token SHARING "sharing"
%token SIG "sig"
%token SIGNATURE "signature"
%token STRUCT "struct"
%token STRUCTURE "structure"
%token THEN "then"
%token TYPE "type"
%token VAL "val"
%token WHERE "where"
%token WITH "with"
%token WITHTYPE "withtype"
%token WHILE "while"

(* Punctuation *)
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACKET "["
%token RBRACKET "]"
%token COMMA ","
%token SEMICOLON ";"
%token COLON ":"
%token COLON_GT ":>"
%token EQUAL "="
%token BAR "|"
%token ELLIPSIS "..."
%token BIGARROW "=>"
%token ARROW "->"
%token UNDERSCORE "_"
%token HASH "#"
%token STAR "*"

(* Literals *)
%token<string> STRING_LIT
%token<string> CHAR_LIT
%token<string> INT_LIT
%token<string> HEX_LIT
%token<string> FLOAT_LIT
%token<string> COMMENT

(* Identifiers *)
%token<Tokens.ident> SHORT_IDENT
%token<Tokens.ident list> LONG_IDENT
%token<string> TYVAR

%token EOF

(* ========================================================================= *)
(* Precedence and associativity (lowest to highest)                          *)
(* ========================================================================= *)

(* Expression precedence - lowest to highest
   Note: Some conflicts remain and are resolved by Menhir's defaults, which is correct for SML *)
%nonassoc ELSE DO                  (* if-then-else, while-do *)
%nonassoc HANDLE                   (* exception handling *)
%right ORELSE                      (* disjunction *)
%right ANDALSO                     (* conjunction *)
%left COLON                        (* type annotation *)
%right AS                          (* layered patterns *)
%right ARROW                       (* function types *)
%left STAR                         (* tuple types *)
%nonassoc RAISE                    (* exception raising *)
%right SEMICOLON                  (* sequencing *)
(* ========================================================================= *)
(* Type declarations for nonterminals                                        *)
(* ========================================================================= *)

%type <Ast.prog> program

%type <Ast.dec> dec
%type <Ast.dec list> dec_seq
%type <Ast.exp> exp atomic_exp
%type <Ast.pat> pat atomic_pat
%type <Ast.typ> typ app_typ atomic_typ
%type <Ast.matching> match_clause
%type <Ast.row> exprow
%type <Ast.pat_row> patrow
%type <Ast.typ_row> typrow
%type <Ast.val_bind> valbind
%type <Ast.fun_bind> funbind
%type <Ast.fun_match> funmatch
%type <Ast.typ_bind> typbind
%type <Ast.dat_bind> datbind
%type <Ast.con_bind> conbind
%type <Ast.exn_bind> exnbind
%type <Ast.str_bind> strbind
%type <Ast.str> str
%type <Ast.sign> sig_
%type <Ast.sign_bind> sigbind
%type <Ast.fct_bind> fctbind
%type <Ast.spec> spec spec_seq
%type <Ast.val_desc> valdesc
%type <Ast.typ_desc> typdesc
%type <Ast.dat_desc> datdesc
%type <Ast.con_desc> condesc
%type <Ast.exn_desc> exndesc
%type <Ast.str_desc> strdesc
%type <Ast.typ_refine> typrefin
%type <Ast.idx> longid lab vid
%type <Ast.idx list> tyvarseq longid_list
%type <Ast.con> scon
%type <Ast.with_op> op_vid
%type <Ast.fixity> fixity
%type <(Ast.anotate * Ast.sign) option> sigconstraint_opt

%start program

%%
program : 
    | dec EOF { ProgDec $1 }
    | "functor" fct_bind EOF { ProgFun $2 } 
    | "signature" sig_bind EOF { ProgSig $2 }
    | seperated_list(";", program) EOF { List.fold_left ProgSeq $1 }
    ;
anotate : COLON { Transparent } | COLON_GT { Opaque } ;
idx : ;
typ : ; 
exp : ; 
pat : ; 
dec : ;
fct_bind : 
    | id1=idx ( "(" id2=idx : sig1=sig ")")? (anotate) 
    |

sig_bind : ;

