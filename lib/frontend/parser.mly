%{
    open Tokens
    open Ast
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

(* ========================================================================= *)
(* Type declarations for nonterminals                                        *)
(* ========================================================================= *)

%type <Ast.prog list> program
%type <Ast.prog> prog
%type <Ast.dec> dec
%type <Ast.dec list> dec_seq
%type <Ast.exp> exp atomic_exp app_exp infix_exp
%type <Ast.pat> pat atomic_pat app_pat infix_pat
%type <Ast.typ> typ atomic_typ
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
%type <Ast.idx list> tyvarseq tyvarseq1 longid_list
%type <Ast.con> scon
%type <Ast.with_op> op_vid
%type <Ast.fixity> fixity
%type <(Ast.anotate * Ast.sign) option> sigconstraint_opt

%start program

%%

(* ========================================================================= *)
(* Programs                                                                  *)
(* ========================================================================= *)

program:
    | EOF                           { [ProgEmpty] }
    | prog_list EOF                 { $1 }
    ;

prog_list:
    | prog                          { [$1] }
    | prog prog_list                { $1 :: $2 }
    ;

prog:
    | dec                           { ProgDec $1 }
    | FUNCTOR fctbind               { ProgFun $2 }
    | SIGNATURE sigbind             { ProgStr $2 }
    | prog SEMICOLON prog           { ProgSeq ($1, $3) }
    ;

(* ========================================================================= *)
(* Functor bindings                                                          *)
(* ========================================================================= *)

fctbind:
    (* id1 ( id2 : sig ) [:[:>] sig] = str [and fctbind] *)
    | vid LPAREN vid COLON sig_ RPAREN sigconstraint_opt EQUAL str and_fctbind_opt
        { match $7 with
          | None -> FctBind ($1, $3, $5, None, $9, $10)
          | Some (a, s2) -> FctBind ($1, $3, $5, Some (a, s2), $9, $10) }
    (* id ( spec ) [:[:>] sig] = str [and fctbind] - opened form *)
    | vid LPAREN spec RPAREN sigconstraint_opt EQUAL str and_fctbind_opt
        { match $5 with
          | None -> FctBindOpen ($1, $3, None, $7, $8)
          | Some (a, s) -> FctBindOpen ($1, $3, Some (a, s), $7, $8) }
    ;

and_fctbind_opt:
    | (* empty *)                   { None }
    | AND fctbind                   { Some $2 }
    ;

(* ========================================================================= *)
(* Signature bindings                                                        *)
(* ========================================================================= *)

sigbind:
    | vid EQUAL sig_ and_sigbind_opt
        { SignBind ($1, $3, $4) }
    ;

and_sigbind_opt:
    | (* empty *)                   { None }
    | AND sigbind                   { Some $2 }
    ;

(* ========================================================================= *)
(* Constants                                                                 *)
(* ========================================================================= *)

scon:
    | INT_LIT                       { ConInt $1 }
    | HEX_LIT                       { ConWord $1 }
    | FLOAT_LIT                     { ConFloat $1 }
    | CHAR_LIT                      { ConChar $1 }
    | STRING_LIT                    { ConString $1 }
    ;

(* ========================================================================= *)
(* Identifiers                                                               *)
(* ========================================================================= *)

(* Any short identifier (alphanumeric or symbolic) *)
vid:
    | SHORT_IDENT                   { match $1 with Name s -> IdxIdx s | Symbol s -> IdxIdx s }
    | STAR                          { IdxIdx "*" }
    | EQUAL                         { IdxIdx "=" }
    ;

(* Long or short identifier *)
longid:
    | SHORT_IDENT                   { match $1 with Name s -> IdxIdx s | Symbol s -> IdxIdx s }
    | LONG_IDENT                    { IdxLong (List.map (fun id -> match id with Name s -> IdxIdx s | Symbol s -> IdxIdx s) $1) }
    | STAR                          { IdxIdx "*" }
    | EQUAL                         { IdxIdx "=" }
    ;

(* Identifier usable as a value (includes op prefix) *)
op_vid:
    | longid                        { WithoutOp $1 }
    | OP longid                     { WithOp $2 }
    ;

(* Record label *)
lab:
    | SHORT_IDENT                   { match $1 with Name s -> IdxLab s | Symbol s -> IdxLab s }
    | INT_LIT                       { IdxNum $1 }
    ;

(* Type variable *)
tyvar:
    | TYVAR                         { IdxVar $1 }
    ;

(* List of long identifiers *)
longid_list:
    | longid                        { [$1] }
    | longid longid_list            { $1 :: $2 }
    ;

(* ========================================================================= *)
(* Type variable sequences                                                   *)
(* ========================================================================= *)

tyvarseq:
    | (* empty *)                   { [] }
    | tyvar                         { [$1] }
    | LPAREN tyvarseq1 RPAREN       { $2 }
    ;

tyvarseq1:
    | tyvar                         { [$1] }
    | tyvar COMMA tyvarseq1         { $1 :: $3 }
    ;

(* ========================================================================= *)
(* Expressions                                                               *)
(* ========================================================================= *)

exp:
    | infix_exp                     { $1 }
    | exp COLON typ                 { TypedExp ($1, $3) }
    | exp ANDALSO exp               { AndExp ($1, $3) }
    | exp ORELSE exp                { OrExp ($1, $3) }
    | exp HANDLE match_clause       { HandleExp ($1, $3) }
    | RAISE exp                     { RaiseExp $2 }
    | IF exp THEN exp ELSE exp      { IfExp ($2, $4, $6) }
    | WHILE exp DO exp              { WhileExp ($2, $4) }
    | CASE exp OF match_clause      { CaseExp ($2, $4) }
    | FN match_clause               { FnExp $2 }
    ;

infix_exp:
    | app_exp                       { $1 }
    | infix_exp vid infix_exp       { InfixApp ($1, $2, $3) }
    ;

app_exp:
    | atomic_exp                    { $1 }
    | app_exp atomic_exp            { ExpApp ($1, $2) }
    ;

atomic_exp:
    | scon                          { ExpCon $1 }
    | op_vid                        { ExpIdx (match $1 with WithOp i -> i | WithoutOp i -> i) }
    | LPAREN RPAREN                 { TupleExp [] }
    | LPAREN exp RPAREN             { ParenExp $2 }
    | LPAREN exp COMMA exp_comma_list RPAREN
                                    { TupleExp ($2 :: $4) }
    | LPAREN exp SEMICOLON exp_semi_list RPAREN
                                    { SeqExp ($2 :: $4) }
    | LBRACE RBRACE                 { RecordExp [] }
    | LBRACE exprow RBRACE          { RecordExp [$2] }
    | HASH lab                      { RecordSelector $2 }
    | LBRACKET RBRACKET             { ListExp [] }
    | LBRACKET exp list_exp_comma RBRACKET
                                    { ListExp ($2 :: $3) }
    | LET dec_seq IN exp let_exp_semi END
                                    { LetExp ($2, $4 :: $5) }
    ;

exp_comma_list:
    | exp                           { [$1] }
    | exp COMMA exp_comma_list      { $1 :: $3 }
    ;

exp_semi_list:
    | exp                           { [$1] }
    | exp SEMICOLON exp_semi_list   { $1 :: $3 }
    ;

list_exp_comma:
    | (* empty *)                   { [] }
    | COMMA exp list_exp_comma      { $2 :: $3 }
    ;

let_exp_semi:
    | (* empty *)                   { [] }
    | SEMICOLON exp let_exp_semi    { $2 :: $3 }
    ;

(* Expression rows for records *)
exprow:
    | lab EQUAL exp                 { Row ($1, $3, None) }
    | lab EQUAL exp COMMA exprow    { Row ($1, $3, Some $5) }
    ;

(* Match clauses *)
match_clause:
    | pat BIGARROW exp              { Case ($1, $3, None) }
    | pat BIGARROW exp BAR match_clause
                                    { Case ($1, $3, Some $5) }
    ;

(* ========================================================================= *)
(* Patterns                                                                  *)
(* ========================================================================= *)

pat:
    | infix_pat                     { $1 }
    | pat COLON typ                 { PatTyp ($1, $3) }
    | op_vid AS pat                 { PatAs ($1, None, $3) }
    | op_vid COLON typ AS pat       { PatAs ($1, Some $3, $5) }
    ;

infix_pat:
    | app_pat                       { $1 }
    | infix_pat vid infix_pat       { PatInfix ($1, $2, $3) }
    ;

app_pat:
    | atomic_pat                    { $1 }
    | op_vid atomic_pat             { PatApp ($1, $2) }
    ;

atomic_pat:
    | scon                          { PatCon $1 }
    | UNDERSCORE                    { PatWildcard }
    | op_vid                        { PatIdx $1 }
    | LPAREN RPAREN                 { PatTuple [] }
    | LPAREN pat RPAREN             { PatParen $2 }
    | LPAREN pat COMMA pat_comma_list RPAREN
                                    { PatTuple ($2 :: $4) }
    | LBRACE RBRACE                 { PatRecord [] }
    | LBRACE patrow RBRACE          { PatRecord [$2] }
    | LBRACKET RBRACKET             { PatList [] }
    | LBRACKET pat list_pat_comma RBRACKET
                                    { PatList ($2 :: $3) }
    ;

pat_comma_list:
    | pat                           { [$1] }
    | pat COMMA pat_comma_list      { $1 :: $3 }
    ;

list_pat_comma:
    | (* empty *)                   { [] }
    | COMMA pat list_pat_comma      { $2 :: $3 }
    ;

(* Pattern rows for records *)
patrow:
    | ELLIPSIS                      { PatRowPoly }
    | lab EQUAL pat                 { PatRowSimple ($1, $3, PatRowPoly) }
    | lab EQUAL pat COMMA patrow    { PatRowSimple ($1, $3, $5) }
    | vid                           { PatRowVar ($1, None, None, None) }
    | vid COLON typ                 { PatRowVar ($1, Some $3, None, None) }
    | vid AS pat                    { PatRowVar ($1, None, None (* should be Some id for as *), Some (PatRowSimple ($1, $3, PatRowPoly))) }
    | vid COLON typ AS pat          { PatRowVar ($1, Some $3, None, Some (PatRowSimple ($1, $5, PatRowPoly))) }
    | vid COMMA patrow              { PatRowVar ($1, None, None, Some $3) }
    | vid COLON typ COMMA patrow    { PatRowVar ($1, Some $3, None, Some $5) }
    ;

(* ========================================================================= *)
(* Types                                                                     *)
(* ========================================================================= *)

typ:
    | tuple_typ                     { $1 }
    | typ ARROW typ                 { TypFun ($1, $3) }
    ;

tuple_typ:
    | app_typ                       { $1 }
    | app_typ STAR tuple_typ_list   { TypTuple ($1 :: $3) }
    ;

tuple_typ_list:
    | app_typ                       { [$1] }
    | app_typ STAR tuple_typ_list   { $1 :: $3 }
    ;

app_typ:
    | atomic_typ                    { $1 }
    | app_typ longid                { TypCon ([$1], $2) }
    ;

atomic_typ:
    | tyvar                         { TypVar $1 }
    | longid                        { TypCon ([], $1) }
    | LPAREN typ RPAREN             { TypPar $2 }
    | LPAREN typ COMMA typ_comma_list RPAREN longid
                                    { TypCon ($2 :: $4, $6) }
    | LBRACE RBRACE                 { TypRecord [] }
    | LBRACE typrow RBRACE          { TypRecord [$2] }
    ;

typ_comma_list:
    | typ                           { [$1] }
    | typ COMMA typ_comma_list      { $1 :: $3 }
    ;

(* Type rows for records *)
typrow:
    | lab COLON typ                 { TypRow ($1, $3, None) }
    | lab COLON typ COMMA typrow    { TypRow ($1, $3, Some $5) }
    ;

(* ========================================================================= *)
(* Declarations                                                              *)
(* ========================================================================= *)

dec:
    | VAL tyvarseq valbind          { ValDec ($2, $3) }
    | FUN funbind                   { FunDec $2 }
    | TYPE typbind                  { TypDec $2 }
    | DATATYPE datbind              { DatDec ($2, None) }
    | DATATYPE datbind WITHTYPE typbind
                                    { DatDec ($2, Some $4) }
    | DATATYPE vid EQUAL DATATYPE longid
                                    { DataDecAlias ($2, $5) }
    | ABSTYPE datbind WITH dec_seq END
                                    { AbstractDec ($2, None, $4) }
    | ABSTYPE datbind WITHTYPE typbind WITH dec_seq END
                                    { AbstractDec ($2, Some $4, $6) }
    | EXCEPTION exnbind             { ExnDec $2 }
    | STRUCTURE strbind             { StrDec $2 }
    | LOCAL dec_seq IN dec_seq END
                                    { LocalDec (SeqDec $2, SeqDec $4) }
    | OPEN longid_list              { OpenDec $2 }
    | fixity vid_list               { FixityDec ($1, $2) }
    ;

dec_seq:
    | (* empty *)                   { [] }
    | dec dec_seq_rest              { $1 :: $2 }
    ;

dec_seq_rest:
    | (* empty *)                   { [] }
    | SEMICOLON dec_seq             { $2 }
    | dec dec_seq_rest              { $1 :: $2 }
    ;

vid_list:
    | vid                           { [$1] }
    | vid vid_list                  { $1 :: $2 }
    ;

(* Fixity *)
fixity:
    | NONFIX                        { Nonfix }
    | INFIX                         { Infix 0 }
    | INFIX INT_LIT                 { Infix (int_of_string $2) }
    | INFIXR                        { Infixr 0 }
    | INFIXR INT_LIT                { Infixr (int_of_string $2) }
    ;

(* ========================================================================= *)
(* Value bindings                                                            *)
(* ========================================================================= *)

valbind:
    | pat EQUAL exp                 { ValBind ($1, $3, None) }
    | pat EQUAL exp AND valbind     { ValBind ($1, $3, Some $5) }
    | REC valbind                   { ValBindRec $2 }
    ;

(* ========================================================================= *)
(* Function bindings                                                         *)
(* ========================================================================= *)

funbind:
    | funmatch                      { FunBind ($1, None) }
    | funmatch AND funbind          { FunBind ($1, Some $3) }
    ;

funmatch:
    (* [op] id pat1 ... patn [: typ] = exp [| funmatch] - prefix form *)
    | op_vid pat_list1 typ_annot_opt EQUAL exp funmatch_rest
        { FunMatchPrefix ($1, $2, $3, $5, $6) }
    (* pat1 id pat2 [: typ] = exp [| funmatch] - infix form *)
    | atomic_pat vid atomic_pat typ_annot_opt EQUAL exp funmatch_rest
        { FunMatchInfix ($1, $2, $3, $4, $6, $7) }
    (* ( pat1 id pat2 ) pat'1 ... pat'n [: typ] = exp [| funmatch] - curried infix *)
    | LPAREN pat vid pat RPAREN pat_list typ_annot_opt EQUAL exp funmatch_rest
        { FunMatchLow ($2, $3, $4, $6, $7, $9, $10) }
    ;

pat_list:
    | (* empty *)                   { [] }
    | atomic_pat pat_list           { $1 :: $2 }
    ;

pat_list1:
    | atomic_pat                    { [$1] }
    | atomic_pat pat_list1          { $1 :: $2 }
    ;

typ_annot_opt:
    | (* empty *)                   { None }
    | COLON typ                     { Some $2 }
    ;

funmatch_rest:
    | (* empty *)                   { None }
    | BAR funmatch                  { Some $2 }
    ;

(* ========================================================================= *)
(* Type bindings                                                             *)
(* ========================================================================= *)

typbind:
    | tyvarseq vid EQUAL typ        { TypBind ($1, $2, $4, None) }
    | tyvarseq vid EQUAL typ AND typbind
                                    { TypBind ($1, $2, $4, Some $6) }
    ;

(* ========================================================================= *)
(* Datatype bindings                                                         *)
(* ========================================================================= *)

datbind:
    | tyvarseq vid EQUAL conbind
                                    { DatBind ($1, $2, $4, None) }
    | tyvarseq vid EQUAL conbind AND datbind
                                    { DatBind ($1, $2, $4, Some $6) }
    ;

conbind:
    | vid                           { ConBind ($1, None, None) }
    | vid OF typ                    { ConBind ($1, Some $3, None) }
    | vid BAR conbind               { ConBind ($1, None, Some $3) }
    | vid OF typ BAR conbind        { ConBind ($1, Some $3, Some $5) }
    ;

(* ========================================================================= *)
(* Exception bindings                                                        *)
(* ========================================================================= *)

exnbind:
    | vid                           { ExnBind ($1, None, None) }
    | vid OF typ                    { ExnBind ($1, Some $3, None) }
    | vid EQUAL longid              { ExnBindAlias ($1, $3, None) }
    | vid AND exnbind               { ExnBind ($1, None, Some $3) }
    | vid OF typ AND exnbind        { ExnBind ($1, Some $3, Some $5) }
    | vid EQUAL longid AND exnbind
                                    { ExnBindAlias ($1, $3, Some $5) }
    ;

(* ========================================================================= *)
(* Structures                                                                *)
(* ========================================================================= *)

str:
    | longid                        { StrIdx $1 }
    | STRUCT dec_seq END            { StructStr (SeqDec $2) }
    (* Note: The AST AnotateStr doesn't store the signature - only the annotation type *)
    | str COLON sig_                { AnotateStr (IdxIdx "", Transparent, $1) }
    | str COLON_GT sig_             { AnotateStr (IdxIdx "", Opaque, $1) }
    | vid LPAREN str RPAREN         { FunctorApp ($1, $3) }
    | vid LPAREN dec_seq RPAREN     { FunctorAppAnonymous ($1, SeqDec $3) }
    | LET dec_seq IN str END        { LocalDec (SeqDec $2, $4) }
    ;

strbind:
    (* Note: The AST str_bind type omits the structure expression - see AST definition *)
    | vid sigconstraint_opt EQUAL str
                                    { StrBind ($1, $2, None) }
    | vid sigconstraint_opt EQUAL str AND strbind
                                    { StrBind ($1, $2, Some $6) }
    ;

sigconstraint_opt:
    | (* empty *)                   { None }
    | COLON sig_                    { Some (Transparent, $2) }
    | COLON_GT sig_                 { Some (Opaque, $2) }
    ;

(* ========================================================================= *)
(* Signatures                                                                *)
(* ========================================================================= *)

sig_:
    | vid                           { SignIdx $1 }
    | SIG spec_seq END              { SignSig (SignIdx (IdxIdx ""), $2) }
    | sig_ WHERE TYPE typrefin      { SignWhere ($1, $4) }
    ;

typrefin:
    | tyvarseq longid EQUAL typ
        { TypRef ($1, $2, $4, None) }
    | tyvarseq longid EQUAL typ AND TYPE typrefin
        { TypRef ($1, $2, $4, Some ($4, $7)) }
    ;

(* ========================================================================= *)
(* Specifications                                                            *)
(* ========================================================================= *)

spec:
    | VAL valdesc                   { SpecVal $2 }
    | TYPE typdesc                  { SpecTyp $2 }
    | EQTYPE typdesc                { SpecEqtyp $2 }
    | TYPE typbind                  { SpecTypBind $2 }
    | DATATYPE datdesc              { SpecDat $2 }
    | DATATYPE vid EQUAL DATATYPE longid
                                    { SpecDatAlias ($2, $5) }
    | EXCEPTION exndesc             { SpecExn $2 }
    | STRUCTURE strdesc             { SpecStr $2 }
    | INCLUDE sig_                  { SpecInclude $2 }
    | INCLUDE vid_list              { SpecIncludeIdx $2 }
    | spec SHARING TYPE longid_eq_list
                                    { SpecSharingTyp ($1, $4) }
    | spec SHARING longid_eq_list
                                    { SpecSharingStr ($1, $3) }
    ;

spec_seq:
    | (* empty *)                   { SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar "'a"), None)) (* empty spec placeholder *) }
    | spec                          { $1 }
    | spec SEMICOLON spec_seq       { SpecSeq ($1, $3) }
    | spec spec_seq_nonempty        { SpecSeq ($1, $2) }
    ;

spec_seq_nonempty:
    | spec                          { $1 }
    | spec SEMICOLON spec_seq       { SpecSeq ($1, $3) }
    | spec spec_seq_nonempty        { SpecSeq ($1, $2) }
    ;

longid_eq_list:
    | longid EQUAL longid           { [$1; $3] }
    | longid EQUAL longid_eq_list
                                    { $1 :: $3 }
    ;

(* Value descriptions *)
valdesc:
    | vid COLON typ                 { ValDesc ($1, $3, None) }
    | vid COLON typ AND valdesc     { ValDesc ($1, $3, Some $5) }
    ;

(* Type descriptions *)
typdesc:
    | tyvarseq vid                  { TypDesc ($1, $2, None) }
    | tyvarseq vid AND typdesc
                                    { TypDesc ($1, $2, Some $4) }
    ;

(* Datatype descriptions *)
datdesc:
    | tyvarseq vid EQUAL condesc
                                    { DatDesc ($1, $2, $4, None) }
    | tyvarseq vid EQUAL condesc AND datdesc
                                    { DatDesc ($1, $2, $4, Some $6) }
    ;

(* Constructor descriptions *)
condesc:
    | vid                           { ConDesc ($1, None, None) }
    | vid OF typ                    { ConDesc ($1, Some $3, None) }
    | vid BAR condesc               { ConDesc ($1, None, Some $3) }
    | vid OF typ BAR condesc        { ConDesc ($1, Some $3, Some $5) }
    ;

(* Exception descriptions *)
exndesc:
    | vid                           { ExnDesc ($1, None, None) }
    | vid OF typ                    { ExnDesc ($1, Some $3, None) }
    | vid AND exndesc               { ExnDesc ($1, None, Some $3) }
    | vid OF typ AND exndesc        { ExnDesc ($1, Some $3, Some $5) }
    ;

(* Structure descriptions *)
strdesc:
    | vid COLON sig_                { StrDesc ($1, $3, None) }
    | vid COLON sig_ AND strdesc
                                    { StrDesc ($1, $3, Some $5) }
    ;

%%
