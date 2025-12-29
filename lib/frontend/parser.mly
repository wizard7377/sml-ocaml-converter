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
    | prog=list(prog) EOF { prog }
    ;

prog:
    | d=dec                             { ProgDec d }
    | FUNCTOR fb=fctbind                { ProgFun fb }
    | SIGNATURE sb=sigbind              { ProgStr sb }
    ;

(* ========================================================================= *)
(* Functor bindings                                                          *)
(* ========================================================================= *)

fctbind:
    (* id1 ( id2 : sig ) [:[:>] sig] = str [and fctbind] *)
    | id1=vid LPAREN id2=vid COLON s1=sig_ RPAREN ann=sigconstraint_opt EQUAL body=str rest=and_fctbind_opt
        { match ann with
          | None -> FctBind (id1, id2, s1, None, body, rest)
          | Some (a, s2) -> FctBind (id1, id2, s1, Some (a, s2), body, rest) }
    (* id ( spec ) [:[:>] sig] = str [and fctbind] - opened form *)
    | id=vid LPAREN sp=spec RPAREN ann=sigconstraint_opt EQUAL body=str rest=and_fctbind_opt
        { match ann with
          | None -> FctBindOpen (id, sp, None, body, rest)
          | Some (a, s) -> FctBindOpen (id, sp, Some (a, s), body, rest) }
    ;

and_fctbind_opt:
    | (* empty *)                       { None }
    | AND fb=fctbind                    { Some fb }
    ;

(* ========================================================================= *)
(* Signature bindings                                                        *)
(* ========================================================================= *)

sigbind:
    | id=vid EQUAL s=sig_ rest=and_sigbind_opt
        { SignBind (id, s, rest) }
    ;

and_sigbind_opt:
    | (* empty *)                       { None }
    | AND sb=sigbind                    { Some sb }
    ;

(* ========================================================================= *)
(* Constants                                                                 *)
(* ========================================================================= *)

scon:
    | s=INT_LIT                         { ConInt s }
    | s=HEX_LIT                         { ConWord s }
    | s=FLOAT_LIT                       { ConFloat s }
    | s=CHAR_LIT                        { ConChar s }
    | s=STRING_LIT                      { ConString s }
    ;

(* ========================================================================= *)
(* Identifiers                                                               *)
(* ========================================================================= *)

(* Any short identifier (alphanumeric or symbolic) *)
vid:
    | id=SHORT_IDENT                    { match id with Name s -> IdxIdx s | Symbol s -> IdxIdx s }
    | STAR                              { IdxIdx "*" }
    | EQUAL                             { IdxIdx "=" }
    ;

(* Long or short identifier *)
longid:
    | id=SHORT_IDENT                    { match id with Name s -> IdxIdx s | Symbol s -> IdxIdx s }
    | ids=LONG_IDENT                    { IdxLong (List.map (fun id -> match id with Name s -> IdxIdx s | Symbol s -> IdxIdx s) ids) }
    | STAR                              { IdxIdx "*" }
    | EQUAL                             { IdxIdx "=" }
    ;

(* Identifier usable as a value (includes op prefix) *)
op_vid:
    | id=longid                         { WithoutOp id }
    | OP id=longid                      { WithOp id }
    ;

(* Record label *)
lab:
    | id=SHORT_IDENT                    { match id with Name s -> IdxLab s | Symbol s -> IdxLab s }
    | n=INT_LIT                         { IdxNum n }
    ;

(* Type variable *)
tyvar:
    | tv=TYVAR                          { IdxVar tv }
    ;

(* List of long identifiers *)
longid_list:
    | id=longid                         { [id] }
    | id=longid ids=longid_list         { id :: ids }
    ;

(* ========================================================================= *)
(* Type variable sequences                                                   *)
(* ========================================================================= *)

tyvarseq:
    | (* empty *)                       { [] }
    | tv=tyvar                          { [tv] }
    | LPAREN tvs=tyvarseq1 RPAREN       { tvs }
    ;

tyvarseq1:
    | tv=tyvar                          { [tv] }
    | tv=tyvar COMMA tvs=tyvarseq1      { tv :: tvs }
    ;

(* ========================================================================= *)
(* Expressions                                                               *)
(* ========================================================================= *)

exp:
    | e=infix_exp                       { e }
    | e=exp COLON t=typ                 { TypedExp (e, t) }
    | e1=exp ANDALSO e2=exp             { AndExp (e1, e2) }
    | e1=exp ORELSE e2=exp              { OrExp (e1, e2) }
    | e=exp HANDLE m=match_clause       { HandleExp (e, m) }
    | RAISE e=exp                       { RaiseExp e }
    | IF e1=exp THEN e2=exp ELSE e3=exp { IfExp (e1, e2, e3) }
    | WHILE e1=exp DO e2=exp            { WhileExp (e1, e2) }
    | CASE e=exp OF m=match_clause      { CaseExp (e, m) }
    | FN m=match_clause                 { FnExp m }
    ;

infix_exp:
    | e=app_exp                         { e }
    | e1=infix_exp id=vid e2=infix_exp  { InfixApp (e1, id, e2) }
    ;

app_exp:
    | e=atomic_exp                      { e }
    | e1=app_exp e2=atomic_exp          { ExpApp (e1, e2) }
    ;

atomic_exp:
    | c=scon                            { ExpCon c }
    | id=op_vid                         { ExpIdx (match id with WithOp i -> i | WithoutOp i -> i) }
    | LPAREN RPAREN                     { TupleExp [] }
    | LPAREN e=exp RPAREN               { ParenExp e }
    | LPAREN e=exp COMMA es=exp_comma_list RPAREN
                                        { TupleExp (e :: es) }
    | LPAREN e=exp SEMICOLON es=exp_semi_list RPAREN
                                        { SeqExp (e :: es) }
    | LBRACE RBRACE                     { RecordExp [] }
    | LBRACE r=exprow RBRACE            { RecordExp [r] }
    | HASH l=lab                        { RecordSelector l }
    | LBRACKET RBRACKET                 { ListExp [] }
    | LBRACKET e=exp es=list_exp_comma RBRACKET
                                        { ListExp (e :: es) }
    | LET d=dec_seq IN e=exp es=let_exp_semi END
                                        { LetExp (d, e :: es) }
    ;

exp_comma_list:
    | e=exp                             { [e] }
    | e=exp COMMA es=exp_comma_list     { e :: es }
    ;

exp_semi_list:
    | e=exp                             { [e] }
    | e=exp SEMICOLON es=exp_semi_list  { e :: es }
    ;

list_exp_comma:
    | (* empty *)                       { [] }
    | COMMA e=exp es=list_exp_comma     { e :: es }
    ;

let_exp_semi:
    | (* empty *)                       { [] }
    | SEMICOLON e=exp es=let_exp_semi   { e :: es }
    ;

(* Expression rows for records *)
exprow:
    | l=lab EQUAL e=exp                 { Row (l, e, None) }
    | l=lab EQUAL e=exp COMMA r=exprow  { Row (l, e, Some r) }
    ;

(* Match clauses *)
match_clause:
    | p=pat BIGARROW e=exp              { Case (p, e, None) }
    | p=pat BIGARROW e=exp BAR m=match_clause
                                        { Case (p, e, Some m) }
    ;

(* ========================================================================= *)
(* Patterns                                                                  *)
(* ========================================================================= *)

pat:
    | p=infix_pat                       { p }
    | p=pat COLON t=typ                 { PatTyp (p, t) }
    | id=op_vid AS p=pat                { PatAs (id, None, p) }
    | id=op_vid COLON t=typ AS p=pat    { PatAs (id, Some t, p) }
    ;

infix_pat:
    | p=app_pat                         { p }
    | p1=infix_pat id=vid p2=infix_pat  { PatInfix (p1, id, p2) }
    ;

app_pat:
    | p=atomic_pat                      { p }
    | id=op_vid p=atomic_pat            { PatApp (id, p) }
    ;

atomic_pat:
    | c=scon                            { PatCon c }
    | UNDERSCORE                        { PatWildcard }
    | id=op_vid                         { PatIdx id }
    | LPAREN RPAREN                     { PatTuple [] }
    | LPAREN p=pat RPAREN               { PatParen p }
    | LPAREN p=pat COMMA ps=pat_comma_list RPAREN
                                        { PatTuple (p :: ps) }
    | LBRACE RBRACE                     { PatRecord [] }
    | LBRACE r=patrow RBRACE            { PatRecord [r] }
    | LBRACKET RBRACKET                 { PatList [] }
    | LBRACKET p=pat ps=list_pat_comma RBRACKET
                                        { PatList (p :: ps) }
    ;

pat_comma_list:
    | p=pat                             { [p] }
    | p=pat COMMA ps=pat_comma_list     { p :: ps }
    ;

list_pat_comma:
    | (* empty *)                       { [] }
    | COMMA p=pat ps=list_pat_comma     { p :: ps }
    ;

(* Pattern rows for records *)
patrow:
    | ELLIPSIS                          { PatRowPoly }
    | l=lab EQUAL p=pat                 { PatRowSimple (l, p, PatRowPoly) }
    | l=lab EQUAL p=pat COMMA r=patrow  { PatRowSimple (l, p, r) }
    | id=vid                            { PatRowVar (id, None, None, None) }
    | id=vid COLON t=typ                { PatRowVar (id, Some t, None, None) }
    | id=vid AS p=pat                   { PatRowVar (id, None, None (* should be Some id for as *), Some (PatRowSimple (id, p, PatRowPoly))) }
    | id=vid COLON t=typ AS p=pat       { PatRowVar (id, Some t, None, Some (PatRowSimple (id, p, PatRowPoly))) }
    | id=vid COMMA r=patrow             { PatRowVar (id, None, None, Some r) }
    | id=vid COLON t=typ COMMA r=patrow { PatRowVar (id, Some t, None, Some r) }
    ;

(* ========================================================================= *)
(* Types                                                                     *)
(* ========================================================================= *)

typ:
    | t=tuple_typ                       { t }
    | t1=typ ARROW t2=typ               { TypFun (t1, t2) }
    ;

tuple_typ:
    | t=app_typ                         { t }
    | t=app_typ STAR ts=tuple_typ_list  { TypTuple (t :: ts) }
    ;

tuple_typ_list:
    | t=app_typ                         { [t] }
    | t=app_typ STAR ts=tuple_typ_list  { t :: ts }
    ;

app_typ:
    | t=atomic_typ                      { t }
    | t=app_typ id=longid               { TypCon ([t], id) }
    ;

atomic_typ:
    | tv=tyvar                          { TypVar tv }
    | id=longid                         { TypCon ([], id) }
    | LPAREN t=typ RPAREN               { TypPar t }
    | LPAREN t=typ COMMA ts=typ_comma_list RPAREN id=longid
                                        { TypCon (t :: ts, id) }
    | LBRACE RBRACE                     { TypRecord [] }
    | LBRACE r=typrow RBRACE            { TypRecord [r] }
    ;

typ_comma_list:
    | t=typ                             { [t] }
    | t=typ COMMA ts=typ_comma_list     { t :: ts }
    ;

(* Type rows for records *)
typrow:
    | l=lab COLON t=typ                 { TypRow (l, t, None) }
    | l=lab COLON t=typ COMMA r=typrow  { TypRow (l, t, Some r) }
    ;

(* ========================================================================= *)
(* Declarations                                                              *)
(* ========================================================================= *)

dec:
    | VAL tvs=tyvarseq vb=valbind       { ValDec (tvs, vb) }
    | FUN fb=funbind                    { FunDec fb }
    | TYPE tb=typbind                   { TypDec tb }
    | DATATYPE db=datbind               { DatDec (db, None) }
    | DATATYPE db=datbind WITHTYPE tb=typbind
                                        { DatDec (db, Some tb) }
    | DATATYPE id=vid EQUAL DATATYPE lid=longid
                                        { DataDecAlias (id, lid) }
    | ABSTYPE db=datbind WITH d=dec_seq END
                                        { AbstractDec (db, None, d) }
    | ABSTYPE db=datbind WITHTYPE tb=typbind WITH d=dec_seq END
                                        { AbstractDec (db, Some tb, d) }
    | EXCEPTION eb=exnbind              { ExnDec eb }
    | STRUCTURE sb=strbind              { StrDec sb }
    | LOCAL d1=dec_seq IN d2=dec_seq END
                                        { LocalDec (SeqDec d1, SeqDec d2) }
    | OPEN ids=longid_list              { OpenDec ids }
    | fix=fixity ids=vid_list           { FixityDec (fix, ids) }
    ;

dec_seq:
    | (* empty *)                       { [] }
    | d=dec ds=dec_seq_rest             { d :: ds }
    ;

dec_seq_rest:
    | (* empty *)                       { [] }
    | SEMICOLON ds=dec_seq              { ds }
    | d=dec ds=dec_seq_rest             { d :: ds }
    ;

vid_list:
    | id=vid                            { [id] }
    | id=vid ids=vid_list               { id :: ids }
    ;

(* Fixity *)
fixity:
    | NONFIX                            { Nonfix }
    | INFIX                             { Infix 0 }
    | INFIX n=INT_LIT                   { Infix (int_of_string n) }
    | INFIXR                            { Infixr 0 }
    | INFIXR n=INT_LIT                  { Infixr (int_of_string n) }
    ;

(* ========================================================================= *)
(* Value bindings                                                            *)
(* ========================================================================= *)

valbind:
    | p=pat EQUAL e=exp                 { ValBind (p, e, None) }
    | p=pat EQUAL e=exp AND vb=valbind  { ValBind (p, e, Some vb) }
    | REC vb=valbind                    { ValBindRec vb }
    ;

(* ========================================================================= *)
(* Function bindings                                                         *)
(* ========================================================================= *)

funbind:
    | fm=funmatch                       { FunBind (fm, None) }
    | fm=funmatch AND fb=funbind        { FunBind (fm, Some fb) }
    ;

funmatch:
    (* [op] id pat1 ... patn [: typ] = exp [| funmatch] - prefix form *)
    | id=op_vid ps=pat_list1 t=typ_annot_opt EQUAL e=exp rest=funmatch_rest
        { FunMatchPrefix (id, ps, t, e, rest) }
    (* pat1 id pat2 [: typ] = exp [| funmatch] - infix form *)
    | p1=atomic_pat id=vid p2=atomic_pat t=typ_annot_opt EQUAL e=exp rest=funmatch_rest
        { FunMatchInfix (p1, id, p2, t, e, rest) }
    (* ( pat1 id pat2 ) pat'1 ... pat'n [: typ] = exp [| funmatch] - curried infix *)
    | LPAREN p1=pat id=vid p2=pat RPAREN ps=pat_list t=typ_annot_opt EQUAL e=exp rest=funmatch_rest
        { FunMatchLow (p1, id, p2, ps, t, e, rest) }
    ;

pat_list:
    | (* empty *)                       { [] }
    | p=atomic_pat ps=pat_list          { p :: ps }
    ;

pat_list1:
    | p=atomic_pat                      { [p] }
    | p=atomic_pat ps=pat_list1         { p :: ps }
    ;

typ_annot_opt:
    | (* empty *)                       { None }
    | COLON t=typ                       { Some t }
    ;

funmatch_rest:
    | (* empty *)                       { None }
    | BAR fm=funmatch                   { Some fm }
    ;

(* ========================================================================= *)
(* Type bindings                                                             *)
(* ========================================================================= *)

typbind:
    | tvs=tyvarseq id=vid EQUAL t=typ   { TypBind (tvs, id, t, None) }
    | tvs=tyvarseq id=vid EQUAL t=typ AND tb=typbind
                                        { TypBind (tvs, id, t, Some tb) }
    ;

(* ========================================================================= *)
(* Datatype bindings                                                         *)
(* ========================================================================= *)

datbind:
    | tvs=tyvarseq id=vid EQUAL cb=conbind
                                        { DatBind (tvs, id, cb, None) }
    | tvs=tyvarseq id=vid EQUAL cb=conbind AND db=datbind
                                        { DatBind (tvs, id, cb, Some db) }
    ;

conbind:
    | id=vid                            { ConBind (id, None, None) }
    | id=vid OF t=typ                   { ConBind (id, Some t, None) }
    | id=vid BAR cb=conbind             { ConBind (id, None, Some cb) }
    | id=vid OF t=typ BAR cb=conbind    { ConBind (id, Some t, Some cb) }
    ;

(* ========================================================================= *)
(* Exception bindings                                                        *)
(* ========================================================================= *)

exnbind:
    | id=vid                            { ExnBind (id, None, None) }
    | id=vid OF t=typ                   { ExnBind (id, Some t, None) }
    | id=vid EQUAL lid=longid           { ExnBindAlias (id, lid, None) }
    | id=vid AND eb=exnbind             { ExnBind (id, None, Some eb) }
    | id=vid OF t=typ AND eb=exnbind    { ExnBind (id, Some t, Some eb) }
    | id=vid EQUAL lid=longid AND eb=exnbind
                                        { ExnBindAlias (id, lid, Some eb) }
    ;

(* ========================================================================= *)
(* Structures                                                                *)
(* ========================================================================= *)

str:
    | id=longid                         { StrIdx id }
    | STRUCT d=dec_seq END              { StructStr (SeqDec d) }
    (* Note: The AST AnotateStr doesn't store the signature - only the annotation type *)
    | s=str COLON _sg=sig_              { AnotateStr (IdxIdx "", Transparent, s) }
    | s=str COLON_GT _sg=sig_           { AnotateStr (IdxIdx "", Opaque, s) }
    | id=vid LPAREN s=str RPAREN        { FunctorApp (id, s) }
    | id=vid LPAREN d=dec_seq RPAREN    { FunctorAppAnonymous (id, SeqDec d) }
    | LET d=dec_seq IN s=str END        { LocalDec (SeqDec d, s) }
    ;

strbind:
    (* Note: The AST str_bind type omits the structure expression - see AST definition *)
    | id=vid ann=sigconstraint_opt EQUAL _s=str
                                        { StrBind (id, ann, None) }
    | id=vid ann=sigconstraint_opt EQUAL _s=str AND sb=strbind
                                        { StrBind (id, ann, Some sb) }
    ;

sigconstraint_opt:
    | (* empty *)                       { None }
    | COLON sg=sig_                     { Some (Transparent, sg) }
    | COLON_GT sg=sig_                  { Some (Opaque, sg) }
    ;

(* ========================================================================= *)
(* Signatures                                                                *)
(* ========================================================================= *)

sig_:
    | id=vid                            { SignIdx id }
    | SIG sp=spec_seq END               { SignSig (SignIdx (IdxIdx ""), sp) }
    | s=sig_ WHERE TYPE tr=typrefin     { SignWhere (s, tr) }
    ;

typrefin:
    | tvs=tyvarseq id=longid EQUAL t=typ
        { TypRef (tvs, id, t, None) }
    | tvs=tyvarseq id=longid EQUAL t=typ AND TYPE tr=typrefin
        { TypRef (tvs, id, t, Some (t, tr)) }
    ;

(* ========================================================================= *)
(* Specifications                                                            *)
(* ========================================================================= *)

spec:
    | VAL vd=valdesc                    { SpecVal vd }
    | TYPE td=typdesc                   { SpecTyp td }
    | EQTYPE td=typdesc                 { SpecEqtyp td }
    | TYPE tb=typbind                   { SpecTypBind tb }
    | DATATYPE dd=datdesc               { SpecDat dd }
    | DATATYPE id=vid EQUAL DATATYPE lid=longid
                                        { SpecDatAlias (id, lid) }
    | EXCEPTION ed=exndesc              { SpecExn ed }
    | STRUCTURE sd=strdesc              { SpecStr sd }
    | INCLUDE sg=sig_                   { SpecInclude sg }
    | INCLUDE ids=vid_list              { SpecIncludeIdx ids }
    | sp=spec SHARING TYPE ids=longid_eq_list
                                        { SpecSharingTyp (sp, ids) }
    | sp=spec SHARING ids=longid_eq_list
                                        { SpecSharingStr (sp, ids) }
    ;

spec_seq:
    | (* empty *)                       { SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar "'a"), None)) (* empty spec placeholder *) }
    | sp=spec                           { sp }
    | sp=spec SEMICOLON sps=spec_seq    { SpecSeq (sp, sps) }
    | sp=spec sps=spec_seq_nonempty     { SpecSeq (sp, sps) }
    ;

spec_seq_nonempty:
    | sp=spec                           { sp }
    | sp=spec SEMICOLON sps=spec_seq    { SpecSeq (sp, sps) }
    | sp=spec sps=spec_seq_nonempty     { SpecSeq (sp, sps) }
    ;

longid_eq_list:
    | id1=longid EQUAL id2=longid       { [id1; id2] }
    | id=longid EQUAL ids=longid_eq_list
                                        { id :: ids }
    ;

(* Value descriptions *)
valdesc:
    | id=vid COLON t=typ                { ValDesc (id, t, None) }
    | id=vid COLON t=typ AND vd=valdesc { ValDesc (id, t, Some vd) }
    ;

(* Type descriptions *)
typdesc:
    | tvs=tyvarseq id=vid               { TypDesc (tvs, id, None) }
    | tvs=tyvarseq id=vid AND td=typdesc
                                        { TypDesc (tvs, id, Some td) }
    ;

(* Datatype descriptions *)
datdesc:
    | tvs=tyvarseq id=vid EQUAL cd=condesc
                                        { DatDesc (tvs, id, cd, None) }
    | tvs=tyvarseq id=vid EQUAL cd=condesc AND dd=datdesc
                                        { DatDesc (tvs, id, cd, Some dd) }
    ;

(* Constructor descriptions *)
condesc:
    | id=vid                            { ConDesc (id, None, None) }
    | id=vid OF t=typ                   { ConDesc (id, Some t, None) }
    | id=vid BAR cd=condesc             { ConDesc (id, None, Some cd) }
    | id=vid OF t=typ BAR cd=condesc    { ConDesc (id, Some t, Some cd) }
    ;

(* Exception descriptions *)
exndesc:
    | id=vid                            { ExnDesc (id, None, None) }
    | id=vid OF t=typ                   { ExnDesc (id, Some t, None) }
    | id=vid AND ed=exndesc             { ExnDesc (id, None, Some ed) }
    | id=vid OF t=typ AND ed=exndesc    { ExnDesc (id, Some t, Some ed) }
    ;

(* Structure descriptions *)
strdesc:
    | id=vid COLON sg=sig_              { StrDesc (id, sg, None) }
    | id=vid COLON sg=sig_ AND sd=strdesc
                                        { StrDesc (id, sg, Some sd) }
    ;

%%
