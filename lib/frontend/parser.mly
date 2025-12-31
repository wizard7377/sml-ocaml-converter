%{
    open Tokens
    open Ast

    (* Helper to convert ident to idx *)
    let ident_to_idx = function
      | Name s -> IdxIdx s
      | Symbol s -> IdxIdx s

    (* Helper to convert ident list to long idx *)
    let idents_to_long_idx = function
      | [id] -> ident_to_idx id
      | ids -> IdxLong (List.map ident_to_idx ids)

    (* Helper for optional values with continuations *)
    let opt_chain opt cont =
      match opt with
      | None -> cont
      | Some v -> Some v
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

(* Identifiers *)
%token<Tokens.ident> SHORT_IDENT
%token<Tokens.ident list> LONG_IDENT
%token<string> TYVAR

%token<string list> EOF

(* ========================================================================= *)
(* Precedence and associativity (lowest to highest)                          *)
(* ========================================================================= *)

%right AND
%nonassoc BIGARROW
%nonassoc BAR
%nonassoc ELSE
%nonassoc DO
%nonassoc RAISE
%right HANDLE
%right ORELSE
%right ANDALSO
%right AS
%right ARROW
%nonassoc SHORT_IDENT EQUAL
%right STAR
%left COLON COLON_GT

(* ========================================================================= *)
(* Type declarations for nonterminals                                        *)
(* ========================================================================= *)

%type <Ast.prog * string list> file
%type <Ast.prog> program
%type <Ast.dec> dec_seq kwdec kwcoredec kwmoduledec
%type <Ast.dec list> kwdec_seq kwcoredec_seq
%type <Ast.exp> exp atomic_exp
%type <Ast.exp list> exp_comma_seq0 exp_comma_seq1 exp_comma_seq2 exp_semicolon_seq2 atomic_exp_seq1
%type <Ast.pat> pat atomic_pat
%type <Ast.pat list> pat_comma_seq0 pat_comma_seq1 pat_comma_seq2 atomic_pat_seq1
%type <Ast.typ> typ typ_sans_star atomic_typ
%type <Ast.typ list> tuple_typ
%type <Ast.typ list> typ_comma_seq2
%type <Ast.matching> match_clause
%type <Ast.row> exprow
%type <Ast.row list> exprow_opt
%type <Ast.pat_row list> patrow patrow_opt
%type <Ast.typ_row list> typrow typrow_opt
%type <Ast.val_bind> valbind
%type <Ast.val_bind option> and_valbind_opt
%type <Ast.fun_bind> funbind
%type <Ast.fun_bind option> and_funbind_opt
%type <Ast.fun_match> funmatch
%type <Ast.fun_match option> bar_funmatch_opt
%type <Ast.typ_bind> typbind
%type <Ast.typ_bind option> typbind_opt and_typbind_opt
%type <Ast.dat_bind> datbind datbind_0 datbind_n
%type <Ast.dat_bind option> and_datbind_opt
%type <Ast.con_bind> conbind
%type <Ast.con_bind option> bar_conbind_opt
%type <Ast.exn_bind> exnbind
%type <Ast.exn_bind option> and_exnbind_opt
%type <Ast.str_bind> strbind
%type <Ast.str_bind option> and_strbind_opt
%type <Ast.str> str atomic_str
%type <Ast.str list> atomic_str_seq1
%type <Ast.sign> sig_expr
%type <Ast.sign_bind> sigbind
%type <Ast.sign_bind option> and_sigbind_opt
%type <Ast.fct_bind> fctbind
%type <Ast.fct_bind option> and_fctbind_opt
%type <Ast.spec> spec kwspec kwcorespec kwmodulespec
%type <Ast.spec list> spec_seq corespec_seq
%type <Ast.val_desc> valdesc
%type <Ast.val_desc option> and_valdesc_opt
%type <Ast.typ_desc> typdesc
%type <Ast.typ_desc option> and_typdesc_opt
%type <Ast.dat_desc> datdesc datdesc_0 datdesc_n
%type <Ast.dat_desc option> and_datdesc_opt
%type <Ast.con_desc> condesc
%type <Ast.con_desc option> bar_condesc_opt
%type <Ast.exn_desc> exndesc
%type <Ast.exn_desc option> and_exndesc_opt
%type <Ast.str_desc> strdesc
%type <Ast.str_desc option> and_strdesc_opt
%type <Ast.typ_refine> typrefin
%type <Ast.typ_refine option> and_typrefin_opt
%type <Ast.idx> ident longid tyvar tycon lab modid sigid
%type <Ast.idx list> tyvarseq tyvarseq1 tyvar_comma_seq1 longid_seq1 eq_ident_seq1 sigid_seq2
%type <Ast.con> scon
%type <Ast.with_op> op_ident op_longid op_eq_ident
%type <Ast.anotate> anotate
%type <(Ast.anotate * Ast.sign) option> sigconstraint_opt
%type <Ast.typ option> of_typ_opt colon_typ_opt
%type <int> digit_opt
%type <Ast.pat option> as_pat_opt

%start file

%%

(* ========================================================================= *)
(* Identifiers                                                               *)
(* ========================================================================= *)

ident:
  | SHORT_IDENT { ident_to_idx $1 }
  | STAR { IdxIdx "*" }
;

eq_ident:
  | ident { $1 }
  | EQUAL { IdxIdx "=" }
;

op_ident:
  | ident { WithoutOp $1 }
  | OP ident { WithOp $2 }
;

modid:
  | ident { $1 }
;

sigid:
  | ident { $1 }
;

tyvar:
  | TYVAR { IdxVar $1 }
;

tycon:
  | SHORT_IDENT { ident_to_idx $1 }
;

longid:
  | ident { $1 }
  | LONG_IDENT { idents_to_long_idx $1 }
;

long_tycon:
  | tycon { $1 }
  | LONG_IDENT { idents_to_long_idx $1 }
;

op_longid:
  | longid { WithoutOp $1 }
  | OP ident { WithOp $2 }
  | OP LONG_IDENT { WithOp (idents_to_long_idx $2) }
;

op_eq_ident:
  | op_longid { $1 }
  | EQUAL { WithoutOp (IdxIdx "=") }
  | OP EQUAL { WithOp (IdxIdx "=") }
;

eq_ident_seq1:
  | eq_ident eq_ident_seq1 { $1 :: $2 }
  | eq_ident { [$1] }
;

longid_seq1:
  | longid longid_seq1 { $1 :: $2 }
  | longid { [$1] }
;

digit_opt:
  | INT_LIT { int_of_string $1 }
  | { 0 }
;

lab:
  | ident { $1 }
  | INT_LIT { IdxNum $1 }
;

(* ========================================================================= *)
(* Constants                                                                 *)
(* ========================================================================= *)

scon:
  | INT_LIT { ConInt $1 }
  | HEX_LIT { ConWord $1 }
  | FLOAT_LIT { ConFloat $1 }
  | CHAR_LIT { ConChar $1 }
  | STRING_LIT { ConString $1 }
;

(* ========================================================================= *)
(* Type Variables                                                            *)
(* ========================================================================= *)

tyvarseq:
  | tyvarseq1 { $1 }
  | { [] }
;

tyvarseq1:
  | tyvar { [$1] }
  | LPAREN tyvar_comma_seq1 RPAREN { $2 }
;

tyvar_comma_seq1:
  | tyvar COMMA tyvar_comma_seq1 { $1 :: $3 }
  | tyvar { [$1] }
;

(* ========================================================================= *)
(* Types                                                                     *)
(* ========================================================================= *)

typ:
  | tuple_typ ARROW typ {
      let src = if List.length $1 = 1 then List.hd $1 else TypTuple $1 in
      TypFun (src, $3)
    }
  | tuple_typ {
      if List.length $1 = 1 then List.hd $1 else TypTuple $1
    }
;

tuple_typ:
  | typ_sans_star { [$1] }
  | typ_sans_star STAR tuple_typ { $1 :: $3 }
;

typ_sans_star:
  | LPAREN typ_comma_seq2 RPAREN long_tycon { TypCon ($2, $4) }
  | typ_sans_star long_tycon { TypCon ([$1], $2) }
  | atomic_typ { $1 }
;

atomic_typ:
  | long_tycon { TypCon ([], $1) }
  | tyvar { TypVar $1 }
  | LBRACE typrow_opt RBRACE { TypRecord $2 }
  | LPAREN typ RPAREN { TypPar $2 }
;

typ_comma_seq2:
  | typ COMMA typ_comma_seq2 { $1 :: $3 }
  | typ COMMA typ { [$1; $3] }
;

typrow_opt:
  | typrow { $1 }
  | { [] }
;

typrow:
  | lab COLON typ comma_typrow_opt { TypRow ($1, $3, None) :: $4 }
;

comma_typrow_opt:
  | COMMA typrow { $2 }
  | { [] }
;

(* ========================================================================= *)
(* Expressions                                                               *)
(* ========================================================================= *)

exp:
  | atomic_exp_seq1 {
      match $1 with
      | [e] -> e
      | f :: args -> List.fold_left (fun acc arg -> ExpApp (acc, arg)) f args
      | [] -> failwith "impossible: empty exp sequence"
    }
  | exp COLON typ { TypedExp ($1, $3) }
  | exp ANDALSO exp { AndExp ($1, $3) }
  | exp ORELSE exp { OrExp ($1, $3) }
  | e=exp HANDLE m=match_clause { HandleExp (e, m) }
  | RAISE e=exp { RaiseExp e }
  | IF c=exp THEN t=exp ELSE f=exp { IfExp (c, t, f) }
  | WHILE c=exp DO b=exp { WhileExp (c, b) }
  | CASE e=exp OF m=match_clause { CaseExp (e, m) }
  | FN m=match_clause { FnExp m }
;

atomic_exp_seq1:
  | atomic_exp atomic_exp_seq1 { $1 :: $2 }
  | atomic_exp { [$1] }
;

atomic_exp:
  | scon { ExpCon $1 }
  | op_eq_ident { ExpIdx (match $1 with WithOp i | WithoutOp i -> i) }
  | LET dec_seq IN exp_semicolon_exp END { LetExp ([$2], $4) }
  | HASH lab { RecordSelector $2 }
  | LPAREN exp RPAREN { ParenExp $2 }
  | LPAREN RPAREN { TupleExp [] }
  | LPAREN exp_comma_seq2 RPAREN { TupleExp $2 }
  | LPAREN exp_semicolon_seq2 RPAREN { SeqExp $2 }
  | LBRACE exprow_opt RBRACE { RecordExp $2 }
  | LBRACKET exp_comma_seq0 RBRACKET { ListExp $2 }
;

exp_semicolon_exp:
  | exp { [$1] }
  | exp SEMICOLON exp_semicolon_exp { $1 :: $3 }
;

exp_comma_seq0:
  | exp_comma_seq1 { $1 }
  | { [] }
;

exp_comma_seq1:
  | exp COMMA exp_comma_seq1 { $1 :: $3 }
  | exp { [$1] }
;

exp_comma_seq2:
  | exp COMMA exp_comma_seq1 { $1 :: $3 }
;

exp_semicolon_seq2:
  | exp SEMICOLON exp_semicolon_seq2 { $1 :: $3 }
  | exp SEMICOLON exp { [$1; $3] }
;

exprow_opt:
  | exprow_list { $1 }
  | { [] }
;

exprow_list:
  | exprow comma_exprow_opt { $1 :: $2 }
;

exprow:
  | lab EQUAL exp { Row ($1, $3, None) }
;

comma_exprow_opt:
  | COMMA exprow_list { $2 }
  | { [] }
;

match_clause:
  | p=pat BIGARROW e=exp BAR rest=match_clause { Case (p, e, Some rest) }
  | p=pat BIGARROW e=exp { Case (p, e, None) }
;

(* ========================================================================= *)
(* Patterns                                                                  *)
(* ========================================================================= *)

pat:
  | atomic_pat_seq1 {
      match $1 with
      | [p] -> p
      (* | (PatIdx op) :: args -> *)
      | op :: args ->
          List.fold_left (fun acc arg -> PatApp (
            (match acc with
             | PatIdx w -> w
             | PatApp (w, _) -> w
             (* | _ -> WithoutOp (IdxIdx "?")), arg)) (PatIdx op) args *)
             | _ -> WithoutOp (IdxIdx "?")), arg)) op args 
          
      | _ -> failwith "impossible: invalid pattern sequence"
    }
  | pat COLON typ { PatTyp ($1, $3) }
  | pat AS pat {
      match $1 with
      | PatIdx op -> PatAs (op, None, $3)
      | PatTyp (PatIdx op, ty) -> PatAs (op, Some ty, $3)
      | _ -> failwith "invalid layered pattern"
    }
;

atomic_pat_seq1:
  | atomic_pat atomic_pat_seq1 { $1 :: $2 }
  | atomic_pat { [$1] }
;

atomic_pat:
  | UNDERSCORE { PatWildcard }
  | scon { PatCon $1 }
  | op_longid { PatIdx $1 }
  | LBRACE patrow_opt RBRACE { PatRecord $2 }
  | LPAREN pat RPAREN { PatParen $2 }
  | LPAREN RPAREN { PatTuple [] }
  | LPAREN pat_comma_seq2 RPAREN { PatTuple $2 }
  | LBRACKET pat_comma_seq0 RBRACKET { PatList $2 }
;

patrow_opt:
  | patrow { $1 }
  | { [] }
;

patrow:
  | ELLIPSIS { [PatRowPoly] }
  | lab EQUAL pat comma_patrow_opt { PatRowSimple ($1, $3, match $4 with [] -> PatRowPoly | h :: _ -> h) :: $4 }
  | ident colon_typ_opt as_pat_opt comma_patrow_opt {
      PatRowVar ($1, $2, None, match $4 with [] -> None | _ -> Some (List.hd $4)) :: $4
    }
;

as_pat_opt:
  | AS pat { Some $2 }
  | { None }
;

comma_patrow_opt:
  | COMMA patrow { $2 }
  | { [] }
;

pat_comma_seq0:
  | pat_comma_seq1 { $1 }
  | { [] }
;

pat_comma_seq1:
  | pat COMMA pat_comma_seq1 { $1 :: $3 }
  | pat { [$1] }
;

pat_comma_seq2:
  | pat COMMA pat_comma_seq1 { $1 :: $3 }
;

(* ========================================================================= *)
(* Declarations                                                              *)
(* ========================================================================= *)

dec_seq:
  | kwdec dec_seq { SeqDec [$1; $2] }
  | SEMICOLON dec_seq { $2 }
  | { SeqDec [] }
;

kwdec_seq:
  | kwdec kwdec_seq { $1 :: $2 }
  | SEMICOLON kwdec_seq { $2 }
  | { [] }
;

kwcoredec_seq:
  | kwcoredec kwcoredec_seq { $1 :: $2 }
  | SEMICOLON kwcoredec_seq { $2 }
  | { [] }
;

kwdec:
  | kwcoredec { $1 }
  | kwmoduledec { $1 }
;

kwmoduledec:
  | STRUCTURE strbind { StrDec $2 }
;

kwcoredec:
  | VAL valbind { ValDec ([], $2) }
  | VAL tyvarseq1 valbind { ValDec ($2, $3) }
  | FUN funbind { FunDec $2 }
  | FUN tyvarseq1 funbind { FunDec $3 }
  | TYPE typbind { TypDec $2 }
  | DATATYPE datbind_0 typbind_opt { DatDec ($2, $3) }
  | DATATYPE datbind_n typbind_opt { DatDec ($2, $3) }
  | DATATYPE tycon EQUAL DATATYPE long_tycon { DataDecAlias ($2, $5) }
  | ABSTYPE datbind typbind_opt WITH dec_seq END { AbstractDec ($2, $3, [$5]) }
  | EXCEPTION exnbind { ExnDec $2 }
  | LOCAL dec_seq IN dec_seq END { LocalDec ($2, $4) }
  | OPEN longid_seq1 { OpenDec $2 }
  | INFIX digit_opt eq_ident_seq1 { FixityDec (Infix $2, $3) }
  | INFIXR digit_opt eq_ident_seq1 { FixityDec (Infixr $2, $3) }
  | NONFIX eq_ident_seq1 { FixityDec (Nonfix, $2) }
;

typbind_opt:
  | WITHTYPE typbind { Some $2 }
  | { None }
;

valbind:
  | pat EQUAL exp and_valbind_opt { ValBind ($1, $3, $4) }
  | REC valbind { ValBindRec $2 }
;

and_valbind_opt:
  | AND valbind { Some $2 }
  | { None }
;

funbind:
  | funmatch and_funbind_opt { FunBind ($1, $2) }
;

and_funbind_opt:
  | AND funbind { Some $2 }
  | { None }
;

funmatch:
  | op_ident atomic_pat_seq1 colon_typ_opt EQUAL exp bar_funmatch_opt {
      FunMatchPrefix ($1, $2, $3, $5, $6)
    }
  | atomic_pat ident atomic_pat colon_typ_opt EQUAL exp bar_funmatch_opt {
      FunMatchInfix ($1, $2, $3, $4, $6, $7)
    }
  | LPAREN atomic_pat ident atomic_pat RPAREN atomic_pat_seq1 colon_typ_opt EQUAL exp bar_funmatch_opt {
      FunMatchLow ($2, $3, $4, $6, $7, $9, $10)
    }
;

bar_funmatch_opt:
  | BAR funmatch { Some $2 }
  | { None }
;

colon_typ_opt:
  | COLON typ { Some $2 }
  | { None }
;

of_typ_opt:
  | OF typ { Some $2 }
  | { None }
;

typbind:
  | tyvarseq tycon EQUAL typ and_typbind_opt { TypBind ($1, $2, $4, $5) }
;

and_typbind_opt:
  | AND typbind { Some $2 }
  | { None }
;

datbind:
  | tyvarseq tycon EQUAL conbind and_datbind_opt { DatBind ($1, $2, $4, $5) }
;

datbind_0:
  | tycon EQUAL conbind and_datbind_opt { DatBind ([], $1, $3, $4) }
;

datbind_n:
  | tyvarseq1 tycon EQUAL conbind and_datbind_opt { DatBind ($1, $2, $4, $5) }
;

and_datbind_opt:
  | AND datbind { Some $2 }
  | { None }
;

conbind:
  | op_ident of_typ_opt bar_conbind_opt {
      let id = match $1 with WithOp i -> i | WithoutOp i -> i in
      ConBind (id, $2, $3)
    }
;

bar_conbind_opt:
  | BAR conbind { Some $2 }
  | { None }
;

exnbind:
  | op_ident of_typ_opt and_exnbind_opt {
      let id = match $1 with WithOp i -> i | WithoutOp i -> i in
      ExnBind (id, $2, $3)
    }
  | op_ident EQUAL op_eq_ident and_exnbind_opt {
      let id1 = match $1 with WithOp i -> i | WithoutOp i -> i in
      let id2 = match $3 with WithOp i -> i | WithoutOp i -> i in
      ExnBindAlias (id1, id2, $4)
    }
;

and_exnbind_opt:
  | AND exnbind { Some $2 }
  | { None }
;

(* ========================================================================= *)
(* Structures                                                                *)
(* ========================================================================= *)

strbind:
  | modid sigconstraint_opt EQUAL str and_strbind_opt {
      StrBind ($1, $2, $5)
    }
;

and_strbind_opt:
  | AND strbind { Some $2 }
  | { None }
;

sigconstraint_opt:
  | anotate sig_expr { Some ($1, $2) }
  | { None }
;

anotate:
  | COLON { Transparent }
  | COLON_GT { Opaque }
;

str:
  | atomic_str_seq1 {
      match $1 with
      | [s] -> s
      | f :: args -> List.fold_left (fun acc arg -> FunctorApp (
          (match acc with StrIdx i -> i | _ -> IdxIdx "?"), arg)) f args
      | [] -> failwith "impossible: empty str sequence"
    }
  | str COLON sig_expr { AnotateStr (IdxIdx "?", Transparent, $1) }
  | str COLON_GT sig_expr { AnotateStr (IdxIdx "?", Opaque, $1) }
;

atomic_str_seq1:
  | atomic_str atomic_str_seq1 { $1 :: $2 }
  | atomic_str { [$1] }
;

atomic_str:
  | STRUCT dec_seq END { StructStr $2 }
  | longid { StrIdx $1 }
  | LET dec_seq IN str END { Ast.LocalDec ($2, $4) }
  | LPAREN str RPAREN { $2 }
  | LPAREN dec_seq RPAREN { StructStr $2 }
;

(* ========================================================================= *)
(* Signatures                                                                *)
(* ========================================================================= *)

sig_expr:
  | SIG spec END { SignSig (SignIdx (IdxIdx ""), $2) }
  | sigid { SignIdx $1 }
  | sig_expr WHERE TYPE typrefin { SignWhere ($1, $4) }
  | FUNCTOR LPAREN modid COLON sig_expr RPAREN ARROW sig_expr {
      SignSig (SignIdx (IdxIdx "functor"), SpecStr (StrDesc ($3, $5, None)))
    }
  | FUNCTOR modid COLON sig_expr ARROW sig_expr {
      SignSig (SignIdx (IdxIdx "functor"), SpecStr (StrDesc ($2, $4, None)))
    }
;

typrefin:
  | tyvarseq long_tycon EQUAL typ and_typrefin_opt {
      TypRef ($1, $2, $4, match $5 with None -> None | Some r -> Some ($4, r))
    }
;

and_typrefin_opt:
  | AND TYPE typrefin { Some $3 }
  | { None }
;

sigbind:
  | sigid EQUAL sig_expr and_sigbind_opt { SignBind ($1, $3, $4) }
;

and_sigbind_opt:
  | AND sigbind { Some $2 }
  | { None }
;

(* ========================================================================= *)
(* Specifications                                                            *)
(* ========================================================================= *)

spec:
  | spec kwspec { SpecSeq ($1, $2) }
  | spec SHARING TYPE longid_eq_seq { SpecSharingTyp ($1, $4) }
  | spec SHARING longid_eq_seq { SpecSharingStr ($1, $3) }
  | spec SEMICOLON { $1 }
  | { SpecSeq (SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar ""), None)),
              SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar ""), None))) } (* empty *)
;

longid_eq_seq:
  | longid EQUAL longid_eq_seq { $1 :: $3 }
  | longid { [$1] }
;

spec_seq:
  | spec_seq kwspec { $2 :: $1 }
  | spec_seq SEMICOLON { $1 }
  | { [] }
;

corespec_seq:
  | corespec_seq kwcorespec { $2 :: $1 }
  | corespec_seq SEMICOLON { $1 }
  | { [] }
;

kwspec:
  | kwcorespec { $1 }
  | kwmodulespec { $1 }
;

kwcorespec:
  | VAL tyvarseq valdesc { SpecVal $3 }
  | TYPE typbind { SpecTypBind $2 }
  | TYPE typdesc { SpecTyp $2 }
  | EQTYPE typdesc { SpecEqtyp $2 }
  | DATATYPE datdesc_0 typbind_opt { SpecDat $2 }
  | DATATYPE datdesc_n typbind_opt { SpecDat $2 }
  | DATATYPE tycon EQUAL DATATYPE long_tycon { SpecDatAlias ($2, $5) }
  | EXCEPTION exndesc { SpecExn $2 }
  | LOCAL spec IN spec END { SpecSeq ($2, $4) }
  | OPEN longid_seq1 { SpecIncludeIdx $2 }
  | INFIX digit_opt eq_ident_seq1 { SpecSeq (SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar ""), None)),
                                             SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar ""), None))) }
  | INFIXR digit_opt eq_ident_seq1 { SpecSeq (SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar ""), None)),
                                              SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar ""), None))) }
  | NONFIX eq_ident_seq1 { SpecSeq (SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar ""), None)),
                                    SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar ""), None))) }
;

kwmodulespec:
  | STRUCTURE strdesc { SpecStr $2 }
  | FUNCTOR fundesc { SpecStr $2 }
  | INCLUDE sig_expr { SpecInclude $2 }
  | INCLUDE sigid_seq2 { SpecIncludeIdx $2 }
  | SIGNATURE sigbind { SpecSeq (SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar ""), None)),
                                 SpecVal (ValDesc (IdxIdx "", TypVar (IdxVar ""), None))) }
;

sigid_seq2:
  | sigid sigid_seq2 { $1 :: $2 }
  | sigid sigid { [$1; $2] }
;

valdesc:
  | op_ident COLON typ and_valdesc_opt {
      ValDesc ((match $1 with WithOp i -> i | WithoutOp i -> i), $3, $4)
    }
;

and_valdesc_opt:
  | AND valdesc { Some $2 }
  | { None }
;

typdesc:
  | tyvarseq tycon and_typdesc_opt { TypDesc ($1, $2, $3) }
;

and_typdesc_opt:
  | AND typdesc { Some $2 }
  | { None }
;

datdesc:
  | tyvarseq tycon EQUAL condesc and_datdesc_opt { DatDesc ($1, $2, $4, $5) }
;

datdesc_0:
  | tycon EQUAL condesc and_datdesc_opt { DatDesc ([], $1, $3, $4) }
;

datdesc_n:
  | tyvarseq1 tycon EQUAL condesc and_datdesc_opt { DatDesc ($1, $2, $4, $5) }
;

and_datdesc_opt:
  | AND datdesc { Some $2 }
  | { None }
;

condesc:
  | op_ident of_typ_opt bar_condesc_opt {
      ConDesc ((match $1 with WithOp i -> i | WithoutOp i -> i), $2, $3)
    }
;

bar_condesc_opt:
  | BAR condesc { Some $2 }
  | { None }
;

exndesc:
  | op_ident of_typ_opt and_exndesc_opt {
      ExnDesc ((match $1 with WithOp i -> i | WithoutOp i -> i), $2, $3)
    }
;

and_exndesc_opt:
  | AND exndesc { Some $2 }
  | { None }
;

strdesc:
  | modid COLON sig_expr and_strdesc_opt { StrDesc ($1, $3, $4) }
;

and_strdesc_opt:
  | AND strdesc { Some $2 }
  | { None }
;

fundesc:
  | modid fundesctail and_fundesc_opt { StrDesc ($1, $2, $3) }
;

fundesctail:
  | COLON sig_expr { $2 }
  | LPAREN modid COLON sig_expr RPAREN fundesctail { SignSig ($4, SpecStr (StrDesc ($2, $6, None))) }
  | modid COLON sig_expr fundesctail { SignSig ($3, SpecStr (StrDesc ($1, $4, None))) }
;

and_fundesc_opt:
  | AND fundesc { Some $2 }
  | { None }
;

(* ========================================================================= *)
(* Functors                                                                  *)
(* ========================================================================= *)

fctbind:
  | modid LPAREN modid COLON sig_expr RPAREN sigconstraint_opt EQUAL str and_fctbind_opt {
      FctBind ($1, $3, $5, $7, $9, $10)
    }
  | modid LPAREN spec RPAREN sigconstraint_opt EQUAL str and_fctbind_opt {
      FctBindOpen ($1, $3, $5, $7, $8)
    }
;

and_fctbind_opt:
  | AND fctbind { Some $2 }
  | { None }
;

(* ========================================================================= *)
(* Program (Top-level)                                                       *)
(* ========================================================================= *)

program:
  | dec_seq { (ProgDec $1) }
  | FUNCTOR fctbind SEMICOLON? { (ProgFun $2) }
  | SIGNATURE sigbind SEMICOLON? { (ProgStr $2) }
;
file: 
  | program EOF { ($1, $2) }
  (* TODO *)
  ;