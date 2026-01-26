%{
    open Tokens
    open Ast

    (* Alias for convenience *)
    let b = box_node

    (* Helper to create node with position information *)
    let bp (v : 'a) (start_pos : Lexing.position) (end_pos : Lexing.position) : 'a node =
      { value = v; pos = Some (start_pos, end_pos) }

    (* Helper to convert ident to idx *)
    let ident_to_idx = function
      | Name s -> IdxIdx (box_node s)
      | Symbol s -> IdxIdx (box_node s)

    (* Helper to convert ident list to long idx *)
    let idents_to_long_idx = function
      | [id] -> ident_to_idx id
      | ids -> IdxLong (List.map (fun x -> b (ident_to_idx x)) ids)

    (* Helper for optional values with continuations *)
    let opt_chain opt cont =
      match opt with
      | None -> cont
      | Some v -> Some v

    (* Flatten nested specification sequences into a list for SignSig. *)
    let rec flatten_spec_node (specification : Ast.specification Ast.node) : Ast.specification Ast.node list =
      match specification.value with
      | SpecSeq (s1, s2) -> flatten_spec_node s1 @ flatten_spec_node s2
      | _ -> [specification]
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
%token CONS "::"
%token EQUAL "="
%token BAR "|"
%token ELLIPSIS "..."
%token BIGARROW "=>"
%token ARROW "->"
%token UNDERSCORE "_"
%token HASH "#"
%token STAR "*"
%token HASH_OPEN "#["

(* Literals *)
%token<string> STRING_LIT
%token<string> CHAR_LIT
%token<string> INT_LIT
%token<string> HEX_LIT
%token<string> FLOAT_LIT

(* Identifiers *)
%token<Tokens.ident> SHORT_IDENT
%token<Tokens.ident> SYMBOL_IDENT
%token<Tokens.ident list> LONG_IDENT
%token<string> TYVAR

%token<string list> EOF

(* ========================================================================= *)
(* Precedence and associativity (lowest to highest)                          *)
(* ========================================================================= *)

%nonassoc EOF

%left PROGRAM_SEP
%left SEMICOLON

%right CONS
%right AND
%nonassoc BIGARROW
%left BAR
%nonassoc ELSE
%nonassoc DO
%nonassoc RAISE
%right HANDLE
%right ORELSE
%right ANDALSO
%right AS
%left INFIX_APP
%right ARROW
%nonassoc EQUAL
%right STAR
%left COLON COLON_GT
%right PREFIX_APP
%left PROGRAM_PREC 

(* ========================================================================= *)
(* Type declarations for nonterminals                                        *)
(* ========================================================================= *)
%type <Ast.prog * string list> main
%type <Ast.prog> file
%type <Ast.prog> program nonempty_program
%type <Ast.declaration> dec_seq nonempty_dec_seq kwdec kwcoredec kwmoduledec
%type <Ast.declaration node list> kwdec_seq kwcoredec_seq
%type <Ast.expression> expression atomic_exp
%type <Ast.expression node list> exp_comma_seq0 exp_comma_seq1 exp_comma_seq2 exp_semicolon_seq2 atomic_exp_seq1
%type <Ast.pat> pat atomic_pat
%type <Ast.pat node list> pat_comma_seq0 pat_comma_seq1 pat_comma_seq2 atomic_pat_seq1
%type <Ast.typ> typ typ_sans_star atomic_typ
%type <Ast.typ node list> tuple_typ
%type <Ast.typ node list> typ_comma_seq2
%type <Ast.matching> match_clause
%type <Ast.row> exprow
%type <Ast.row node list> exprow_opt
%type <Ast.pat_row node list> patrow patrow_opt
%type <Ast.typ_row node list> typrow typrow_opt
%type <Ast.value_binding> valbind
%type <Ast.value_binding node option> and_valbind_opt
%type <Ast.function_binding> funbind
%type <Ast.function_binding node option> and_funbind_opt
%type <Ast.fun_match> funmatch
%type <Ast.fun_match node option> bar_funmatch_opt
%type <Ast.type_binding> typbind
%type <Ast.type_binding node option> typbind_opt and_typbind_opt
%type <Ast.data_binding> datbind datbind_0 datbind_n
%type <Ast.data_binding node option> and_datbind_opt
%type <Ast.constructor_binding> conbind
%type <Ast.constructor_binding node option> bar_conbind_opt
%type <Ast.exn_bind> exnbind
%type <Ast.exn_bind node option> and_exnbind_opt
%type <Ast.structure_binding> strbind
%type <Ast.structure_binding node option> and_strbind_opt
%type <Ast.structure> structure atomic_str
%type <Ast.structure node list> atomic_str_seq1
%type <Ast.signature> sig_expr
%type <Ast.signature_binding> sigbind
%type <Ast.signature_binding node option> and_sigbind_opt
%type <Ast.functor_binding> fctbind
%type <Ast.functor_binding node option> and_fctbind_opt
%type <Ast.specification> specification kwspec kwcorespec kwmodulespec
%type <Ast.specification node list> spec_seq corespec_seq
%type <Ast.val_specification> valdesc
%type <Ast.val_specification node option> and_valdesc_opt
%type <Ast.typ_specification> typdesc
%type <Ast.typ_specification node option> and_typdesc_opt
%type <Ast.dat_specification> datdesc datdesc_0 datdesc_n
%type <Ast.dat_specification node option> and_datdesc_opt
%type <Ast.con_specification> condesc
%type <Ast.con_specification node option> bar_condesc_opt
%type <Ast.exn_specification> exndesc
%type <Ast.exn_specification node option> and_exndesc_opt
%type <Ast.str_specification> strdesc
%type <Ast.str_specification node option> and_strdesc_opt
%type <Ast.typ_refine> typrefin
%type <Ast.typ_refine node option> and_typrefin_opt
%type <Ast.idx> ident longid tyvar tycon lab modid sigid
%type <Ast.idx node list> tyvarseq tyvarseq1 tyvar_comma_seq1 longid_seq1 eq_ident_seq1 sigid_seq2
%type <Ast.constant> scon
%type <Ast.with_op> op_ident op_longid op_eq_ident
%type <Ast.anotate> anotate
%type <(Ast.anotate node * Ast.signature node) option> sigconstraint_opt
%type <Ast.typ node option> of_typ_opt colon_typ_opt
%type <int> digit_opt
%type <Ast.pat node option> as_pat_opt

%start main
%start<Ast.prog * string list> main_top
%start<Ast.expression> expression_top 
%start<Ast.pat> pat_top
%start<Ast.typ> typ_top

%%

(* ========================================================================= *)
(* Identifiers                                                               *)
(* ========================================================================= *)

main_top : 
  | main { $1 }
expression_top:
  | expression EOF { $1 }
;
pat_top:
  | pat EOF { $1 }
;
typ_top:
  | typ EOF { $1 }
;
let boxed(r) := 
  | v=r; { { value = v; pos=Some ($symbolstartpos , $endpos ) } } 

%inline ident:
  | SHORT_IDENT { ident_to_idx $1 }
  | lifted_symbol { ident_to_idx $1 }
  | boxed(STAR) { IdxIdx (b "*") }
;

%inline eq_ident:
  | ident { $1 }
  | boxed(EQUAL) { IdxIdx (b "=") }
;

%inline op_ident:
  | ident { WithoutOp (b $1) }
  | "op" ident { WithOp (b $2) }
;

%inline modid:
  | ident { $1 }
;

%inline sigid:
  | ident { $1 }
;

%inline tyvar:
  | TYVAR { IdxVar (b $1) }
;

%inline lifted_symbol: 
  | "op" SYMBOL_IDENT { $2 }
  | "(" SYMBOL_IDENT ")" { $2 }
%inline tycon:
  | SHORT_IDENT { ident_to_idx $1 }
  | lifted_symbol { ident_to_idx $1 }
;

%inline longid:
  | ident { $1 }
  | LONG_IDENT { idents_to_long_idx $1 }
;

%inline long_tycon:
  | tycon { $1 }
  | LONG_IDENT { idents_to_long_idx $1 }
;

op_longid:
  | longid { WithoutOp (b $1) }
  | "op" ident { WithOp (b $2) }
  | "op" LONG_IDENT { WithOp (b (idents_to_long_idx $2)) }
;

op_eq_ident:
  | op_longid { $1 }
  | EQUAL { WithoutOp (b (IdxIdx (b "="))) }
  | "op" EQUAL { WithOp (b (IdxIdx (b "="))) }
  | CONS { WithoutOp (b (IdxIdx (b "::"))) }
  | "op" CONS { WithOp (b (IdxIdx (b "::"))) }
;

eq_ident_seq1:
  | eq_ident eq_ident_seq1 { b $1 :: $2 }
  | eq_ident { [b $1] }
;

longid_seq1:
  | longid longid_seq1 { b $1 :: $2 }
  | longid { [b $1] }
;

digit_opt:
  | INT_LIT { int_of_string $1 }
  | { 0 }
;

lab:
  | ident { $1 }
  | INT_LIT { IdxNum (b $1) }
;

(* ========================================================================= *)
(* Constants                                                                 *)
(* ========================================================================= *)

scon:
  | INT_LIT { ConInt (b $1) }
  | HEX_LIT { ConWord (b $1) }
  | FLOAT_LIT { ConFloat (b $1) }
  | CHAR_LIT { ConChar (b $1) }
  | STRING_LIT { ConString (b $1) }
;

(* ========================================================================= *)
(* Type Variables                                                            *)
(* ========================================================================= *)

tyvarseq:
  | tyvarseq1 { $1 }
  | { [] }
;

tyvarseq1:
  | tyvar { [b $1] }
  | "(" tyvar_comma_seq1 ")" { $2 }
;

tyvar_comma_seq1:
  | tyvar "," tyvar_comma_seq1 { b $1 :: $3 }
  | tyvar { [b $1] }
;

(* ========================================================================= *)
(* Types                                                                     *)
(* ========================================================================= *)

typ:
  | tuple_typ "->" typ {
      let src = if List.length $1 = 1 then List.hd $1 else bp (TypTuple $1) $startpos($1) $endpos($1) in
      TypFun (src, bp $3 $startpos($3) $endpos($3))
    }
  | tuple_typ {
      if List.length $1 = 1 then (List.hd $1).value else TypTuple $1
    }
;

tuple_typ:
  | typ_sans_star { [bp $1 $startpos($1) $endpos($1)] }
  | typ_sans_star STAR tuple_typ { bp $1 $startpos($1) $endpos($1) :: $3 }
;

typ_sans_star:
  | "(" typ_comma_seq2 ")" long_tycon { TypCon ($2, bp $4 $startpos($4) $endpos($4)) }
  | typ_sans_star long_tycon { TypCon ([bp $1 $startpos($1) $endpos($1)], bp $2 $startpos($2) $endpos($2)) }
  | atomic_typ { $1 }
;

atomic_typ:
  | long_tycon { TypCon ([], bp $1 $startpos($1) $endpos($1)) }
  | tyvar { TypVar (bp $1 $startpos($1) $endpos($1)) }
  | LBRACE typrow_opt RBRACE { TypRecord $2 }
  | "(" typ ")" { TypPar (bp $2 $startpos($2) $endpos($2)) }
;

typ_comma_seq2:
  | typ "," typ_comma_seq2 { bp $1 $startpos($1) $endpos($1) :: $3 }
  | typ "," typ { [bp $1 $startpos($1) $endpos($1); bp $3 $startpos($3) $endpos($3)] }
;

typrow_opt:
  | typrow { $1 }
  | { [] }
;

typrow:
  | lab COLON typ comma_typrow_opt { bp (TypRow (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), None)) $startpos $endpos :: $4 }
;

comma_typrow_opt:
  | "," typrow { $2 }
  | { [] }
;

(* ========================================================================= *)
(* Expressions                                                               *)
(* ========================================================================= *)

expression:
  | atomic_exp_seq1 {
      match $1 with
      | [e] -> e.value
      | f :: args -> List.fold_left (fun acc arg -> ExpApp (b acc, arg)) f.value args
      | [] -> failwith "impossible: empty expression sequence"
    }
  | expression SYMBOL_IDENT expression %prec INFIX_APP { InfixApp (bp $1 $startpos($1) $endpos($1), b (IdxIdx (b (match $2 with Symbol s -> s | Name s -> s))), bp $3 $startpos($3) $endpos($3)) }
  | expression COLON typ { TypedExp (bp $1 $startpos $endpos, bp $3 $startpos($3) $endpos($3)) }
  | expression "andalso" expression { AndExp (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3)) }
  | expression "orelse" expression { OrExp (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3)) }
  | expression CONS expression { InfixApp (bp $1 $startpos($1) $endpos($1), b (IdxIdx (b "::")), bp $3 $startpos($3) $endpos($3)) }
  | e=expression "handle" m=match_clause { HandleExp (bp e $startpos(e) $endpos(e), bp m $startpos(m) $endpos(m)) }
  | "raise" e=expression { RaiseExp (bp e $startpos(e) $endpos(e)) }
  | "if" c=expression "then" t=expression "else" f=expression { IfExp (bp c $startpos(c) $endpos(c), bp t $startpos(t) $endpos(t), bp f $startpos(f) $endpos(f)) }
  | "while" c=expression "do" bdy=expression { WhileExp (bp c $startpos(c) $endpos(c), bp bdy $startpos(bdy) $endpos(bdy)) }
  | "case" e=expression "of" m=match_clause { CaseExp (bp e $startpos(e) $endpos(e), bp m $startpos(m) $endpos(m)) }
  | "fn" m=match_clause { FnExp (bp m $startpos(m) $endpos(m)) }
  | head=SYMBOL_IDENT arg=expression %prec PREFIX_APP { ExpApp ((bp (ExpIdx (bp (ident_to_idx head) $startpos(head) $endpos(head))) $startpos(head) $endpos(head)), bp arg $startpos(arg) $endpos(arg)) }
  ;

atomic_exp_seq1:
  | atomic_exp atomic_exp_seq1 { bp $1 $startpos($1) $endpos($1) :: $2 }
  | atomic_exp { [bp $1 $startpos($1) $endpos($1)] }
;

atomic_exp:
  | scon { ExpCon (bp $1 $startpos($1) $endpos($1)) }
  | op_eq_ident { ExpIdx (match $1 with WithOp i | WithoutOp i -> i) }
  | "let" dec_seq "in" exp_semicolon_exp "end" { LetExp ([bp $2 $startpos($2) $endpos($2)], $4) }
  | HASH lab { RecordSelector (bp $2 $startpos($2) $endpos($2)) }
  | "(" expression ")" { ParenExp (bp $2 $startpos($2) $endpos($2)) }
  | "(" ")" { TupleExp [] }
  | "(" exp_comma_seq2 ")" { TupleExp $2 }
  | "(" exp_semicolon_seq2 ")" { SeqExp $2 }
  | LBRACE exprow_opt RBRACE { RecordExp $2 }
  | HASH_OPEN exp_comma_seq0 RBRACKET { ArrayExp $2 }
  | LBRACKET exp_comma_seq0 RBRACKET { ListExp $2 }
  
;

exp_semicolon_exp:
  | expression { [bp $1 $startpos($1) $endpos($1)] }
  | expression SEMICOLON exp_semicolon_exp { bp $1 $startpos($1) $endpos($1) :: $3 }
;

exp_comma_seq0:
  | exp_comma_seq1 { $1 }
  | { [] }
;

exp_comma_seq1:
  | expression "," exp_comma_seq1 { bp $1 $startpos($1) $endpos($1) :: $3 }
  | expression { [bp $1 $startpos($1) $endpos($1)] }
;

exp_comma_seq2:
  | expression "," exp_comma_seq1 { bp $1 $startpos($1) $endpos($1) :: $3 }
;

exp_semicolon_seq2:
  | expression SEMICOLON exp_semicolon_seq2 { bp $1 $startpos($1) $endpos($1) :: $3 }
  | expression SEMICOLON expression { [bp $1 $startpos($1) $endpos($1); bp $3 $startpos($3) $endpos($3)] }
;

exprow_opt:
  | exprow_list { $1 }
  | { [] }
;

exprow_list:
  | exprow comma_exprow_opt { bp $1 $startpos($1) $endpos($1) :: $2 }
;

exprow:
  | lab EQUAL expression { Row (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), None) }
;

comma_exprow_opt:
  | "," exprow_list { $2 }
  | { [] }
;

match_clause:
  | p=pat BIGARROW e=expression BAR rest=match_clause { Case (bp p $startpos(p) $endpos(p), bp e $startpos(e) $endpos(e), Some (bp rest $startpos(rest) $endpos(rest))) }
  | p=pat BIGARROW e=expression { Case (bp p $startpos(p) $endpos(p), bp e $startpos(e) $endpos(e), None) }
;

(* ========================================================================= *)
(* Patterns                                                                  *)
(* ========================================================================= *)

pat:
  | atomic_pat_seq1 {
      match $1 with
      | [p] -> p.value
      | op :: args ->
          List.fold_left (fun acc arg -> PatApp (
            (match acc with
             | PatIdx w -> w
             | PatApp (w, _) -> w
             | _ -> failwith "invalid pattern application"), arg)) op.value args

      | _ -> failwith "impossible: invalid pattern sequence"
    }
  | p0=pat BAR p1=pat {
      PatOr (bp p0 $startpos(p0) $endpos(p0), bp p1 $startpos(p1) $endpos(p1))
    }
  | pat COLON typ { PatTyp (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3)) }
  | pat CONS pat { PatInfix (bp $1 $startpos($1) $endpos($1), b (IdxIdx (b "::")), bp $3 $startpos($3) $endpos($3)) }
  | pat "as" pat {
      match $1 with
      | PatIdx op -> PatAs (op, None, bp $3 $startpos($3) $endpos($3))
      | PatTyp (p, ty) -> (match p.value with PatIdx op -> PatAs (op, Some ty, bp $3 $startpos($3) $endpos($3)) | _ -> failwith "invalid layered pattern")
      | _ -> failwith "invalid layered pattern"
    }
;

atomic_pat_seq1:
  | atomic_pat atomic_pat_seq1 { bp $1 $startpos($1) $endpos($1) :: $2 }
  | atomic_pat { [bp $1 $startpos($1) $endpos($1)] }
;

atomic_pat:
  | UNDERSCORE { PatWildcard }
  | scon { PatCon (bp $1 $startpos($1) $endpos($1)) }
  | op_longid { PatIdx (bp $1 $startpos($1) $endpos($1)) }
  | LBRACE patrow_opt RBRACE { PatRecord $2 }
  | "(" pat ")" { PatParen (bp $2 $startpos($2) $endpos($2)) }
  | "(" ")" { PatTuple [] }
  | "(" pat_comma_seq2 ")" { PatTuple $2 }
  | HASH_OPEN pat_comma_seq0 RBRACKET { PatArray $2 }
  | LBRACKET pat_comma_seq0 RBRACKET { PatList $2 }
;

patrow_opt:
  | patrow { $1 }
  | { [] }
;

patrow:
  | ELLIPSIS { [bp PatRowPoly $startpos $endpos] }
  | lab EQUAL pat comma_patrow_opt { bp (PatRowSimple (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), match $4 with [] -> b PatRowPoly | h :: _ -> h)) $startpos $endpos :: $4 }
  | ident colon_typ_opt as_pat_opt comma_patrow_opt {
      bp (PatRowVar (bp $1 $startpos($1) $endpos($1), $2, None, match $4 with [] -> None | _ -> Some (List.hd $4))) $startpos $endpos :: $4
    }
;

as_pat_opt:
  | "as" pat { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

comma_patrow_opt:
  | "," patrow { $2 }
  | { [] }
;

pat_comma_seq0:
  | pat_comma_seq1 { $1 }
  | { [] }
;

pat_comma_seq1:
  | pat "," pat_comma_seq1 { bp $1 $startpos($1) $endpos($1) :: $3 }
  | pat { [bp $1 $startpos($1) $endpos($1)] }
;

pat_comma_seq2:
  | pat "," pat_comma_seq1 { bp $1 $startpos($1) $endpos($1) :: $3 }
;

(* ========================================================================= *)
(* Declarations                                                              *)
(* ========================================================================= *)

dec_seq:
  | kwdec SEMICOLON* dec_seq { SeqDec [bp $1 $startpos($1) $endpos($1); bp $3 $startpos($3) $endpos($3)] }
  | expression SEMICOLON+ dec_seq { SeqDec [bp (ExpDec (bp $1 $startpos($1) $endpos($1))) $startpos($1) $endpos($1); bp $3 $startpos($3) $endpos($3)] }
  | expression { SeqDec [bp (ExpDec (bp $1 $startpos($1) $endpos($1))) $startpos($1) $endpos($1)] }
  | { SeqDec [] }
;

(* Non-empty declaration sequence - requires at least one declaration *)
nonempty_dec_seq:
  | kwdec SEMICOLON* dec_seq { SeqDec [bp $1 $startpos($1) $endpos($1); bp $3 $startpos($3) $endpos($3)] }
  | expression SEMICOLON+ dec_seq { SeqDec [bp (ExpDec (bp $1 $startpos($1) $endpos($1))) $startpos($1) $endpos($1); bp $3 $startpos($3) $endpos($3)] }
  | expression { SeqDec [bp (ExpDec (bp $1 $startpos($1) $endpos($1))) $startpos($1) $endpos($1)] }
;

kwdec_seq:
  | kwdec SEMICOLON* kwdec_seq { bp $1 $startpos($1) $endpos($1) :: $3 }
  | { [] }
;

kwcoredec_seq:
  | kwcoredec SEMICOLON* kwcoredec_seq { bp $1 $startpos($1) $endpos($1) :: $3 }
  | { [] }
;

kwdec:
  | kwcoredec { $1 }
  | kwmoduledec { $1 }
;

kwmoduledec:
  | "structure" strbind { StrDec (bp $2 $startpos($2) $endpos($2)) }
;

kwcoredec:
  | "val" valbind { ValDec ([], bp $2 $startpos($2) $endpos($2)) }
  | "val" tyvarseq1 valbind { ValDec ($2, bp $3 $startpos($3) $endpos($3)) }
  | "fun" funbind { FunDec (bp $2 $startpos($2) $endpos($2)) }
  | "fun" tyvarseq1 funbind { FunDec (bp $3 $startpos($3) $endpos($3)) }
  | "type" typbind { TypDec (bp $2 $startpos($2) $endpos($2)) }
  | "datatype" datbind_0 typbind_opt { DatDec (bp $2 $startpos($2) $endpos($2), $3) }
  | "datatype" datbind_n typbind_opt { DatDec (bp $2 $startpos($2) $endpos($2), $3) }
  | "datatype" tycon EQUAL "datatype" long_tycon { DataDecAlias (bp $2 $startpos($2) $endpos($2), bp $5 $startpos($5) $endpos($5)) }
  | "abstype" datbind typbind_opt "with" dec_seq "end" { AbstractDec (bp $2 $startpos($2) $endpos($2), $3, [bp $5 $startpos($5) $endpos($5)]) }
  | "exception" exnbind { ExnDec (bp $2 $startpos($2) $endpos($2)) }
  | "local" dec_seq "in" dec_seq "end" { LocalDec (bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4)) }
  | "open" longid_seq1 { OpenDec $2 }
  | "infix" digit_opt eq_ident_seq1 { FixityDec (bp (Infix (b $2)) $startpos $endpos, $3) }
  | "infixr" digit_opt eq_ident_seq1 { FixityDec (bp (Infixr (b $2)) $startpos $endpos, $3) }
  | "nonfix" eq_ident_seq1 { FixityDec (bp Nonfix $startpos $endpos, $2) }
;

typbind_opt:
  | "withtype" typbind { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

valbind:
  | pat EQUAL expression and_valbind_opt { ValBind (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), $4) }
  | "rec" valbind { ValBindRec (bp $2 $startpos($2) $endpos($2)) }
;

and_valbind_opt:
  | "and" valbind { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

funbind:
  | funmatch and_funbind_opt { FunBind (bp $1 $startpos($1) $endpos($1), $2) }
;

and_funbind_opt:
  | "and" funbind { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

funmatch:
  | op_ident atomic_pat_seq1 colon_typ_opt EQUAL expression bar_funmatch_opt {
      FunMatchPrefix (bp $1 $startpos($1) $endpos($1), $2, $3, bp $5 $startpos($5) $endpos($5), $6)
    }
  | atomic_pat ident atomic_pat colon_typ_opt EQUAL expression bar_funmatch_opt {
      FunMatchInfix (bp $1 $startpos($1) $endpos($1), bp $2 $startpos($2) $endpos($2), bp $3 $startpos($3) $endpos($3), $4, bp $6 $startpos($6) $endpos($6), $7)
    }
  | "(" atomic_pat ident atomic_pat ")" atomic_pat_seq1 colon_typ_opt EQUAL expression bar_funmatch_opt {
      FunMatchLow (bp $2 $startpos($2) $endpos($2), bp $3 $startpos($3) $endpos($3), bp $4 $startpos($4) $endpos($4), $6, $7, bp $9 $startpos($9) $endpos($9), $10)
    }
;

bar_funmatch_opt:
  | BAR funmatch { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

colon_typ_opt:
  | COLON typ { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

of_typ_opt:
  | "of" typ { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

typbind:
  | tyvarseq tycon EQUAL typ and_typbind_opt { TypBind ($1, bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4), $5) }
;

and_typbind_opt:
  | "and" typbind { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

datbind:
  | tyvarseq tycon EQUAL conbind and_datbind_opt { DatBind ($1, bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4), $5) }
;

datbind_0:
  | tycon EQUAL conbind and_datbind_opt { DatBind ([], bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), $4) }
;

datbind_n:
  | tyvarseq1 tycon EQUAL conbind and_datbind_opt { DatBind ($1, bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4), $5) }
;

and_datbind_opt:
  | "and" datbind { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

conbind:
  | op_ident of_typ_opt bar_conbind_opt {
      let id = match $1 with WithOp i -> i | WithoutOp i -> i in
      ConBind (id, $2, $3)
    }
;

bar_conbind_opt:
  | BAR conbind { Some (bp $2 $startpos($2) $endpos($2)) }
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
  | "and" exnbind { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

(* ========================================================================= *)
(* Structures                                                                *)
(* ========================================================================= *)

strbind:
  | modid sigconstraint_opt EQUAL structure and_strbind_opt {
      StrBind (bp $1 $startpos($1) $endpos($1), $2, bp $4 $startpos($4) $endpos($4), $5)
    }
;

and_strbind_opt:
  | "and" strbind { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

sigconstraint_opt:
  | anotate sig_expr { Some ((bp $1 $startpos($1) $endpos($1)), (bp $2 $startpos($2) $endpos($2))) }
  | { None }
;

anotate:
  | COLON { Transparent }
  | COLON_GT { Opaque }
;

structure:
  | atomic_str_seq1 {
      match $1 with
      | [s] -> s.value
      | f :: args -> List.fold_left (fun acc arg -> FunctorApp (
          (match acc with StrIdx i -> i | _ -> b (IdxIdx (b "?"))), arg)) f.value args
      | [] -> failwith "impossible: empty structure sequence"
    }
  | structure COLON sig_expr { AnotateStr (b (IdxIdx (b "?")), bp Transparent $startpos $endpos, bp $1 $startpos($1) $endpos($1)) }
  | structure COLON_GT sig_expr { AnotateStr (b (IdxIdx (b "?")), bp Opaque $startpos $endpos, bp $1 $startpos($1) $endpos($1)) }
;

atomic_str_seq1:
  | atomic_str atomic_str_seq1 { bp $1 $startpos($1) $endpos($1) :: $2 }
  | atomic_str { [bp $1 $startpos($1) $endpos($1)] }
;

atomic_str:
  | "struct" dec_seq "end" { StructStr (bp $2 $startpos($2) $endpos($2)) }
  | longid { StrIdx (bp $1 $startpos($1) $endpos($1)) }
  | "let" dec_seq "in" structure "end" { Ast.LocalDec (bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4)) }
  | "(" structure ")" { $2 }
  | "(" dec_seq ")" { StructStr (bp $2 $startpos($2) $endpos($2)) }
;

(* ========================================================================= *)
(* Signatures                                                                *)
(* ========================================================================= *)

sig_expr:

  | "sig" specification "end" { SignSig (flatten_spec_node (bp $2 $startpos($2) $endpos($2))) }
  | "sig" "end" { SignSig [] }
  | sigid { SignIdx (bp $1 $startpos($1) $endpos($1)) }
  | sig_expr "where" "type" typrefin { SignWhere (bp $1 $startpos($1) $endpos($1), bp $4 $startpos($4) $endpos($4)) }
  | "functor" "(" modid COLON sig_expr ")" "->" sig_expr {
      let param_spec = bp (SpecStr (bp (StrDesc (bp $3 $startpos($3) $endpos($3), bp $5 $startpos($5) $endpos($5), None)) $startpos($3) $endpos($5))) $startpos $endpos in
      let result_spec = bp (SpecInclude (bp $8 $startpos($8) $endpos($8))) $startpos($8) $endpos($8) in
      SignSig (flatten_spec_node (bp (SpecSeq (param_spec, result_spec)) $startpos $endpos))
    }
  | "functor" modid COLON sig_expr "->" sig_expr {
      let param_spec = bp (SpecStr (bp (StrDesc (bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4), None)) $startpos($2) $endpos($4))) $startpos $endpos in
      let result_spec = bp (SpecInclude (bp $6 $startpos($6) $endpos($6))) $startpos($6) $endpos($6) in
      SignSig (flatten_spec_node (bp (SpecSeq (param_spec, result_spec)) $startpos $endpos))
    }
;

typrefin:
  | tyvarseq long_tycon EQUAL typ and_typrefin_opt {
      TypRef ($1, bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4), match $5 with None -> None | Some r -> Some (bp $4 $startpos($4) $endpos($4), r))
    }
;

and_typrefin_opt:
  | "and" "type" typrefin { Some (bp $3 $startpos($3) $endpos($3)) }
  | { None }
;

sigbind:
  | sigid EQUAL sig_expr and_sigbind_opt { SignBind (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), $4) }
;

and_sigbind_opt:
  | "and" sigbind { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

(* ========================================================================= *)
(* Specifications                                                            *)
(* ========================================================================= *)

specification:
  | kwspec { $1 }
  | specification kwspec { SpecSeq (bp $1 $startpos($1) $endpos($1), bp $2 $startpos($2) $endpos($2)) }
  | specification "sharing" "type" longid_eq_seq { SpecSharingTyp (bp $1 $startpos($1) $endpos($1), $4) }
  | specification "sharing" longid_eq_seq { SpecSharingStr (bp $1 $startpos($1) $endpos($1), $3) }
  | specification SEMICOLON { $1 }
;

longid_eq_seq:
  | longid EQUAL longid_eq_seq { bp $1 $startpos($1) $endpos($1) :: $3 }
  | longid { [bp $1 $startpos($1) $endpos($1)] }
;

spec_seq:
  | spec_seq kwspec { bp $2 $startpos($2) $endpos($2) :: $1 }
  | spec_seq SEMICOLON { $1 }
  | { [] }
;

corespec_seq:
  | corespec_seq kwcorespec { bp $2 $startpos($2) $endpos($2) :: $1 }
  | corespec_seq SEMICOLON { $1 }
  | { [] }
;

kwspec:
  | kwcorespec { $1 }
  | kwmodulespec { $1 }
;

kwcorespec:
  | "val" tyvarseq valdesc { SpecVal (bp $3 $startpos($3) $endpos($3)) }
  | "type" typbind { SpecTypBind (bp $2 $startpos($2) $endpos($2)) }
  | "type" typdesc { SpecTyp (bp $2 $startpos($2) $endpos($2)) }
  | "eqtype" typdesc { SpecEqtyp (bp $2 $startpos($2) $endpos($2)) }
  | "datatype" datdesc_0 typbind_opt { SpecDat (bp $2 $startpos($2) $endpos($2)) }
  | "datatype" datdesc_n typbind_opt { SpecDat (bp $2 $startpos($2) $endpos($2)) }
  | "datatype" tycon EQUAL "datatype" long_tycon { SpecDatAlias (bp $2 $startpos($2) $endpos($2), bp $5 $startpos($5) $endpos($5)) }
  | "exception" exndesc { SpecExn (bp $2 $startpos($2) $endpos($2)) }
  | "local" specification "in" specification "end" { SpecSeq (bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4)) }
  | "open" longid_seq1 { SpecIncludeIdx $2 }
  | "infix" digit_opt eq_ident_seq1 { SpecSeq (b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None)))),
                                             b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None))))) }
  | "infixr" digit_opt eq_ident_seq1 { SpecSeq (b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None)))),
                                              b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None))))) }
  | "nonfix" eq_ident_seq1 { SpecSeq (b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None)))),
                                    b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None))))) }
;

kwmodulespec:
  | "structure" strdesc { SpecStr (bp $2 $startpos($2) $endpos($2)) }
  | "functor" fundesc { SpecStr (bp $2 $startpos($2) $endpos($2)) }
  | "include" sig_expr { SpecInclude (bp $2 $startpos($2) $endpos($2)) }
  | "include" sigid_seq2 { SpecIncludeIdx $2 }
  | "signature" sigbind { SpecSeq (b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None)))),
                                 b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None))))) }
;

sigid_seq2:
  | sigid sigid_seq2 { bp $1 $startpos($1) $endpos($1) :: $2 }
  | sigid sigid { [bp $1 $startpos($1) $endpos($1); bp $2 $startpos($2) $endpos($2)] }
;

valdesc:
  | op_ident COLON typ and_valdesc_opt {
      ValDesc ((match $1 with WithOp i -> i | WithoutOp i -> i), bp $3 $startpos($3) $endpos($3), $4)
    }
;

and_valdesc_opt:
  | "and" valdesc { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

typdesc:
  | tyvarseq tycon and_typdesc_opt { TypDesc ($1, bp $2 $startpos($2) $endpos($2), $3) }
;

and_typdesc_opt:
  | "and" typdesc { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

datdesc:
  | tyvarseq tycon EQUAL condesc and_datdesc_opt { DatDesc ($1, bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4), $5) }
;

datdesc_0:
  | tycon EQUAL condesc and_datdesc_opt { DatDesc ([], bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), $4) }
;

datdesc_n:
  | tyvarseq1 tycon EQUAL condesc and_datdesc_opt { DatDesc ($1, bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4), $5) }
;

and_datdesc_opt:
  | "and" datdesc { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

condesc:
  | op_ident of_typ_opt bar_condesc_opt {
      ConDesc ((match $1 with WithOp i -> i | WithoutOp i -> i), $2, $3)
    }
;
%inline
bar_condesc_opt:
  | BAR condesc { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

exndesc:
  | op_ident of_typ_opt and_exndesc_opt {
      ExnDesc ((match $1 with WithOp i -> i | WithoutOp i -> i), $2, $3)
    }
;
%inline
and_exndesc_opt:
  | "and" exndesc { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

strdesc:
  | modid COLON sig_expr and_strdesc_opt { StrDesc (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), $4) }
;
%inline
and_strdesc_opt:
  | "and" strdesc { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

fundesc:
  | modid fundesctail and_fundesc_opt { StrDesc (bp $1 $startpos($1) $endpos($1), bp $2 $startpos($2) $endpos($2), $3) }
;

fundesctail:
  | COLON sig_expr { $2 }
  | "(" modid COLON sig_expr ")" fundesctail {
      let param_spec = bp (SpecStr (bp (StrDesc (bp $2 $startpos($2) $endpos($2), bp $4 $startpos($4) $endpos($4), None)) $startpos($2) $endpos($4))) $startpos $endpos in
      let result_spec = bp (SpecInclude (bp $6 $startpos($6) $endpos($6))) $startpos($6) $endpos($6) in
      SignSig (flatten_spec_node (bp (SpecSeq (param_spec, result_spec)) $startpos $endpos))
    }
  | modid COLON sig_expr fundesctail {
      let param_spec = bp (SpecStr (bp (StrDesc (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), None)) $startpos($1) $endpos($3))) $startpos $endpos in
      let result_spec = bp (SpecInclude (bp $4 $startpos($4) $endpos($4))) $startpos($4) $endpos($4) in
      SignSig (flatten_spec_node (bp (SpecSeq (param_spec, result_spec)) $startpos $endpos))
    }
;
%inline
and_fundesc_opt:
  | "and" fundesc { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

(* ========================================================================= *)
(* Functors                                                                  *)
(* ========================================================================= *)

fctbind:
  | modid "(" modid COLON sig_expr ")" sigconstraint_opt EQUAL structure and_fctbind_opt {
      FctBind (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), bp $5 $startpos($5) $endpos($5), $7, bp $9 $startpos($9) $endpos($9), $10)
    }
  | modid "(" specification ")" sigconstraint_opt EQUAL structure and_fctbind_opt {
      FctBindOpen (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), $5, bp $7 $startpos($7) $endpos($7), $8)
    }
  | modid "(" ")" sigconstraint_opt EQUAL structure and_fctbind_opt {
      FctGen (bp $1 $startpos($1) $endpos($1), $4, bp $6 $startpos($6) $endpos($6), $7)
    }
;
%inline
and_fctbind_opt:
  | "and" fctbind { Some (bp $2 $startpos($2) $endpos($2)) }
  | { None }
;

(* ========================================================================= *)
(* Program (Top-level)                                                       *)
(* ========================================================================= *)

(* Program that can be empty - used for final position *)
program:
  | dec_seq { (ProgDec (bp $1 $startpos($1) $endpos($1))) }
  | "functor" fctbind { (ProgFun (bp $2 $startpos($2) $endpos($2))) }
  | "signature" sigbind { (ProgStr (bp $2 $startpos($2) $endpos($2))) }
;

(* Non-empty program - must start with a keyword *)
nonempty_program:
  | nonempty_dec_seq { (ProgDec (bp $1 $startpos($1) $endpos($1))) }
  | "functor" fctbind { (ProgFun (bp $2 $startpos($2) $endpos($2))) }
  | "signature" sigbind { (ProgStr (bp $2 $startpos($2) $endpos($2))) }
;

(* Program list - allows consecutive programs without semicolons *)
(* Each non-final program must be non-empty to break the cycle *)
program_list:
  | nonempty_program SEMICOLON? program_list %prec PROGRAM_SEP { ProgSeq (b $1, b $3) }
  (* | nonempty_program program_list { ProgSeq (b $1, b $2) } *)
  | program { $1 }
  ;

%inline
file:
  program_list { $1 }
  ;
main: 
  | file EOF { ($1, $2) }
