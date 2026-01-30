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

    (* Helper to unwrap with_op to get the inner idx node *)
    let unwrap_op = function
      | WithOp idx -> idx
      | WithoutOp idx -> idx

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
%nonassoc USE_OP 
%left PROGRAM_SEP
%left SEMICOLON
%right JUXTAPOSE
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
%right PREFIX_APP
%left INFIX_APP
%right ARROW
%nonassoc EQUAL
%right STAR
%left COLON COLON_GT

%left PROGRAM_PREC 

(* ========================================================================= *)
(* Type declarations for nonterminals                                        *)
(* ========================================================================= *)
%type <Ast.prog * string list> main
%type <Ast.prog> file
%type <Ast.prog> program nonempty_program
%type <Ast.declaration> dec_seq nonempty_dec_seq kwdec kwcoredec kwmoduledec
%type <Ast.declaration node list> kwdec_seq kwcoredec_seq
%type <Ast.expression> expression 
%type <Ast.expression> at_exp appexp infexp
%type <Ast.pat> pat atomic_pat
%type <Ast.pat node list> pat_comma_seq2
%type <Ast.typ> typ typ_sans_star atomic_typ
%type <Ast.typ node list> tuple_typ typ_comma_seq2
%type <Ast.matching> match_clause
%type <Ast.expression node list> exp_seq_list

%type <Ast.pat_row node list> patrow patrow_opt
%type <Ast.row node list> exp_row exp_row_opt
%type <Ast.typ_row node list> typrow typrow_opt
%type <Ast.value_binding> valbind
%type <Ast.function_binding> funbind
%type <Ast.fun_match> funmatch
%type <Ast.type_binding> typbind
%type <Ast.type_binding node option> typbind_opt
%type <Ast.data_binding> datbind datbind_0 datbind_n
%type <Ast.constructor_binding> conbind
%type <Ast.exn_bind> exnbind
%type <Ast.structure_binding> strbind
%type <Ast.structure> structure atomic_str
%type <Ast.signature> sig_expr
%type <Ast.signature_binding> sigbind
%type <Ast.functor_binding> fctbind
%type <Ast.specification> specification kwspec kwcorespec kwmodulespec
%type <Ast.specification node list> spec_seq corespec_seq
%type <Ast.val_specification> valdesc
%type <Ast.typ_specification> typdesc
%type <Ast.dat_specification> datdesc datdesc_0 datdesc_n
%type <Ast.con_specification> condesc
%type <Ast.exn_specification> exndesc
%type <Ast.str_specification> strdesc fundesc
%type <Ast.typ_refine> typrefin
%type <Ast.idx> ident longid tyvar tycon lab modid sigid
%type <Ast.idx node list> tyvarseq tyvarseq1 sigid_seq2
%type <Ast.constant> scon
%type <Ast.with_op> op_ident op_longid op_eq_ident
%type <Ast.anotate> anotate
%type <Ast.signature> fundesctail

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
  | m=main { m }
expression_top:
  | e=expression eof=EOF { e }
;
pat_top:
  | p=pat eof=EOF { p }
;
typ_top:
  | t=typ eof=EOF { t }
;
let boxed(r) :=
  | v=r; { { value = v; pos=Some ($symbolstartpos , $endpos ) } }

(* Helper for creating nodes with position info using bp *)
let located(X) :=
  | x=X; { bp x $startpos(x) $endpos(x) }

(* Helper for at-least-2 element lists *)
let list2(X) :=
  | x1=X; x2=X; { [x1; x2] }
  | x=X; rest=nonempty_list(X); { x :: rest }

(* ========================================================================= *)
(* Identifier Rules                                                          *)
(* ========================================================================= *)
(*
   SML identifiers are organized in layers, from most basic to most permissive:

   LAYER 0: Atomic tokens
   ├─ tyvar              Type variables: 'a, 'b, ''a
   └─ lifted_symbol      Symbols used as regular identifiers: "op +" or "(+)"

   LAYER 1: Base identifiers (produce idx)
   ├─ ident              Standard identifier: foo, +, *
   │                     Used for: value names, function names, most contexts
   └─ tycon              Type constructor: int, list (no * since it means tuple)
                         Used for: type definitions, type applications

   LAYER 2: Extended base identifiers (produce idx)
   ├─ eq_ident           ident + "=" symbol
   │                     Used for: contexts where = is a valid identifier
   └─ any_ident          eq_ident + bare SYMBOL_IDENT (boxed)
                         Used for: fixity declarations (infix, infixr, nonfix)

   LAYER 3: Qualified identifiers (produce idx)
   ├─ longid             Qualified paths: A.B.c, foo
   │                     Used for: expressions, structures
   └─ long_tycon         Qualified type constructors: A.B.t, int
                         Used for: type expressions

   LAYER 4: "op"-wrapped identifiers (produce with_op)
   ├─ op_ident           Short ident with optional "op": foo, op +, ::
   │                     Used for: function bindings, constructor bindings
   ├─ op_longid          Long ident with optional "op" + bare symbols
   │                     Used for: patterns, expressions with operator context
   └─ op_eq_ident        op_longid + "=" and "::" (most permissive)
                         Used for: exception aliases, pattern matching

   LAYER 5: Context-specific aliases
   ├─ modid              Module identifier (= ident)
   ├─ sigid              Signature identifier (= ident)
   └─ lab                Record label: field names or numeric (#1, #2)
*)

(* ------------------------------------------------------------------------- *)
(* Layer 0: Atomic token conversions                                         *)
(* ------------------------------------------------------------------------- *)

(* Type variables: 'a, 'b, ''a (equality type variables) *)
%inline tyvar:
  | tv=TYVAR { IdxVar (b tv) }
;

(* Lift symbolic identifiers to be usable in regular identifier positions.
   SML allows this via "op" prefix or parenthesization: "op +" or "(+)" *)
%inline lifted_symbol:
  | "op" sym=SYMBOL_IDENT { sym }
  | "(" sym=SYMBOL_IDENT ")" { sym }
;

(* ------------------------------------------------------------------------- *)
(* Layer 1: Base identifiers                                                 *)
(* ------------------------------------------------------------------------- *)

(* Standard identifier - the most common form.
   Includes: alphanumeric (foo), lifted symbols (op +), and * (for signatures) *)
%inline ident:
  | id=SHORT_IDENT { ident_to_idx id }
  | sym=lifted_symbol { ident_to_idx sym }
  | STAR { IdxIdx (b "*") }
;

(* Type constructor - like ident but WITHOUT * (since * means tuple type).
   Examples: int, list, option *)
%inline tycon:
  | id=SHORT_IDENT { ident_to_idx id }
  | sym=lifted_symbol { ident_to_idx sym }
;

(* ------------------------------------------------------------------------- *)
(* Layer 2: Extended base identifiers                                        *)
(* ------------------------------------------------------------------------- *)

(* ident extended with "=" as a valid identifier.
   Needed for contexts where = can be used as a value name *)
%inline eq_ident:
  | id=ident { id }
  | EQUAL { IdxIdx (b "=") }
;

(* Most permissive base identifier - includes bare symbolic identifiers.
   Used for fixity declarations: infix 5 ++, infixr 3 @@ *)
any_ident:
  | id=eq_ident { b id }
  | sym=SYMBOL_IDENT %prec USE_OP { b (ident_to_idx sym) }
;

(* ------------------------------------------------------------------------- *)
(* Layer 3: Qualified (long) identifiers                                     *)
(* ------------------------------------------------------------------------- *)

(* Qualified identifier path: A.B.c or simple foo *)
%inline longid:
  | id=ident { id }
  | ids=LONG_IDENT { idents_to_long_idx ids }
;

(* Qualified type constructor path: A.B.t or simple int *)
%inline long_tycon:
  | tc=tycon { tc }
  | ids=LONG_IDENT { idents_to_long_idx ids }
;

(* ------------------------------------------------------------------------- *)
(* Layer 4: "op"-wrapped identifiers (produce with_op)                       *)
(* ------------------------------------------------------------------------- *)

(* Short identifier with optional "op" prefix.
   Used for: function names in fun bindings, constructor names.
   Also accepts :: since it's a built-in constructor. *)
op_ident:
  | id=ident { WithoutOp (b id) }
  | "op" id=ident { WithOp (b id) }
  | CONS { WithoutOp (b (IdxIdx (b "::"))) }
;

(* Long identifier with optional "op" prefix.
   Also accepts bare SYMBOL_IDENT (used as infix operator in pattern context) *)
op_longid:
  | lid=longid { WithoutOp (b lid) }
  | "op" lid=longid { WithOp (b lid) }
  | sym=SYMBOL_IDENT %prec USE_OP { WithoutOp (b (ident_to_idx sym)) }
;

(* Most permissive "op"-wrapped identifier.
   Extends op_longid with "=" and "::" (with optional "op").
   Used for: exception rebindings, pattern matching contexts *)
op_eq_ident:
  | olid=op_longid { olid }
  | EQUAL { WithoutOp (b (IdxIdx (b "="))) }
  | "op" EQUAL { WithOp (b (IdxIdx (b "="))) }
  | CONS { WithoutOp (b (IdxIdx (b "::"))) }
  | "op" CONS { WithOp (b (IdxIdx (b "::"))) }
;

(* ------------------------------------------------------------------------- *)
(* Layer 5: Context-specific aliases                                         *)
(* ------------------------------------------------------------------------- *)

(* Module identifier - semantic alias for documentation *)
%inline modid:
  | id=ident { id }
;

(* Signature identifier - semantic alias for documentation *)
%inline sigid:
  | id=ident { id }
;

(* Record label - can be alphanumeric (name) or numeric (#1, #2 for tuples) *)
lab:
  | id=ident { id }
  | lit=INT_LIT { IdxNum (b lit) }
;

(* ========================================================================= *)
(* Constants                                                                 *)
(* ========================================================================= *)

scon:
  | lit=INT_LIT { ConInt (b lit) }
  | lit=HEX_LIT { ConWord (b lit) }
  | lit=FLOAT_LIT { ConFloat (b lit) }
  | lit=CHAR_LIT { ConChar (b lit) }
  | lit=STRING_LIT { ConString (b lit) }
;

(* ========================================================================= *)
(* Type Variables                                                            *)
(* ========================================================================= *)

tyvarseq:
  | seq=tyvarseq1 { seq }
  | { [] }
;

tyvarseq1:
  | tv=tyvar { [b tv] }
  | "(" seq=separated_nonempty_list(",", boxed(tyvar)) ")" { seq }
;

(* ========================================================================= *)
(* Types                                                                     *)
(* ========================================================================= *)

typ:
  | tuple=tuple_typ "->" result=typ {
      let src = if List.length tuple = 1 then List.hd tuple else bp (TypTuple tuple) $startpos(tuple) $endpos(tuple) in
      TypFun (src, bp result $startpos(result) $endpos(result))
    }
  | tuple=tuple_typ {
      if List.length tuple = 1 then (List.hd tuple).value else TypTuple tuple
    }
;

tuple_typ:
  | t=typ_sans_star { [bp t $startpos(t) $endpos(t)] }
  | t=typ_sans_star STAR rest=tuple_typ { bp t $startpos(t) $endpos(t) :: rest }
;

typ_sans_star:
  | "(" args=typ_comma_seq2 ")" tc=long_tycon { TypCon (args, bp tc $startpos(tc) $endpos(tc)) }
  | arg=typ_sans_star tc=long_tycon { TypCon ([bp arg $startpos(arg) $endpos(arg)], bp tc $startpos(tc) $endpos(tc)) }
  | t=atomic_typ { t }
;

atomic_typ:
  | tc=long_tycon { TypCon ([], bp tc $startpos(tc) $endpos(tc)) }
  | tv=tyvar { TypVar (bp tv $startpos(tv) $endpos(tv)) }
  | LBRACE rows=typrow_opt RBRACE { TypRecord rows }
  | "(" t=typ ")" { TypPar (bp t $startpos(t) $endpos(t)) }
;

typ_comma_seq2:
  | t=typ "," rest=separated_nonempty_list(",", located(typ)) { bp t $startpos(t) $endpos(t) :: rest }
;

typrow_opt:
  | rows=typrow { rows }
  | { [] }
;

typrow:
  | l=lab COLON t=typ rest=option(preceded(",", typrow)) { 
      bp (TypRow (bp l $startpos(l) $endpos(l), bp t $startpos(t) $endpos(t), None)) $startpos $endpos 
      :: (match rest with Some r -> r | None -> []) 
    }
;

(* ========================================================================= *)
(* Expressions                                                               *)
(* ========================================================================= *)
/*

atexp ::= scon special constant
  〈op〉longvid value identifier
  { 〈exprow 〉 } record
  # lab record selector
  () 0-tuple
  (exp1 , ··· , expn) n-tuple, n ≥ 2
  [exp1 , ··· , expn] list, n ≥ 0
  (exp1 ; ··· ; expn) sequence, n ≥ 2
  let dec in exp1 ; ··· ; expn end local declaration, n ≥ 1
  ( exp )
exprow ::= lab = exp 〈 , exprow〉 expression row
appexp ::= atexp
  appexp atexp application expression
infexp ::= appexp
  infexp1 vid infexp2 infix expression
exp ::= infexp
  exp : ty typed (L)
  exp1 andalso exp2 conjunction
  exp1 orelse exp2 disjunction
  exp handle match handle exception
  raise exp raise exception
  if exp1 then exp2 else exp3 conditional
  while exp1 do exp2 iteration
  case exp of match case analysis
  fn match function
*/
at_exp:
  | c=boxed(scon) { ExpCon c }
  | id=op_longid { ExpIdx (unwrap_op id) }
  | "{" rows=exp_row_opt "}" { RecordExp rows }
  | "#" lab=boxed(lab) { RecordSelector lab }
  | "(" ")" { TupleExp [] }
  | "(" seq_start=boxed(expression) "," seq=separated_nonempty_list(",", boxed(expression)) ")" { TupleExp (seq_start :: seq) }
  | "[" seq=separated_list(",", boxed(expression)) "]" { ListExp seq }
  | "(" e=exp_seq_list ")" { SeqExp e }
  | "let" dec=boxed(dec_seq) "in" exp=exp_seq_list "end" { LetExp ([dec], exp) }
  | "(" e=boxed(expression) ")" { ParenExp e }
 ;


exp_seq_list: 
  | e=boxed(expression) { [e] }
  | e=boxed(expression) ";" rest=exp_seq_list { e :: rest }
  ;

%inline
exp_seq: 
  | e=expression { e }
  | e=boxed(expression) ";" rest=boxed(exp_seq) { SeqExp [e; rest] }
  ;

exp_row_opt:
  | rows=exp_row { rows }
  | { [] }
  ;

exp_row: 
  | l=boxed(lab) "=" e=boxed(expression) rest=option(preceded(",", exp_row)) {
      bp (Row (l, e, None)) $startpos $endpos :: (match rest with Some r -> r | None -> [])
    }
  ;

appexp:
  | a=at_exp { a }
  | f=appexp a=at_exp { ExpApp (bp f $startpos(f) $endpos(f), bp a $startpos(a) $endpos(a)) }
  ;

infexp: 
  | a=appexp { a }
  | a1=infexp op=SYMBOL_IDENT a2=appexp %prec INFIX_APP {
      InfixApp (bp a1 $startpos(a1) $endpos(a1), b (ident_to_idx op), bp a2 $startpos(a2) $endpos(a2))
    }
  | a1=infexp CONS a2=appexp %prec INFIX_APP {
      InfixApp (bp a1 $startpos(a1) $endpos(a1), b (IdxIdx (b "::")), bp a2 $startpos(a2) $endpos(a2))
    }
  | a1=infexp EQUAL a2=appexp %prec INFIX_APP {
      InfixApp (bp a1 $startpos(a1) $endpos(a1), b (IdxIdx (b "=")), bp a2 $startpos(a2) $endpos(a2))
    }
  ;

expression:
  | e=infexp { e }
  | e=expression ":" ty=typ { TypedExp (bp e $startpos(e) $endpos(e), bp ty $startpos(ty) $endpos(ty)) }
  | e1=boxed(expression) ANDALSO e2=boxed(expression) %prec ANDALSO { AndExp (e1, e2) }
  | e1=boxed(expression) ORELSE e2=boxed(expression) %prec ORELSE { OrExp (e1, e2) }
  | e=boxed(expression) HANDLE m=match_clause %prec HANDLE { HandleExp (e, bp m $startpos(m) $endpos(m)) }
  | RAISE e=boxed(expression) %prec RAISE { RaiseExp e }
  | IF e1=boxed(expression) THEN e2=boxed(expression) ELSE e3=boxed(expression) { IfExp (e1, e2, e3) }
  | WHILE e1=boxed(expression) DO e2=boxed(expression) { WhileExp (e1, e2) }
  | CASE e=boxed(expression) OF m=match_clause { CaseExp (e, bp m $startpos(m) $endpos(m)) }
  | FN m=match_clause { FnExp (bp m $startpos(m) $endpos(m)) }
;

match_clause:
  | p=pat BIGARROW e=expression BAR rest=match_clause { Case (bp p $startpos(p) $endpos(p), bp e $startpos(e) $endpos(e), Some (bp rest $startpos(rest) $endpos(rest))) }
  | p=pat BIGARROW e=expression { Case (bp p $startpos(p) $endpos(p), bp e $startpos(e) $endpos(e), None) }
;

(* ========================================================================= *)
(* Patterns                                                                  *)
(* ========================================================================= *)

pat:
  | seq=nonempty_list(located(atomic_pat)) {
      match seq with
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
  | p=pat COLON t=typ { PatTyp (bp p $startpos(p) $endpos(p), bp t $startpos(t) $endpos(t)) }

  | p1=pat "as" p2=pat {
      match p1 with
      | PatIdx op -> PatAs (op, None, bp p2 $startpos(p2) $endpos(p2))
      | PatTyp (p, ty) -> (match p.value with PatIdx op -> PatAs (op, Some ty, bp p2 $startpos(p2) $endpos(p2)) | _ -> failwith "invalid layered pattern")
      | _ -> failwith "invalid layered pattern"
    }
   //  | sym=SYMBOL_IDENT p=pat %prec PREFIX_APP { PatApp (b (WithoutOp (b (IdxIdx (b (match sym with Symbol s -> s | Name s -> s))))), bp p $startpos(p) $endpos(p)) }
;

atomic_pat:
  | UNDERSCORE { PatWildcard }
  | c=scon { PatCon (bp c $startpos(c) $endpos(c)) }
  | id=op_longid { PatIdx (bp id $startpos(id) $endpos(id)) }
  | LBRACE rows=patrow_opt RBRACE { PatRecord rows }
  | "(" p=pat ")" { PatParen (bp p $startpos(p) $endpos(p)) }
  | "(" ")" { PatTuple [] }
  | "(" seq=pat_comma_seq2 ")" { PatTuple seq }
  | HASH_OPEN pats=separated_list(",", located(pat)) RBRACKET { PatArray pats }
  | LBRACKET pats=separated_list(",", located(pat)) RBRACKET { PatList pats }
;

patrow_opt:
  | rows=patrow { rows }
  | { [] }
;

patrow:
  | ELLIPSIS { [bp PatRowPoly $startpos $endpos] }
  | l=lab EQUAL p=pat rest=option(preceded(",", patrow)) { 
      let rest_list = match rest with Some r -> r | None -> [] in
      bp (PatRowSimple (bp l $startpos(l) $endpos(l), bp p $startpos(p) $endpos(p), match rest_list with [] -> b PatRowPoly | h :: _ -> h)) $startpos $endpos :: rest_list 
    }
  | id=ident ty=option(preceded(COLON, located(typ))) aspat=option(preceded("as", located(pat))) rest=option(preceded(",", patrow)) {
      let rest_list = match rest with Some r -> r | None -> [] in
      bp (PatRowVar (bp id $startpos(id) $endpos(id), ty, None, match rest_list with [] -> None | _ -> Some (List.hd rest_list))) $startpos $endpos :: rest_list
    }
;

pat_comma_seq2:
  | p=pat "," rest=separated_nonempty_list(",", located(pat)) { bp p $startpos(p) $endpos(p) :: rest }
;

(* ========================================================================= *)
(* Declarations                                                              *)
(* ========================================================================= *)

dec_seq:
  | d=kwdec SEMICOLON* rest=dec_seq { SeqDec [bp d $startpos(d) $endpos(d); bp rest $startpos(rest) $endpos(rest)] }
  | e=expression SEMICOLON+ rest=dec_seq { SeqDec [bp (ExpDec (bp e $startpos(e) $endpos(e))) $startpos(e) $endpos(e); bp rest $startpos(rest) $endpos(rest)] }
  | e=expression %prec PROGRAM_SEP { SeqDec [bp (ExpDec (bp e $startpos(e) $endpos(e))) $startpos(e) $endpos(e)] }
  | { SeqDec [] }
;

(* Non-empty declaration sequence - requires at least one declaration *)
nonempty_dec_seq:
  | d=kwdec SEMICOLON* rest=dec_seq { SeqDec [bp d $startpos(d) $endpos(d); bp rest $startpos(rest) $endpos(rest)] }
  | e=expression SEMICOLON+ rest=dec_seq { SeqDec [bp (ExpDec (bp e $startpos(e) $endpos(e))) $startpos(e) $endpos(e); bp rest $startpos(rest) $endpos(rest)] }
  | e=expression { SeqDec [bp (ExpDec (bp e $startpos(e) $endpos(e))) $startpos(e) $endpos(e)] }
;

kwdec_seq:
  | d=kwdec SEMICOLON* rest=kwdec_seq { bp d $startpos(d) $endpos(d) :: rest }
  | { [] }
;

kwcoredec_seq:
  | d=kwcoredec SEMICOLON* rest=kwcoredec_seq { bp d $startpos(d) $endpos(d) :: rest }
  | { [] }
;

kwdec:
  | d=kwcoredec { d }
  | d=kwmoduledec { d }
;

kwmoduledec:
  | "structure" bind=strbind { StrDec (bp bind $startpos(bind) $endpos(bind)) }
;

kwcoredec:
  | "val" bind=valbind { ValDec ([], bp bind $startpos(bind) $endpos(bind)) }
  | "val" tvs=tyvarseq1 bind=valbind { ValDec (tvs, bp bind $startpos(bind) $endpos(bind)) }
  | "fun" bind=funbind { FunDec (bp bind $startpos(bind) $endpos(bind)) }
  | "fun" tvs=tyvarseq1 bind=funbind { FunDec (bp bind $startpos(bind) $endpos(bind)) }
  | "type" bind=typbind { TypDec (bp bind $startpos(bind) $endpos(bind)) }
  | "datatype" bind=datbind_0 wt=typbind_opt { DatDec (bp bind $startpos(bind) $endpos(bind), wt) }
  | "datatype" bind=datbind_n wt=typbind_opt { DatDec (bp bind $startpos(bind) $endpos(bind), wt) }
  | "datatype" tc=tycon EQUAL "datatype" ltc=long_tycon { DataDecAlias (bp tc $startpos(tc) $endpos(tc), bp ltc $startpos(ltc) $endpos(ltc)) }
  | "abstype" bind=datbind wt=typbind_opt "with" decs=dec_seq "end" { AbstractDec (bp bind $startpos(bind) $endpos(bind), wt, [bp decs $startpos(decs) $endpos(decs)]) }
  | "exception" bind=exnbind { ExnDec (bp bind $startpos(bind) $endpos(bind)) }
  | "local" d1=dec_seq "in" d2=dec_seq "end" { LocalDec (bp d1 $startpos(d1) $endpos(d1), bp d2 $startpos(d2) $endpos(d2)) }
  | "open" ids=nonempty_list(boxed(longid)) { OpenDec ids }
  | "infix" n=option(INT_LIT) ids=nonempty_list(any_ident) { FixityDec (bp (Infix (b (match n with Some lit -> int_of_string lit | None -> 0))) $startpos $endpos, ids) }
  | "infixr" n=option(INT_LIT) ids=nonempty_list(any_ident) { FixityDec (bp (Infixr (b (match n with Some lit -> int_of_string lit | None -> 0))) $startpos $endpos, ids) }
  | "nonfix" ids=nonempty_list(any_ident) { FixityDec (bp Nonfix $startpos $endpos, ids) }
;

typbind_opt:
  | "withtype" bind=typbind { Some (bp bind $startpos(bind) $endpos(bind)) }
  | { None }
;

valbind:
  | p=pat EQUAL e=expression rest=option(preceded("and", located(valbind))) { ValBind (bp p $startpos(p) $endpos(p), bp e $startpos(e) $endpos(e), rest) }
  | "rec" bind=valbind { ValBindRec (bp bind $startpos(bind) $endpos(bind)) }
;

funbind:
  | m=funmatch rest=option(preceded("and", located(funbind))) { FunBind (bp m $startpos(m) $endpos(m), rest) }
;

funmatch:
  | id=op_ident pats=nonempty_list(located(atomic_pat)) ty=option(preceded(COLON, located(typ))) EQUAL e=expression rest=option(preceded(BAR, located(funmatch))) {
      FunMatchPrefix (bp id $startpos(id) $endpos(id), pats, ty, bp e $startpos(e) $endpos(e), rest)
    }
  | p1=atomic_pat id=ident p2=atomic_pat ty=option(preceded(COLON, located(typ))) EQUAL e=expression rest=option(preceded(BAR, located(funmatch))) {
      FunMatchInfix (bp p1 $startpos(p1) $endpos(p1), bp id $startpos(id) $endpos(id), bp p2 $startpos(p2) $endpos(p2), ty, bp e $startpos(e) $endpos(e), rest)
    }
  | "(" p1=atomic_pat id=ident p2=atomic_pat ")" pats=nonempty_list(located(atomic_pat)) ty=option(preceded(COLON, located(typ))) EQUAL e=expression rest=option(preceded(BAR, located(funmatch))) {
      FunMatchLow (bp p1 $startpos(p1) $endpos(p1), bp id $startpos(id) $endpos(id), bp p2 $startpos(p2) $endpos(p2), pats, ty, bp e $startpos(e) $endpos(e), rest)
    }
;

typbind:
  | tvs=tyvarseq tc=tycon EQUAL t=typ rest=option(preceded("and", located(typbind))) { TypBind (tvs, bp tc $startpos(tc) $endpos(tc), bp t $startpos(t) $endpos(t), rest) }
;

datbind:
  | tvs=tyvarseq tc=tycon EQUAL cons=conbind rest=option(preceded("and", located(datbind))) { DatBind (tvs, bp tc $startpos(tc) $endpos(tc), bp cons $startpos(cons) $endpos(cons), rest) }
;

datbind_0:
  | tc=tycon EQUAL cons=conbind rest=option(preceded("and", located(datbind))) { DatBind ([], bp tc $startpos(tc) $endpos(tc), bp cons $startpos(cons) $endpos(cons), rest) }
;

datbind_n:
  | tvs=tyvarseq1 tc=tycon EQUAL cons=conbind rest=option(preceded("and", located(datbind))) { DatBind (tvs, bp tc $startpos(tc) $endpos(tc), bp cons $startpos(cons) $endpos(cons), rest) }
;

conbind:
  | id=op_ident ty=option(preceded("of", located(typ))) rest=option(preceded(BAR, located(conbind))) {
      let id = match id with WithOp i -> i | WithoutOp i -> i in
      ConBind (id, ty, rest)
    }
;

exnbind:
  | id=op_ident ty=option(preceded("of", located(typ))) rest=option(preceded("and", located(exnbind))) {
      let id = match id with WithOp i -> i | WithoutOp i -> i in
      ExnBind (id, ty, rest)
    }
  | id1=op_ident EQUAL id2=op_eq_ident rest=option(preceded("and", located(exnbind))) {
      let id1 = match id1 with WithOp i -> i | WithoutOp i -> i in
      let id2 = match id2 with WithOp i -> i | WithoutOp i -> i in
      ExnBindAlias (id1, id2, rest)
    }
;

(* ========================================================================= *)
(* Structures                                                                *)
(* ========================================================================= *)

strbind:
  | id=modid sign=option(pair(located(anotate), located(sig_expr))) EQUAL str=structure rest=option(preceded("and", located(strbind))) {
      StrBind (bp id $startpos(id) $endpos(id), sign, bp str $startpos(str) $endpos(str), rest)
    }
;

anotate:
  | COLON { Transparent }
  | COLON_GT { Opaque }
;

structure:
  | seq=nonempty_list(located(atomic_str)) {
      match seq with
      | [s] -> s.value
      | f :: args -> List.fold_left (fun acc arg -> FunctorApp (
          (match acc with StrIdx i -> i | _ -> b (IdxIdx (b "?"))), arg)) f.value args
      | [] -> failwith "impossible: empty structure sequence"
    }
  | str=structure COLON sign=sig_expr { AnotateStr (b (IdxIdx (b "?")), bp Transparent $startpos $endpos, bp str $startpos(str) $endpos(str)) }
  | str=structure COLON_GT sign=sig_expr { AnotateStr (b (IdxIdx (b "?")), bp Opaque $startpos $endpos, bp str $startpos(str) $endpos(str)) }
;

atomic_str:
  | "struct" decs=dec_seq "end" { StructStr (bp decs $startpos(decs) $endpos(decs)) }
  | id=longid { StrIdx (bp id $startpos(id) $endpos(id)) }
  | "let" decs=dec_seq "in" str=structure "end" { Ast.LocalDec (bp decs $startpos(decs) $endpos(decs), bp str $startpos(str) $endpos(str)) }
  | "(" str=structure ")" { str }
  | "(" decs=dec_seq ")" { StructStr (bp decs $startpos(decs) $endpos(decs)) }
;

(* ========================================================================= *)
(* Signatures                                                                *)
(* ========================================================================= *)

sig_expr:

  | "sig" spec=specification "end" { SignSig (flatten_spec_node (bp spec $startpos(spec) $endpos(spec))) }
  | "sig" "end" { SignSig [] }
  | id=sigid { SignIdx (bp id $startpos(id) $endpos(id)) }
  | sign=sig_expr "where" "type" ref=typrefin { SignWhere (bp sign $startpos(sign) $endpos(sign), bp ref $startpos(ref) $endpos(ref)) }
  | "functor" "(" id=modid COLON psig=sig_expr ")" "->" rsig=sig_expr {
      let param_spec = bp (SpecStr (bp (StrDesc (bp id $startpos(id) $endpos(id), bp psig $startpos(psig) $endpos(psig), None)) $startpos(id) $endpos(psig))) $startpos $endpos in
      let result_spec = bp (SpecInclude (bp rsig $startpos(rsig) $endpos(rsig))) $startpos(rsig) $endpos(rsig) in
      SignSig (flatten_spec_node (bp (SpecSeq (param_spec, result_spec)) $startpos $endpos))
    }
  | "functor" id=modid COLON psig=sig_expr "->" rsig=sig_expr {
      let param_spec = bp (SpecStr (bp (StrDesc (bp id $startpos(id) $endpos(id), bp psig $startpos(psig) $endpos(psig), None)) $startpos(id) $endpos(psig))) $startpos $endpos in
      let result_spec = bp (SpecInclude (bp rsig $startpos(rsig) $endpos(rsig))) $startpos(rsig) $endpos(rsig) in
      SignSig (flatten_spec_node (bp (SpecSeq (param_spec, result_spec)) $startpos $endpos))
    }
;

typrefin:
  | tvs=tyvarseq tc=long_tycon EQUAL t=typ rest=option(preceded("and", preceded("type", located(typrefin)))) {
      TypRef (tvs, bp tc $startpos(tc) $endpos(tc), bp t $startpos(t) $endpos(t), match rest with None -> None | Some r -> Some (bp t $startpos(t) $endpos(t), r))
    }
;

sigbind:
  | id=sigid EQUAL sign=sig_expr rest=option(preceded("and", located(sigbind))) { SignBind (bp id $startpos(id) $endpos(id), bp sign $startpos(sign) $endpos(sign), rest) }
;

(* ========================================================================= *)
(* Specifications                                                            *)
(* ========================================================================= *)

specification:
  | spec=kwspec { spec }
  | s1=specification s2=kwspec { SpecSeq (bp s1 $startpos(s1) $endpos(s1), bp s2 $startpos(s2) $endpos(s2)) }
  | spec=specification "sharing" "type" seq=longid_eq_seq { SpecSharingTyp (bp spec $startpos(spec) $endpos(spec), seq) }
  | spec=specification "sharing" seq=longid_eq_seq { SpecSharingStr (bp spec $startpos(spec) $endpos(spec), seq) }
  | spec=specification SEMICOLON { spec }
;

longid_eq_seq:
  | id=longid EQUAL rest=longid_eq_seq { bp id $startpos(id) $endpos(id) :: rest }
  | id=longid { [bp id $startpos(id) $endpos(id)] }
;

spec_seq:
  | seq=spec_seq spec=kwspec { bp spec $startpos(spec) $endpos(spec) :: seq }
  | seq=spec_seq SEMICOLON { seq }
  | { [] }
;

corespec_seq:
  | seq=corespec_seq spec=kwcorespec { bp spec $startpos(spec) $endpos(spec) :: seq }
  | seq=corespec_seq SEMICOLON { seq }
  | { [] }
;

kwspec:
  | spec=kwcorespec { spec }
  | spec=kwmodulespec { spec }
;

kwcorespec:
  | "val" tvs=tyvarseq desc=valdesc { SpecVal (bp desc $startpos(desc) $endpos(desc)) }
  | "type" bind=typbind { SpecTypBind (bp bind $startpos(bind) $endpos(bind)) }
  | "type" desc=typdesc { SpecTyp (bp desc $startpos(desc) $endpos(desc)) }
  | "eqtype" desc=typdesc { SpecEqtyp (bp desc $startpos(desc) $endpos(desc)) }
  | "datatype" desc=datdesc_0 wt=typbind_opt { SpecDat (bp desc $startpos(desc) $endpos(desc)) }
  | "datatype" desc=datdesc_n wt=typbind_opt { SpecDat (bp desc $startpos(desc) $endpos(desc)) }
  | "datatype" tc=tycon EQUAL "datatype" ltc=long_tycon { SpecDatAlias (bp tc $startpos(tc) $endpos(tc), bp ltc $startpos(ltc) $endpos(ltc)) }
  | "exception" desc=exndesc { SpecExn (bp desc $startpos(desc) $endpos(desc)) }
  | "local" s1=specification "in" s2=specification "end" { SpecSeq (bp s1 $startpos(s1) $endpos(s1), bp s2 $startpos(s2) $endpos(s2)) }
  | "open" ids=nonempty_list(boxed(longid)) { SpecIncludeIdx ids }
  | "infix" n=option(INT_LIT) ids=nonempty_list(any_ident) { SpecSeq (b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None)))),
                                             b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None))))) }
  | "infixr" n=option(INT_LIT) ids=nonempty_list(any_ident) { SpecSeq (b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None)))),
                                              b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None))))) }
  | "nonfix" ids=nonempty_list(any_ident) { SpecSeq (b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None)))),
                                    b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None))))) }
;

kwmodulespec:
  | "structure" desc=strdesc { SpecStr (bp desc $startpos(desc) $endpos(desc)) }
  | "functor" desc=fundesc { SpecStr (bp desc $startpos(desc) $endpos(desc)) }
  | "include" sign=sig_expr { SpecInclude (bp sign $startpos(sign) $endpos(sign)) }
  | "include" ids=sigid_seq2 { SpecIncludeIdx ids }
  | "signature" bind=sigbind { SpecSeq (b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None)))),
                                 b (SpecVal (b (ValDesc (b (IdxIdx (b "")), b (TypVar (b (IdxVar (b "")))), None))))) }
;

sigid_seq2:
  | id=sigid rest=nonempty_list(located(sigid)) { bp id $startpos(id) $endpos(id) :: rest }
;

valdesc:
  | id=any_ident COLON t=typ rest=option(preceded("and", located(valdesc))) {
      ValDesc (id, bp t $startpos(t) $endpos(t), rest)
    }
;

typdesc:
  | tvs=tyvarseq tc=tycon rest=option(preceded("and", located(typdesc))) { TypDesc (tvs, bp tc $startpos(tc) $endpos(tc), rest) }
;

datdesc:
  | tvs=tyvarseq tc=tycon EQUAL cons=condesc rest=option(preceded("and", located(datdesc))) { DatDesc (tvs, bp tc $startpos(tc) $endpos(tc), bp cons $startpos(cons) $endpos(cons), rest) }
;

datdesc_0:
  | tc=tycon EQUAL cons=condesc rest=option(preceded("and", located(datdesc))) { DatDesc ([], bp tc $startpos(tc) $endpos(tc), bp cons $startpos(cons) $endpos(cons), rest) }
;

datdesc_n:
  | tvs=tyvarseq1 tc=tycon EQUAL cons=condesc rest=option(preceded("and", located(datdesc))) { DatDesc (tvs, bp tc $startpos(tc) $endpos(tc), bp cons $startpos(cons) $endpos(cons), rest) }
;

condesc:
  | id=op_ident ty=option(preceded("of", located(typ))) rest=option(preceded(BAR, located(condesc))) {
      ConDesc ((match id with WithOp i -> i | WithoutOp i -> i), ty, rest)
    }
;

exndesc:
  | id=op_ident ty=option(preceded("of", located(typ))) rest=option(preceded("and", located(exndesc))) {
      ExnDesc ((match id with WithOp i -> i | WithoutOp i -> i), ty, rest)
    }
;

strdesc:
  | id=modid COLON sign=sig_expr rest=option(preceded("and", located(strdesc))) { StrDesc (bp id $startpos(id) $endpos(id), bp sign $startpos(sign) $endpos(sign), rest) }
;

fundesc:
  | id=modid tail=fundesctail rest=option(preceded("and", located(fundesc))) { StrDesc (bp id $startpos(id) $endpos(id), bp tail $startpos(tail) $endpos(tail), rest) }
;

fundesctail:
  | COLON sign=sig_expr { sign }
  | "(" id=modid COLON psig=sig_expr ")" tail=fundesctail {
      let param_spec = bp (SpecStr (bp (StrDesc (bp id $startpos(id) $endpos(id), bp psig $startpos(psig) $endpos(psig), None)) $startpos(id) $endpos(psig))) $startpos $endpos in
      let result_spec = bp (SpecInclude (bp tail $startpos(tail) $endpos(tail))) $startpos(tail) $endpos(tail) in
      SignSig (flatten_spec_node (bp (SpecSeq (param_spec, result_spec)) $startpos $endpos))
    }
  | id=modid COLON psig=sig_expr tail=fundesctail {
      let param_spec = bp (SpecStr (bp (StrDesc (bp id $startpos(id) $endpos(id), bp psig $startpos(psig) $endpos(psig), None)) $startpos(id) $endpos(psig))) $startpos $endpos in
      let result_spec = bp (SpecInclude (bp tail $startpos(tail) $endpos(tail))) $startpos(tail) $endpos(tail) in
      SignSig (flatten_spec_node (bp (SpecSeq (param_spec, result_spec)) $startpos $endpos))
    }
;

(* ========================================================================= *)
(* Functors                                                                  *)
(* ========================================================================= *)

fctbind:
  | fid=modid "(" pid=modid COLON psig=sig_expr ")" sign=option(pair(located(anotate), located(sig_expr))) EQUAL str=structure rest=option(preceded("and", located(fctbind))) {
      FctBind (bp fid $startpos(fid) $endpos(fid), bp pid $startpos(pid) $endpos(pid), bp psig $startpos(psig) $endpos(psig), sign, bp str $startpos(str) $endpos(str), rest)
    }
  | fid=modid "(" spec=specification ")" sign=option(pair(located(anotate), located(sig_expr))) EQUAL str=structure rest=option(preceded("and", located(fctbind))) {
      FctBindOpen (bp fid $startpos(fid) $endpos(fid), bp spec $startpos(spec) $endpos(spec), sign, bp str $startpos(str) $endpos(str), rest)
    }
  | fid=modid "(" ")" sign=option(pair(located(anotate), located(sig_expr))) EQUAL str=structure rest=option(preceded("and", located(fctbind))) {
      FctGen (bp fid $startpos(fid) $endpos(fid), sign, bp str $startpos(str) $endpos(str), rest)
    }
;

(* ========================================================================= *)
(* Program (Top-level)                                                       *)
(* ========================================================================= *)

(* Program that can be empty - used for final position *)
program:
  | decs=dec_seq { (ProgDec (bp decs $startpos(decs) $endpos(decs))) }
  | "functor" bind=fctbind { (ProgFun (bp bind $startpos(bind) $endpos(bind))) }
  | "signature" bind=sigbind { (ProgStr (bp bind $startpos(bind) $endpos(bind))) }
;

(* Non-empty program - must start with a keyword *)
nonempty_program:
  | decs=nonempty_dec_seq { (ProgDec (bp decs $startpos(decs) $endpos(decs))) }
  | "functor" bind=fctbind { (ProgFun (bp bind $startpos(bind) $endpos(bind))) }
  | "signature" bind=sigbind { (ProgStr (bp bind $startpos(bind) $endpos(bind))) }
;

(* Program list - allows consecutive programs without semicolons *)
(* Each non-final program must be non-empty to break the cycle *)
program_list:
  | p1=nonempty_program SEMICOLON? p2=program_list %prec PROGRAM_SEP { ProgSeq (b p1, b p2) }
  (* | nonempty_program program_list { ProgSeq (b $1, b $2) } *)
  | p=program { p }
  ;

%inline
file:
  plist=program_list { plist }
  ;
main:
  | f=file eof=EOF { (f, eof) }
