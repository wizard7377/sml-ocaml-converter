(** Abstract Syntax Tree for Standard ML

    This module defines the complete abstract syntax tree for Standard ML ('97 revision),
    including all core language constructs and the module language (structures, signatures,
    and functors). The grammar follows the Definition of Standard ML, including derived
    forms from Appendix A.

    @see <http://mitpress.mit.edu/books/definition-standard-ml> The Definition of Standard ML *)

(* open Stdlib *)

type 'a node = {
    value : 'a ;
    comments : string list ;
}
(** {1 Programs}

    Programs are the top-level syntactic category in SML. A program consists of
    core declarations, functor declarations, signature declarations, or sequences thereof. *)

(** Top-level program constructs.

    {[
    prog ::= dec                           (* core declaration *)
           | functor fctbind               (* functor declaration *)
           | signature sigbind             (* signature declaration *)
           |                               (* empty *)
           | prog1 [;] prog2               (* sequence *)
    ]} *)
type prog =
  | ProgDec of dec
      (** Core declaration at the top level. *)
  | ProgFun of fct_bind
      (** Functor declaration: [functor fctbind]. *)
  | ProgStr of sign_bind
      (** Signature declaration: [signature sigbind]. *)
  | ProgSeq of prog * prog
  | ProgEmpty
      (** Sequence of programs: [prog1 ; prog2]. *)
    [@@deriving show]

(** Functor bindings.

    {[
    fctbind ::= id1 ( id2 : sig ) [:[:>] sig] = str [and fctbind]    (* plain *)
              | id ( spec ) [:[:>] sig] = str [and fctbind]          (* opened *)
    ]}

    Functors are parameterized modules that take a structure matching a signature
    and produce a new structure. *)
and fct_bind =
  | FctBind of idx * idx * sign * (anotate * sign) option * str * fct_bind option
      (** Plain functor: [id1 ( id2 : sig ) [:[:>] sig] = str].
          The tuple contains: functor name, parameter name, parameter signature,
          optional result annotation, body structure, and optional additional bindings. *)
  | FctBindOpen of idx * spec * (anotate * sign) option * str * fct_bind option
      (** Opened functor: [id ( spec ) [:[:>] sig] = str].
          The specification is directly visible in the functor body without qualification. *)

(** Signature bindings.

    {[
    sigbind ::= id = sig [and sigbind]
    ]}

    Binds identifiers to signatures for later use. *)
and sign_bind =
  | SignBind of idx * sign * sign_bind option
      (** Signature binding: [id = sig]. *)

(** {1 Core Language}

    The core language includes constants, identifiers, expressions, patterns,
    types, and declarations. *)

(** {2 Constants}

    {[
    con ::= int       (* integer *)
          | word      (* word *)
          | float     (* floating point *)
          | char      (* character *)
          | string    (* string *)
    ]}

    Integer constants may be decimal or hexadecimal (prefixed with [0x]),
    and may have an optional negation prefix [~].
    Word constants are unsigned and prefixed with [0w] (decimal) or [0wx] (hex).
    Character constants are written as [#"c"].
    String constants are enclosed in double quotes. *)
and con =
  | ConInt of string
      (** Integer constant. Decimal: [\[~\]num], hexadecimal: [\[~\]0xhex]. *)
  | ConWord of string
      (** Word (unsigned integer) constant. Decimal: [0wnum], hexadecimal: [0wxhex]. *)
  | ConFloat of string
      (** Floating point constant: [\[~\]num.num] or [\[~\]num\[.num\]e\[~\]num]. *)
  | ConChar of string
      (** Character constant: [#"ascii"]. *)
  | ConString of string
      (** String constant: ["ascii*"]. *)

(** {2 Identifiers}

    {[
    id     ::= letter (letter | digit | ' | _)*     (* alphanumeric *)
             | (! | % | & | $ | # | + | - | ...)+   (* symbolic *)
    var    ::= 'letter (letter | digit | ' | _)*    (* unconstrained type variable *)
             | ''letter (letter | digit | ' | _)*   (* equality type variable *)
    longid ::= id1.....idn                          (* qualified, n >= 1 *)
    lab    ::= id                                   (* identifier label *)
             | num                                  (* numeric label, may not start with 0 *)
    ]}

    Identifiers can be alphanumeric or symbolic. Type variables are prefixed with
    a single quote ([']) or double quote (['']) for equality type variables.
    Long identifiers are dot-separated qualified names. *)
and idx =
  | IdxIdx of string
      (** Simple alphanumeric or symbolic identifier. *)
  | IdxVar of string
      (** Type variable: ['var] or [''var] for equality type variables. *)
  | IdxLong of idx list
      (** Long (qualified) identifier: [id1.id2...idn]. *)
  | IdxLab of string
      (** Record label (alphanumeric identifier). *)
  | IdxNum of string
      (** Numeric label for tuple access (1-indexed, may not start with 0). *)

(** {2 Expressions}

    {[
    exp ::= con                                    (* constant *)
          | [op] longid                            (* value or constructor identifier *)
          | exp1 exp2                              (* application *)
          | exp1 id exp2                           (* infix application *)
          | ( exp )                                (* parentheses *)
          | ( exp1 , ... , expn )                  (* tuple, n != 1 *)
          | { [exprow] }                           (* record *)
          | # lab                                  (* record selector *)
          | [ exp1 , ... , expn ]                  (* list, n >= 0 *)
          | ( exp1 ; ... ; expn )                  (* sequence, n >= 2 *)
          | let dec in exp1 ; ... ; expn end       (* local declaration, n >= 1 *)
          | exp : typ                              (* type annotation *)
          | raise exp                              (* exception raising *)
          | exp handle match                       (* exception handling *)
          | exp1 andalso exp2                      (* conjunction *)
          | exp1 orelse exp2                       (* disjunction *)
          | if exp1 then exp2 else exp3            (* conditional *)
          | while exp1 do exp2                     (* iteration *)
          | case exp of match                      (* case analysis *)
          | fn match                               (* function *)
    ]} *)
and exp =
  | ExpCon of con
      (** Constant expression. *)
  | ExpIdx of idx
      (** Value or constructor identifier, optionally prefixed with [op]. *)
  | ExpApp of exp * exp
      (** Function application: [exp1 exp2]. Left-associative. *)
  | InfixApp of exp * idx * exp
      (** Infix operator application: [exp1 id exp2].
          The operator [id] must be declared infix. *)
  | ParenExp of exp
      (** Parenthesized expression: [( exp )]. *)
  | TupleExp of exp list
      (** Tuple expression: [( exp1 , ... , expn )] where n != 1.
          The unit value [()] is represented as an empty list. *)
  | RecordExp of row list
      (** Record expression: [{ exprow }]. *)
  | RecordSelector of idx
      (** Record field selector: [# lab]. Selects a field from a record. *)
  | ListExp of exp list
      (** List expression: [\[ exp1 , ... , expn \]] where n >= 0. *)
  | SeqExp of exp list
      (** Sequential expression: [( exp1 ; ... ; expn )] where n >= 2.
          Evaluates expressions left-to-right, returning the last value. *)
  | LetExp of dec list * exp list
      (** Local declaration: [let dec in exp1 ; ... ; expn end].
          Declarations are visible only within the body expressions. *)
  | TypedExp of exp * typ
      (** Type-annotated expression: [exp : typ]. *)
  | RaiseExp of exp
      (** Exception raising: [raise exp].
          The expression must evaluate to an exception value. *)
  | HandleExp of exp * matching
      (** Exception handling: [exp handle match].
          If [exp] raises an exception, it is matched against [match]. *)
  | AndExp of exp * exp
      (** Short-circuit conjunction: [exp1 andalso exp2].
          [exp2] is evaluated only if [exp1] is [true]. *)
  | OrExp of exp * exp
      (** Short-circuit disjunction: [exp1 orelse exp2].
          [exp2] is evaluated only if [exp1] is [false]. *)
  | IfExp of exp * exp * exp
      (** Conditional: [if exp1 then exp2 else exp3]. *)
  | WhileExp of exp * exp
      (** While loop: [while exp1 do exp2].
          Repeatedly evaluates [exp2] while [exp1] is [true]. Returns unit. *)
  | CaseExp of exp * matching
      (** Case analysis: [case exp of match].
          Pattern matches [exp] against the cases in [match]. *)
  | FnExp of matching
      (** Anonymous function: [fn match].
          Creates a function value from a match. *)

(** {2 Expression Rows}

    {[
    exprow ::= lab = exp [, exprow]
    ]}

    Record rows bind labels to expressions in record literals. *)
and row =
  | Row of idx * exp * row option
      (** Expression row: [lab = exp].
          The optional third component allows chaining: [lab1 = exp1 , lab2 = exp2]. *)

(** {2 Match Clauses}

    {[
    match ::= pat => exp [| match]
    ]}

    Matches are used in [case], [handle], and [fn] expressions.
    Each clause binds pattern variables in the corresponding expression. *)
and matching =
  | Case of pat * exp * matching option
      (** Match clause: [pat => exp].
          The optional third component chains additional clauses: [pat1 => exp1 | pat2 => exp2]. *)

(** {2 Declarations}

    {[
    dec ::= val [var,] valbind                                  (* value *)
          | fun [var,] funbind                                  (* function *)
          | type typbind                                        (* type *)
          | datatype datbind [withtype typbind]                 (* data type *)
          | datatype id = datatype longid                       (* data type replication *)
          | abstype datbind [withtype typbind] with dec end     (* abstract type *)
          | exception exnbind                                   (* exception *)
          | structure strbind                                   (* structure, not in expressions *)
          |                                                     (* empty *)
          | dec1 [;] dec2                                       (* sequence *)
          | local dec1 in dec2 end                              (* local *)
          | open longid1 ... longidn                            (* inclusion, n >= 1 *)
          | nonfix id1 ... idn                                  (* nonfix, n >= 1 *)
          | infix [digit] id1 ... idn                           (* left-associative infix, n >= 1 *)
          | infixr [digit] id1 ... idn                          (* right-associative infix, n >= 1 *)
    ]} *)
and dec =
  | ValDec of idx list * val_bind
      (** Value declaration: [val \[var,\] valbind].
          The [idx list] contains explicit type variables for polymorphism. *)
  | FunDec of fun_bind
      (** Function declaration: [fun \[var,\] funbind].
          Syntactic sugar for recursive function definitions. *)
  | TypDec of typ_bind
      (** Type abbreviation: [type typbind].
          Introduces type synonyms. *)
  | DatDec of dat_bind * typ_bind option
      (** Datatype declaration: [datatype datbind \[withtype typbind\]].
          The optional [typbind] allows mutually recursive type abbreviations. *)
  | DataDecAlias of idx * idx
      (** Datatype replication: [datatype id = datatype longid].
          Makes [id] an alias for an existing datatype. *)
  | AbstractDec of dat_bind * typ_bind option * dec list
      (** Abstract type: [abstype datbind \[withtype typbind\] with dec end].
          The datatype constructors are hidden outside the [with] block. *)
  | ExnDec of exn_bind
      (** Exception declaration: [exception exnbind]. *)
  | StrDec of str_bind
      (** Structure declaration: [structure strbind].
          Not allowed inside expressions. *)
  | SeqDec of dec list
      (** Sequence of declarations: [dec1 ; dec2 ; ...]. *)
  | LocalDec of dec * dec
      (** Local declaration: [local dec1 in dec2 end].
          [dec1] is visible only within [dec2]. *)
  | OpenDec of idx list
      (** Structure inclusion: [open longid1 ... longidn].
          Makes all bindings from the structures directly visible. *)
  | FixityDec of fixity * idx list
      (** Fixity declaration for operators. *)

(** Operator fixity specifications.

    {[
    nonfix id1 ... idn                  (* remove infix status *)
    infix [digit] id1 ... idn           (* left-associative, precedence 0-9 *)
    infixr [digit] id1 ... idn          (* right-associative, precedence 0-9 *)
    ]}

    Precedence defaults to 0 if not specified. Higher numbers bind tighter. *)
and fixity =
  | Nonfix
      (** Remove infix status: [nonfix id]. *)
  | Infix of int
      (** Left-associative infix: [infix \[n\] id]. Precedence 0-9. *)
  | Infixr of int
      (** Right-associative infix: [infixr \[n\] id]. Precedence 0-9. *)

(** Value bindings.

    {[
    valbind ::= pat = exp [and valbind]     (* destructuring *)
              | rec valbind                 (* recursive *)
    ]} *)
and val_bind =
  | ValBind of pat * exp * val_bind option
      (** Destructuring binding: [pat = exp].
          Pattern variables are bound to corresponding parts of the value. *)
  | ValBindRec of val_bind
      (** Recursive binding: [rec valbind].
          Allows the bound names to be used in the expressions. *)

(** Function bindings.

    {[
    funbind ::= funmatch [and funbind]      (* clausal function *)
    ]} *)
and fun_bind =
  | FunBind of fun_match * fun_bind option
      (** Function binding with clauses. Additional bindings for mutual recursion. *)

(** Function match clauses.

    {[
    funmatch ::= [op] id pat1 ... patn [: typ] = exp [| funmatch]    (* nonfix, n >= 1 *)
               | pat1 id pat2 [: typ] = exp [| funmatch]             (* infix *)
               | ( pat1 id pat2 ) pat'1 ... pat'n [: typ] = exp [| funmatch]  (* infix, n >= 0 *)
    ]}

    Multiple clauses define pattern matching on function arguments. *)
and fun_match =
  | FunMatchPrefix of with_op * pat list * typ option * exp * fun_match option
      (** Prefix (nonfix) function clause: [\[op\] id pat1 ... patn \[: typ\] = exp].
          The [with_op] indicates the function name (with optional [op] prefix). *)
  | FunMatchInfix of pat * idx * pat * typ option * exp * fun_match option
      (** Infix function clause: [pat1 id pat2 \[: typ\] = exp].
          The identifier [id] must be declared infix. *)
  | FunMatchLow of pat * idx * pat * pat list * typ option * exp * fun_match option
      (** Curried infix clause: [( pat1 id pat2 ) pat'1 ... pat'n \[: typ\] = exp].
          An infix operator with additional curried arguments. *)

(** Type bindings.

    {[
    typbind ::= [var,] id = typ [and typbind]
    ]}

    Introduces type abbreviations (synonyms). *)
and typ_bind =
  | TypBind of idx list * idx * typ * typ_bind option
      (** Type abbreviation: [\[var,\] id = typ].
          The [idx list] contains type parameters. *)

(** Datatype bindings.

    {[
    datbind ::= [var,] id = conbind [and datbind]
    ]}

    Introduces new algebraic data types with constructors. *)
and dat_bind =
  | DatBind of idx list * idx * con_bind * dat_bind option
      (** Datatype binding: [\[var,\] id = conbind].
          Type parameters, type name, constructors, and optional additional bindings. *)

(** Constructor bindings.

    {[
    conbind ::= id [of typ] [| conbind]
    ]}

    Defines data constructors for a datatype. *)
and con_bind =
  | ConBind of idx * typ option * con_bind option
      (** Constructor: [id \[of typ\]].
          The optional type specifies the constructor's argument type. *)

(** Exception bindings.

    {[
    exnbind ::= id [of typ] [and exnbind]     (* generative *)
              | id = longid [and exnbind]     (* renaming *)
    ]} *)
and exn_bind =
  | ExnBind of idx * typ option * exn_bind option
      (** Generative exception: [id \[of typ\]].
          Creates a new exception constructor. *)
  | ExnBindAlias of idx * idx * exn_bind option
      (** Exception renaming: [id = longid].
          Makes [id] an alias for an existing exception. *)

(** {1 Module Language}

    The module language provides structures (collections of declarations),
    signatures (types of structures), and functors (parameterized structures). *)

(** {2 Structures}

    {[
    str ::= longid                        (* identifier *)
          | struct dec end                (* structure *)
          | str : sig                     (* transparent annotation *)
          | str :> sig                    (* opaque annotation *)
          | id ( str )                    (* functor application *)
          | id ( dec )                    (* functor application *)
          | let dec in str end            (* local declaration *)
    ]} *)
and str =
  | StrIdx of idx
      (** Structure identifier (possibly qualified). *)
  | StructStr of dec
      (** Structure expression: [struct dec end]. *)
  | AnotateStr of idx * anotate * str
      (** Annotated structure: [str : sig] or [str :> sig].
          Note: Uses structure name as first component for binding context. *)
  | FunctorApp of idx * str
      (** Functor application: [id ( str )]. *)
  | FunctorAppAnonymous of idx * dec
      (** Functor application with anonymous argument: [id ( dec )].
          The declarations form an anonymous structure. *)
  | LocalDec of dec * str
      (** Local declaration in structure: [let dec in str end]. *)

(** Signature annotations for structures.

    Transparent annotation ([: sig]) preserves type equalities.
    Opaque annotation ([:> sig]) hides type implementations. *)
and anotate =
  | Transparent
      (** Transparent annotation: [str : sig].
          Type equalities are visible outside the structure. *)
  | Opaque
      (** Opaque annotation: [str :> sig].
          Abstract types hide their implementations. *)

(** Structure bindings.

    {[
    strbind ::= id [:[:>] sig] = str [and strbind]
    ]} *)
and str_bind =
  | StrBind of idx * (anotate * sign) option * str_bind option
      (** Structure binding: [id \[:[\:>\] sig\] = str].
          Note: The structure expression is omitted here; see context. *)

(** {2 Signatures}

    {[
    sig ::= id                                   (* identifier *)
          | sig spec end                         (* signature *)
          | sig where type typrefin              (* refinement *)
    ]} *)
and sign =
  | SignIdx of idx
      (** Signature identifier. *)
  | SignSig of sign * spec
      (** Signature expression: [sig spec end].
          Note: Represented as signature with specification body. *)
  | SignWhere of sign * typ_refine
      (** Signature with type refinement: [sig where type typrefin]. *)

(** Type refinements in [where type] clauses.

    {[
    typrefin ::= [var,] longid = typ [and type typrefin]
    ]}

    Refines abstract types in a signature to specific types. *)
and typ_refine =
  | TypRef of idx list * idx * typ * (typ * typ_refine) option
      (** Type refinement: [\[var,\] longid = typ].
          Type parameters, type path, definition, and optional additional refinements. *)

(** {2 Specifications}

    {[
    spec ::= val valdesc                                          (* value *)
           | type typdesc                                         (* type *)
           | eqtype typdesc                                       (* equality type *)
           | type typbind                                         (* type abbreviation *)
           | datatype datdesc                                     (* data type *)
           | datatype id = datatype longid                        (* data type replication *)
           | exception exndesc                                    (* exception *)
           | structure strdesc                                    (* structure *)
           |                                                      (* empty *)
           | spec1 [;] spec2                                      (* sequence *)
           | include sig                                          (* inclusion *)
           | include id1 ... idn                                  (* inclusion, n >= 1 *)
           | spec sharing type longid1 = ... = longidn            (* type sharing, n >= 2 *)
           | spec sharing longid1 = ... = longidn                 (* structure sharing, n >= 2 *)
    ]} *)
and spec =
  | SpecVal of val_desc
      (** Value specification: [val valdesc]. *)
  | SpecTyp of typ_desc
      (** Abstract type specification: [type typdesc]. *)
  | SpecEqtyp of typ_desc
      (** Equality type specification: [eqtype typdesc].
          Specifies types that admit equality. *)
  | SpecTypBind of typ_bind
      (** Type abbreviation in signature: [type typbind]. *)
  | SpecDat of dat_desc
      (** Datatype specification: [datatype datdesc]. *)
  | SpecDatAlias of idx * idx
      (** Datatype replication: [datatype id = datatype longid]. *)
  | SpecExn of exn_desc
      (** Exception specification: [exception exndesc]. *)
  | SpecStr of str_desc
      (** Structure specification: [structure strdesc]. *)
  | SpecSeq of spec * spec
      (** Sequence of specifications: [spec1 ; spec2]. *)
  | SpecInclude of sign
      (** Include signature: [include sig]. *)
  | SpecIncludeIdx of idx list
      (** Include multiple signatures: [include id1 ... idn]. *)
  | SpecSharingTyp of spec * idx list
      (** Type sharing constraint: [spec sharing type longid1 = ... = longidn].
          Asserts that the named types are the same. *)
  | SpecSharingStr of spec * idx list
      (** Structure sharing constraint: [spec sharing longid1 = ... = longidn].
          Asserts that the named structures share all type components. *)

(** Value descriptions in signatures.

    {[
    valdesc ::= id : typ [and valdesc]
    ]} *)
and val_desc =
  | ValDesc of idx * typ * val_desc option
      (** Value description: [id : typ]. *)

(** Type descriptions (abstract types) in signatures.

    {[
    typdesc ::= [var,] id [and typdesc]
    ]} *)
and typ_desc =
  | TypDesc of idx list * idx * typ_desc option
      (** Type description: [\[var,\] id].
          Declares an abstract type with given arity. *)

(** Datatype descriptions in signatures.

    {[
    datdesc ::= [var,] id = condesc [and datdesc]
    ]} *)
and dat_desc =
  | DatDesc of idx list * idx * con_desc * dat_desc option
      (** Datatype description with constructors. *)

(** Constructor descriptions in signatures.

    {[
    condesc ::= id [of typ] [| condesc]
    ]} *)
and con_desc =
  | ConDesc of idx * typ option * con_desc option
      (** Constructor description: [id \[of typ\]]. *)

(** Exception descriptions in signatures.

    {[
    exndesc ::= id [of typ] [and exndesc]
    ]} *)
and exn_desc =
  | ExnDesc of idx * typ option * exn_desc option
      (** Exception description: [id \[of typ\]]. *)

(** Structure descriptions in signatures.

    {[
    strdesc ::= id : sig [and strdesc]
    ]} *)
and str_desc =
  | StrDesc of idx * sign * str_desc option
      (** Structure description: [id : sig]. *)

(** {2 Types}

    {[
    typ ::= var                          (* variable *)
          | [typ,] longid                (* constructor *)
          | ( typ )                      (* parentheses *)
          | typ1 -> typ2                 (* function *)
          | typ1 * ... * typn            (* tuple, n >= 2 *)
          | { [typrow] }                 (* record *)
    ]} *)
and typ =
  | TypVar of idx
      (** Type variable: ['var] or [''var]. *)
  | TypCon of typ list * idx
      (** Type constructor application: [\[typ,\] longid].
          Examples: [int], [int list], [(int, string) either]. *)
  | TypPar of typ
      (** Parenthesized type: [( typ )]. *)
  | TypFun of typ * typ
      (** Function type: [typ1 -> typ2]. Right-associative. *)
  | TypTuple of typ list
      (** Tuple type: [typ1 * ... * typn] where n >= 2. *)
  | TypRecord of typ_row list
      (** Record type: [{ typrow }]. *)

(** Type rows in record types.

    {[
    typrow ::= lab : typ [, typrow]
    ]} *)
and typ_row =
  | TypRow of idx * typ * typ_row option
      (** Type row: [lab : typ]. *)

(** Identifier with optional [op] prefix.

    The [op] keyword removes the infix status of an identifier,
    allowing it to be used as a regular prefix identifier. *)
and with_op =
  | WithOp of idx
      (** Identifier with [op] prefix: [op id]. *)
  | WithoutOp of idx
      (** Plain identifier without [op] prefix. *)

(** {2 Patterns}

    {[
    pat ::= con                               (* constant *)
          | _                                 (* wildcard *)
          | [op] id                           (* variable *)
          | [op] longid [pat]                 (* construction *)
          | pat1 id pat2                      (* infix construction *)
          | ( pat )                           (* parentheses *)
          | ( pat1 , ... , patn )             (* tuple, n != 1 *)
          | { [patrow] }                      (* record *)
          | [ pat1 , ... , patn ]             (* list, n >= 0 *)
          | pat : typ                         (* type annotation *)
          | [op] id [: typ] as pat            (* layered *)
    ]} *)
and pat =
  | PatCon of con
      (** Constant pattern: matches a specific constant value. *)
  | PatWildcard
      (** Wildcard pattern: [_]. Matches any value without binding. *)
  | PatIdx of with_op
      (** Variable or nullary constructor pattern: [\[op\] id]. *)
  | PatApp of with_op * pat
      (** Constructor application pattern: [\[op\] longid pat]. *)
  | PatInfix of pat * idx * pat
      (** Infix constructor pattern: [pat1 id pat2].
          The identifier must be an infix constructor (e.g., [::]). *)
  | PatParen of pat
      (** Parenthesized pattern: [( pat )]. *)
  | PatTuple of pat list
      (** Tuple pattern: [( pat1 , ... , patn )] where n != 1. *)
  | PatRecord of pat_row list
      (** Record pattern: [{ patrow }]. *)
  | PatList of pat list
      (** List pattern: [\[ pat1 , ... , patn \]] where n >= 0. *)
  | PatTyp of pat * typ
      (** Type-annotated pattern: [pat : typ]. *)
  | PatAs of with_op * typ option * pat
      (** Layered (as) pattern: [\[op\] id \[: typ\] as pat].
          Binds the identifier to the entire matched value while also
          matching the inner pattern. *)

(** Pattern rows in record patterns.

    {[
    patrow ::= ...                                    (* wildcard *)
             | lab = pat [, patrow]                   (* pattern *)
             | id [: typ] [as pat] [, patrow]         (* variable *)
    ]} *)
and pat_row =
  | PatRowPoly
      (** Wildcard row: [...]. Matches remaining record fields. *)
  | PatRowSimple of idx * pat * pat_row
      (** Pattern row: [lab = pat]. Matches a specific field. *)
  | PatRowVar of idx * typ option * idx option * pat_row option
      (** Variable row: [id \[: typ\] \[as pat\]].
          Shorthand where the label equals the variable name. *)




type cm_filetype = CM_CM | CM_Sig | CM_Sml | CM_Fun
type cm_file = { 

    cm_header : string ; 
    cm_sources : (string * cm_filetype) list
}
