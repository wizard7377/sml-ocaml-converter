(** {1 SML to OCaml Backend}

    This module implements the backend of the SML to OCaml converter,
    transforming the SML abstract syntax tree into OCaml's Parsetree
    representation for code generation.

    The conversion follows these key principles:
    - SML types map to OCaml types with adjustments for syntax differences
    - SML module system (structures, signatures, functors) maps to OCaml modules
    - Pattern matching and expressions are converted with equivalent semantics
    - Name processing handles reserved word conflicts and capitalization conventions

    @see <https://ocaml.org/api/compilerlibref/Parsetree.html> OCaml Parsetree documentation
    @see <http://sml-family.org/sml97-defn.pdf> Standard ML definition *)

open Process_names

(** Result type for the complete conversion.
    Currently unspecified; will contain the final OCaml structure/signature. *)
type res

include Helpers

(** Helper function to create a located value with no source location.

    @param v The value to wrap with a phantom location
    @return The value wrapped in a {!Location.loc} structure *)
let ghost (v : 'a) : 'a Location.loc = Location.mkloc v Location.none

(** Main entry point for converting a complete SML program.

    @param prog The SML program to convert
    @return The converted OCaml representation
    @raise Assert_failure Currently unimplemented *)
let process_sml ~(prog:Ast.prog): res = assert false

(** {2 Type Processing}

    Functions for converting SML types to OCaml types.

    SML and OCaml have similar type systems, but with syntactic differences:
    - SML: [int * string -> bool] vs OCaml: [int * string -> bool]
    - SML: ['a list] vs OCaml: ['a list]
    - SML: [{x: int, y: int}] (records) vs OCaml: [< x: int; y: int >] (objects)

    The main difference is that SML records are converted to OCaml objects. *)

(** Convert an SML type to an OCaml core type.

    This is the main type conversion function that handles all SML type forms:
    - Type variables (['a], [''a] for equality types)
    - Type constructors ([int], ['a list], [(int, string) either])
    - Function types ([int -> bool])
    - Tuple types ([int * string * bool])
    - Record types ([{x: int, y: string}] → object types)

    @param ty The SML type to convert
    @return The corresponding OCaml {!Parsetree.core_type}

    @example
    {[
      (* SML: 'a -> 'a list *)
      process_type_value (TypFun (TypVar "a", TypCon ([TypVar "a"], "list")))
      (* → OCaml Parsetree for: 'a -> 'a list *)
    ]} *)
let rec process_type_value (ty : Ast.typ) : Parsetree.core_type = match ty with
  TypVar name -> process_type_var name
  | TypCon (args, head) -> let
    head' = process_name ~context:ValueType head in
    let args' = List.map process_type_value args in
    Builder.ptyp_constr (ghost (process_longid head')) args'
  | TypPar ty -> process_type_value ty
  | TypFun (ty1, ty2) -> let ty1', ty2' = process_type_value ty1, process_type_value ty2 in
    Builder.ptyp_arrow Nolabel ty1' ty2'
  | TypTuple tys -> Builder.ptyp_tuple (List.map process_type_value tys)
  | TypRecord fields -> let fields' = List.flatten (List.map process_object_field_type fields) in
    Builder.ptyp_object fields' Closed

(** Convert SML record type rows to OCaml object fields.

    SML record types like [{name: string, age: int}] are converted to
    OCaml object types like [< name: string; age: int >].

    @param field A single type row (field) and its optional continuation
    @return A list of OCaml object fields (tags with their types)

    @example
    {[
      (* SML: {x: int, y: int} *)
      process_object_field_type (TypRow ("x", TypCon ([], "int"),
                                         Some (TypRow ("y", TypCon ([], "int"), None))))
      (* → [Otag("x", int_type); Otag("y", int_type)] *)
    ]} *)
and process_object_field_type (field : Ast.typ_row) : Parsetree.object_field list = match field with
  | Ast.TypRow (name, ty, rest) -> let here : Parsetree.object_field = Builder.otag (ghost (process_name ~context:Label name |> idx_to_string)) (process_type_value ty) in
    begin match rest with
    | Some rest' -> here :: (process_object_field_type rest')
    | None -> [here]
    end

(** Wrapper function for {!process_type_value}.

    @param ty The SML type to convert
    @return The corresponding OCaml core type *)
let rec process_type (ty : Ast.typ) : Parsetree.core_type = process_type_value ty

(** {2 Constant Processing}

    Functions for converting SML constants to OCaml constants.

    Note: SML and OCaml have different constant syntaxes that need translation:
    - SML uses [~] for negation, OCaml uses [-]
    - SML has word literals ([0w42]), OCaml doesn't (need conversion)
    - SML character literals use [#"c"], OCaml uses ['c'] *)

(** Convert an SML constant to an OCaml constant.

    Handles:
    - Integer constants (decimal and hexadecimal)
    - Word constants (unsigned integers, SML-specific)
    - Floating-point constants
    - Character constants ([#"a"] → ['a'])
    - String constants

    @param con The SML constant to convert
    @return The corresponding OCaml {!Parsetree.constant}
    @raise Assert_failure All cases currently unimplemented *)
let rec process_con (con : Ast.con) : Parsetree.constant = match con with
  | ConInt i -> assert false
  | ConWord w -> assert false
  | ConFloat r -> assert false
  | ConChar c -> assert false
  | ConString s -> assert false

(** {2 Expression Processing}

    Functions for converting SML expressions to OCaml expressions.

    Expression conversion is mostly straightforward, with key differences:
    - SML's [andalso]/[orelse] → OCaml's [&&]/[||]
    - SML's [fn] → OCaml's [fun]
    - SML's sequential expressions [(e1; e2; e3)] → OCaml's [e1; e2; e3]
    - Let expressions require different structuring *)

(** Convert an SML expression to an OCaml expression.

    Currently handles:
    - Constants (via {!process_con})
    - Identifiers (with proper name processing)
    - Function application ([f x])
    - Infix operators ([x + y])

    @param exp The SML expression to convert
    @return The corresponding OCaml {!Parsetree.expression}
    @raise Assert_failure For unimplemented expression forms

    @example
    {[
      (* SML: f x *)
      process_exp (ExpApp (ExpIdx "f", ExpIdx "x"))
      (* → OCaml Parsetree for: f x *)

      (* SML: x + y *)
      process_exp (InfixApp (ExpIdx "x", "+", ExpIdx "y"))
      (* → OCaml Parsetree for: (+) x y *)
    ]} *)
let rec process_exp (exp : Ast.exp) : Parsetree.expression = match exp with
  | ExpCon c -> Builder.pexp_constant (process_con c)
  | ExpApp (e1, e2) -> let e1' = process_exp e1 in
    let e2' = process_exp e2 in
    Builder.pexp_apply e1' [(Nolabel, e2')]
  | ExpIdx idx -> Builder.pexp_ident (ghost (process_longid (process_name ~context:Value idx)))
  | InfixApp (e1, op, e2) -> let op' = process_name ~context:Value op in
      Builder.pexp_apply (Builder.pexp_ident (ghost (process_longid op'))) [(Nolabel, process_exp e1); (Nolabel, process_exp e2)]
  | LetExp (bindings, body) -> assert false
  | _ -> assert false
  

(** {1 Pattern Processing}

    Functions for converting SML patterns to OCaml patterns.

    Pattern conversion is complex due to:
    - Distinguishing constructors from variables (SML uses capitalization,
      but also allows lowercase constructors in some contexts)
    - Handling the [op] keyword for treating infix operators as prefix
    - Converting record patterns to tuple/record patterns
    - Layered patterns ([x as pat]) *)

(** Convert an SML pattern to an OCaml pattern.

    Handles all SML pattern forms including:
    - Constant patterns ([42], ["hello"])
    - Wildcard ([_])
    - Variable and constructor patterns (with heuristics for disambiguation)
    - Constructor application ([SOME x], [Cons(h, t)])
    - Infix constructors ([h :: t])
    - Tuples ([(x, y, z)])
    - Records ([{x, y}] or [{x = px, y = py}])
    - Lists ([[x, y, z]])
    - Type-annotated patterns ([x : int])
    - Layered patterns ([x as SOME y])

    @param is_head When true, treats identifier patterns as constructors.
                   Used in the head position of constructor applications.
    @param pat The SML pattern to convert
    @return The corresponding OCaml {!Parsetree.pattern}
    @raise Assert_failure For unimplemented pattern forms
    @raise WrongTypeName When identifier context is incorrect

    @example
    {[
      (* SML: case x of NONE => 0 | SOME y => y *)
      process_pat ~is_head:true (PatIdx (WithoutOp "NONE"))
      (* → Constructor pattern *)

      process_pat ~is_head:false (PatIdx (WithoutOp "y"))
      (* → Variable pattern *)
    ]} *)
let rec process_pat ?(is_head=false) (pat : Ast.pat) : Parsetree.pattern = match pat with
  | PatCon c -> Builder.ppat_constant (process_con c)
  | PatWildcard -> Builder.ppat_any
  | PatIdx (WithOp op) when is_head || (not (pattern_is_var_idx op)) -> let name = process_name ~context:(if is_head then PatternHead else PatternOther) op in
      begin match name with
      | Name n -> Builder.ppat_construct (ghost (process_longid name)) None
      | _ -> raise WrongTypeName
      end
  | PatIdx (WithoutOp id) when is_head || (not (pattern_is_var_idx id)) -> let name = process_name ~context:(if is_head then PatternHead else PatternOther) id in
      begin match name with
      | Name n -> Builder.ppat_construct (ghost (process_longid name)) None
      | _ -> raise WrongTypeName
      end
  | PatIdx (WithOp idx) -> let name = process_name ~context:PatternVar idx in Builder.ppat_var (ghost (idx_to_string name))
  | PatIdx (WithoutOp idx) -> let name = process_name ~context:PatternVar idx in Builder.ppat_var (ghost (idx_to_string name))
  | PatApp (wo, p) -> assert false
  | PatInfix (p1, id, p2) -> assert false
  | PatParen p -> assert false
  | PatTuple ps -> assert false
  | PatRecord rows -> assert false
  | PatList ps -> assert false
  | PatTyp (p, t) -> assert false
  | PatAs (wo, t_opt, p) -> assert false

(** Convert SML pattern rows (record pattern fields) to OCaml record patterns.

    SML record patterns have three forms:
    - Wildcard: [{..., x, y}] matches any record with at least x and y fields
    - Simple: [{x = px, y = py}] binds px and py
    - Variable shorthand: [{x, y}] is sugar for [{x = x, y = y}]

    @param row The pattern row to convert
    @return A list of field-pattern pairs
    @raise Assert_failure Currently unimplemented *)
and process_pat_row (row : Ast.pat_row) : (string * Parsetree.pattern) list = match row with
  | PatRowPoly -> assert false
  | PatRowSimple (lab, pat, rest) -> assert false
  | PatRowVar (id, ty_opt, as_opt, rest_opt) -> assert false

(** {2 Expression Rows and Matching}

    Helper functions for expression-related constructs. *)

(** Convert an SML expression row (record field) to an OCaml record field.

    SML: [{x = 1, y = 2}]
    OCaml: [{x = 1; y = 2}]

    @param row The expression row (field binding)
    @return A pair of field identifier and expression
    @raise Assert_failure Currently unimplemented *)
and process_row (row : Ast.row) : (Longident.t Location.loc * Parsetree.expression) = match row with
  | Row (lab, exp, rest_opt) -> assert false

(** Convert SML match clauses to OCaml case list.

    SML: [pat1 => exp1 | pat2 => exp2]
    OCaml: [| pat1 -> exp1 | pat2 -> exp2]

    Used in [case], [fn], and [handle] expressions.

    @param m The match clause(s)
    @return A list of OCaml case expressions
    @raise Assert_failure Currently unimplemented *)
and process_matching (m : Ast.matching) : Parsetree.case list = match m with
  | Case (pat, exp, rest_opt) -> assert false

(** {1 Declaration Processing}

    Functions for converting SML declarations to OCaml structure items.

    SML declarations include:
    - Value bindings ([val x = 42])
    - Function definitions ([fun f x = x + 1])
    - Type definitions ([type t = int * string])
    - Datatype declarations ([datatype t = A | B of int])
    - Exception declarations ([exception E of string])
    - Structure/module declarations
    - Local declarations, fixity declarations, etc. *)

(** Convert an SML declaration to OCaml value bindings.

    @param dec The SML declaration to convert
    @return A list of OCaml value bindings
    @raise Assert_failure Currently unimplemented *)
and process_dec (dec : Ast.dec) : Parsetree.value_binding list = match dec with
  | ValDec (tvars, vb) -> assert false
  | FunDec fb -> assert false
  | TypDec tb -> assert false
  | DatDec (db, tb_opt) -> assert false
  | DataDecAlias (id1, id2) -> assert false
  | AbstractDec (db, tb_opt, decs) -> assert false
  | ExnDec eb -> assert false
  | StrDec sb -> assert false
  | SeqDec decs -> assert false
  | LocalDec (d1, d2) -> assert false
  | OpenDec ids -> assert false
  | FixityDec (fix, ids) -> assert false

(** Convert SML fixity declarations to string representation.

    SML fixity: [infix 6 +], [infixr 5 ::], [nonfix f]

    @param fix The fixity specification
    @return String representation of fixity
    @raise Assert_failure Currently unimplemented *)
and process_fixity (fix : Ast.fixity) : string = match fix with
  | Nonfix -> assert false
  | Infix n -> assert false
  | Infixr n -> assert false

(** Convert SML value bindings.

    SML: [val x = 42 and y = 43]
    OCaml: [let x = 42 and y = 43]

    @param vb The value binding(s)
    @return List of OCaml value bindings
    @raise Assert_failure Currently unimplemented *)
and process_val_bind (vb : Ast.val_bind) : Parsetree.value_binding list = match vb with
  | ValBind (pat, exp, rest_opt) -> assert false
  | ValBindRec vb -> assert false

(** Convert SML function bindings to OCaml.

    SML: [fun f 0 = 1 | f n = n * f (n-1)]
    OCaml: [let rec f = function 0 -> 1 | n -> n * f (n-1)]

    @param fb The function binding(s)
    @return List of OCaml value bindings
    @raise Assert_failure Currently unimplemented *)
and process_fun_bind (fb : Ast.fun_bind) : Parsetree.value_binding list = match fb with
  | FunBind (fm, rest_opt) -> assert false

(** Convert SML function match clauses.

    Helper for {!process_fun_bind}.

    @param fm The function match clause(s)
    @return List of pattern-expression pairs
    @raise Assert_failure Currently unimplemented *)
and process_fun_match (fm : Ast.fun_match) : (Parsetree.pattern list * Parsetree.expression) list = match fm with
  | FunMatchPrefix (wo, pats, ty_opt, exp, rest_opt) -> assert false
  | FunMatchInfix (p1, id, p2, ty_opt, exp, rest_opt) -> assert false
  | FunMatchLow (p1, id, p2, pats, ty_opt, exp, rest_opt) -> assert false

(** Convert SML type bindings (type abbreviations).

    SML: [type 'a pair = 'a * 'a]
    OCaml: [type 'a pair = 'a * 'a]

    @param tb The type binding(s)
    @return List of OCaml type declarations
    @raise Assert_failure Currently unimplemented *)
and process_typ_bind (tb : Ast.typ_bind) : Parsetree.type_declaration list = match tb with
  | TypBind (tvars, id, ty, rest_opt) -> assert false

(** Convert SML datatype bindings to OCaml variant types.

    SML: [datatype 'a option = NONE | SOME of 'a]
    OCaml: [type 'a option = None | Some of 'a]

    @param db The datatype binding(s)
    @return List of OCaml type declarations
    @raise Assert_failure Currently unimplemented *)
and process_dat_bind (db : Ast.dat_bind) : Parsetree.type_declaration list = match db with
  | DatBind (tvars, id, cb, rest_opt) -> assert false

(** Convert SML constructor bindings within a datatype.

    @param cb The constructor binding(s)
    @return List of OCaml constructor declarations
    @raise Assert_failure Currently unimplemented *)
and process_con_bind (cb : Ast.con_bind) : Parsetree.constructor_declaration list = match cb with
  | ConBind (id, ty_opt, rest_opt) -> assert false

(** Convert SML exception bindings.

    SML: [exception Empty] or [exception Fail of string]
    OCaml: [exception Empty] or [exception Fail of string]

    @param eb The exception binding(s)
    @return List of OCaml extension constructors
    @raise Assert_failure Currently unimplemented *)
and process_exn_bind (eb : Ast.exn_bind) : Parsetree.extension_constructor list = match eb with
  | ExnBind (id, ty_opt, rest_opt) -> assert false
  | ExnBindAlias (id1, id2, rest_opt) -> assert false

(** Extract identifier from SML [op] prefix wrapper.

    The [op] keyword in SML removes infix status: [op +] is prefix [+].

    @param wo The identifier with or without [op]
    @return The processed identifier
    @raise Assert_failure Currently unimplemented *)
and process_with_op (wo : Ast.with_op) : idx = match wo with
  | WithOp id -> assert false
  | WithoutOp id -> assert false

(** {1 Structure Processing}

    Functions for converting SML structures (modules) to OCaml modules.

    SML structures are first-class modules that can be:
    - Named and bound ([structure S = struct ... end])
    - Annotated with signatures ([S : SIG] or [S :> SIG])
    - Created via functor application ([F(A)])
    - Combined with local declarations *)

(** Convert an SML structure expression to OCaml module items.

    @param str The SML structure to convert
    @return List of OCaml structure items
    @raise Assert_failure Currently unimplemented *)
and process_str (str : Ast.str) : Parsetree.structure_item list = match str with
  | StrIdx id -> assert false
  | StructStr dec -> assert false
  | AnotateStr (id, annot, s) -> assert false
  | FunctorApp (id, s) -> assert false
  | FunctorAppAnonymous (id, dec) -> assert false
  | LocalDec (dec, s) -> assert false

(** Convert SML signature annotation type.

    - Transparent ([:]): Type equalities visible
    - Opaque ([:>]): Abstract types hidden

    @param a The annotation type
    @return String representation
    @raise Assert_failure Currently unimplemented *)
and process_anotate (a : Ast.anotate) : string = match a with
  | Transparent -> assert false
  | Opaque -> assert false

(** Convert SML structure bindings.

    SML: [structure S = struct ... end]
    OCaml: [module S = struct ... end]

    @param sb The structure binding(s)
    @return List of OCaml module bindings
    @raise Assert_failure Currently unimplemented *)
and process_str_bind (sb : Ast.str_bind) : Parsetree.module_binding list = match sb with
  | StrBind (id, annot_opt, rest_opt) -> assert false

(** {1 Signature Processing}

    Functions for converting SML signatures (module types) to OCaml signatures.

    SML signatures specify the interface of structures, including:
    - Value specifications ([val f : int -> int])
    - Type specifications ([type t], [eqtype t])
    - Datatype specifications
    - Exception specifications
    - Nested structure specifications
    - Sharing constraints ([sharing type t1 = t2]) *)

(** Convert an SML signature to OCaml signature items.

    @param sign The SML signature to convert
    @return List of OCaml signature items
    @raise Assert_failure Currently unimplemented *)
and process_sign (sign : Ast.sign) : Parsetree.signature_item list = match sign with
  | SignIdx id -> assert false
  | SignSig (s, spec) -> assert false
  | SignWhere (s, tr) -> assert false

(** Convert SML type refinement ([where type]) clauses.

    SML: [sig ... end where type t = int]
    OCaml: Uses [with type] constraints

    @param tr The type refinement
    @return List of type identifier-definition pairs
    @raise Assert_failure Currently unimplemented *)
and process_typ_refine (tr : Ast.typ_refine) : (Longident.t * Parsetree.core_type) list = match tr with
  | TypRef (tvars, id, ty, rest_opt) -> assert false

(** Convert SML specifications within signatures.

    @param spec The specification to convert
    @return List of OCaml signature items
    @raise Assert_failure Currently unimplemented *)
and process_spec (spec : Ast.spec) : Parsetree.signature_item list = match spec with
  | SpecVal vd -> assert false
  | SpecTyp td -> assert false
  | SpecEqtyp td -> assert false
  | SpecTypBind tb -> assert false
  | SpecDat dd -> assert false
  | SpecDatAlias (id1, id2) -> assert false
  | SpecExn ed -> assert false
  | SpecStr sd -> assert false
  | SpecSeq (s1, s2) -> assert false
  | SpecInclude s -> assert false
  | SpecIncludeIdx ids -> assert false
  | SpecSharingTyp (s, ids) -> assert false
  | SpecSharingStr (s, ids) -> assert false

(** Convert SML value descriptions in signatures.

    @param vd The value description(s)
    @return List of OCaml value descriptions
    @raise Assert_failure Currently unimplemented *)
and process_val_desc (vd : Ast.val_desc) : Parsetree.value_description list = match vd with
  | ValDesc (id, ty, rest_opt) -> assert false

(** Convert SML abstract type descriptions.

    @param td The type description(s)
    @return List of OCaml type declarations
    @raise Assert_failure Currently unimplemented *)
and process_typ_desc (td : Ast.typ_desc) : Parsetree.type_declaration list = match td with
  | TypDesc (tvars, id, rest_opt) -> assert false

(** Convert SML datatype descriptions in signatures.

    @param dd The datatype description(s)
    @return List of OCaml type declarations
    @raise Assert_failure Currently unimplemented *)
and process_dat_desc (dd : Ast.dat_desc) : Parsetree.type_declaration list = match dd with
  | DatDesc (tvars, id, cd, rest_opt) -> assert false

(** Convert SML constructor descriptions in signatures.

    @param cd The constructor description(s)
    @return List of OCaml constructor declarations
    @raise Assert_failure Currently unimplemented *)
and process_con_desc (cd : Ast.con_desc) : Parsetree.constructor_declaration list = match cd with
  | ConDesc (id, ty_opt, rest_opt) -> assert false

(** Convert SML exception descriptions in signatures.

    @param ed The exception description(s)
    @return List of OCaml extension constructors
    @raise Assert_failure Currently unimplemented *)
and process_exn_desc (ed : Ast.exn_desc) : Parsetree.extension_constructor list = match ed with
  | ExnDesc (id, ty_opt, rest_opt) -> assert false

(** Convert SML structure descriptions in signatures.

    @param sd The structure description(s)
    @return List of OCaml module declarations
    @raise Assert_failure Currently unimplemented *)
and process_str_desc (sd : Ast.str_desc) : Parsetree.module_declaration list = match sd with
  | StrDesc (id, s, rest_opt) -> assert false

(** {1 Program Processing}

    Functions for converting top-level SML programs.

    A program is a sequence of:
    - Core declarations
    - Functor declarations ([functor F(X: S) = ...])
    - Signature declarations ([signature S = ...]) *)

(** Convert a top-level SML program to an OCaml structure.

    @param prog The SML program to convert
    @return An OCaml structure (list of structure items)
    @raise Assert_failure Currently unimplemented *)
and process_prog (prog : Ast.prog) : Parsetree.structure = match prog with
  | ProgDec dec -> assert false
  | ProgFun fb -> assert false
  | ProgStr sb -> assert false
  | ProgSeq (p1, p2) -> assert false
  | ProgEmpty -> assert false

(** Convert SML functor bindings (parameterized modules).

    SML: [functor F(X : SIG) = struct ... end]
    OCaml: [module F(X : SIG) = struct ... end]

    @param fb The functor binding(s)
    @return List of OCaml module bindings
    @raise Assert_failure Currently unimplemented *)
and process_fct_bind (fb : Ast.fct_bind) : Parsetree.module_binding list = match fb with
  | FctBind (name, param, sig1, annot_opt, body, rest_opt) -> assert false
  | FctBindOpen (name, spec, annot_opt, body, rest_opt) -> assert false

(** Convert SML signature bindings.

    SML: [signature SIG = sig ... end]
    OCaml: [module type SIG = sig ... end]

    @param sb The signature binding(s)
    @return List of OCaml module type declarations
    @raise Assert_failure Currently unimplemented *)
and process_sign_bind (sb : Ast.sign_bind) : Parsetree.module_type_declaration list = match sb with
  | SignBind (id, s, rest_opt) -> assert false

let%test_unit "process basic type" = (let _ = process_type (Ast.TypVar (IdxVar "a")) in ())

