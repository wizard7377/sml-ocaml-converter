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


include Helpers
open Common 
module Make(Config : CONFIG) = struct 
  type res
  module Config = Config
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
    @raise Assert_failure For word constants (not supported in OCaml) *)
let rec process_con (con : Ast.con) : Parsetree.constant = match con with
  | ConInt i ->
      (* SML uses ~ for negation, OCaml uses - *)
      let i' = String.map (function '~' -> '-' | c -> c) i in
      Pconst_integer (i', None)
  | ConWord w ->
      (* Words are unsigned integers in SML, not directly supported in OCaml *)
      (* Convert to regular integer, stripping 0w or 0wx prefix *)
      let w' =
        if String.starts_with ~prefix:"0wx" w then
          "0x" ^ String.sub w 3 (String.length w - 3)
        else if String.starts_with ~prefix:"0w" w then
          String.sub w 2 (String.length w - 2)
        else w
      in
      Pconst_integer (w', None)
  | ConFloat r ->
      (* SML uses ~ for negation, OCaml uses - *)
      let r' = String.map (function '~' -> '-' | c -> c) r in
      Pconst_float (r', None)
  | ConChar c ->
      (* SML: #"a", OCaml: 'a' - the string should already be the character *)
      Pconst_char (String.get c 0)
  | ConString s ->
      Pconst_string (s, Location.none, None)

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
  | ExpApp (e1, e2) ->
      let e1' = process_exp e1 in
      let e2' = process_exp e2 in
      Builder.pexp_apply e1' [(Nolabel, e2')]
  | ExpIdx idx ->
      Builder.pexp_ident (ghost (process_longid (process_name ~context:Value idx)))
  | InfixApp (e1, op, e2) ->
      let op' = process_name ~context:Value op in
      Builder.pexp_apply (Builder.pexp_ident (ghost (process_longid op')))
        [(Nolabel, process_exp e1); (Nolabel, process_exp e2)]
  | ParenExp e -> process_exp e
  | TupleExp [] -> Builder.pexp_construct (ghost (Longident.Lident "()")) None
  | TupleExp exps -> Builder.pexp_tuple (List.map process_exp exps)
  | RecordExp rows ->
      let fields = List.map (fun (Ast.Row (lab, exp, _)) ->
        let lab' = process_name ~context:Label lab in
        (ghost (process_longid lab'), process_exp exp)
      ) rows in
      Builder.pexp_record fields None
  | RecordSelector lab ->
      (* #label -> fun r -> r.label *)
      let lab' = process_name ~context:Label lab in
      let lab_str = idx_to_string lab' in
      let r_pat = Builder.ppat_var (ghost "r") in
      let r_exp = Builder.pexp_ident (ghost (Longident.Lident "r")) in
      let field_exp = Builder.pexp_field r_exp (ghost (Longident.Lident lab_str)) in
      Builder.pexp_fun Nolabel None r_pat field_exp
  | ListExp exps ->
      (* Build list from right to left using :: *)
      List.fold_right (fun e acc ->
        Builder.pexp_construct (ghost (Longident.Lident "::"))
          (Some (Builder.pexp_tuple [process_exp e; acc]))
      ) exps (Builder.pexp_construct (ghost (Longident.Lident "[]")) None)
  | SeqExp exps ->
      (* Build sequence expression from list *)
      let rec build_seq = function
        | [] -> Builder.pexp_construct (ghost (Longident.Lident "()")) None
        | [e] -> process_exp e
        | e :: rest -> Builder.pexp_sequence (process_exp e) (build_seq rest)
      in
      build_seq exps
  | LetExp (decs, exps) ->
      (* let dec1 dec2 ... in exp1; exp2; ... end *)
      let bindings = List.flatten (List.map process_dec decs) in
      let body =
        let rec build_seq = function
          | [] -> Builder.pexp_construct (ghost (Longident.Lident "()")) None
          | [e] -> process_exp e
          | e :: rest -> Builder.pexp_sequence (process_exp e) (build_seq rest)
        in
        build_seq exps
      in
      List.fold_right (fun binding acc ->
        Builder.pexp_let Nonrecursive [binding] acc
      ) bindings body
  | TypedExp (e, ty) ->
      Builder.pexp_constraint (process_exp e) (process_type ty)
  | RaiseExp e ->
      let e' = process_exp e in
      Builder.pexp_apply (Builder.pexp_ident (ghost (Longident.Lident "raise")))
        [(Nolabel, e')]
  | HandleExp (e, cases) ->
      Builder.pexp_try (process_exp e) (process_matching cases)
  | AndExp (e1, e2) ->
      (* andalso -> && *)
      Builder.pexp_apply (Builder.pexp_ident (ghost (Longident.Lident "&&")))
        [(Nolabel, process_exp e1); (Nolabel, process_exp e2)]
  | OrExp (e1, e2) ->
      (* orelse -> || *)
      Builder.pexp_apply (Builder.pexp_ident (ghost (Longident.Lident "||")))
        [(Nolabel, process_exp e1); (Nolabel, process_exp e2)]
  | IfExp (e1, e2, e3) ->
      Builder.pexp_ifthenelse (process_exp e1) (process_exp e2) (Some (process_exp e3))
  | WhileExp (e1, e2) ->
      Builder.pexp_while (process_exp e1) (process_exp e2)
  | CaseExp (e, cases) ->
      Builder.pexp_match (process_exp e) (process_matching cases)
  | FnExp cases ->
      (* fn match -> function ... *)
      Builder.pexp_function (process_matching cases)

(** {2 Expression Rows and Matching}

    Helper functions for expression-related constructs. *)

(** Convert an SML expression row (record field) to an OCaml record field.

    SML: [{x = 1, y = 2}]
    OCaml: [{x = 1; y = 2}]

    @param row The expression row (field binding)
    @return A pair of field identifier and expression *)
and process_row (row : Ast.row) : (Longident.t Location.loc * Parsetree.expression) = match row with
  | Row (lab, exp, rest_opt) ->
      let lab' = process_name ~context:Label lab in
      (ghost (process_longid lab'), process_exp exp)

(** Convert SML match clauses to OCaml case list.

    SML: [pat1 => exp1 | pat2 => exp2]
    OCaml: [| pat1 -> exp1 | pat2 -> exp2]

    Used in [case], [fn], and [handle] expressions.

    @param m The match clause(s)
    @return A list of OCaml case expressions *)
and process_matching (m : Ast.matching) : Parsetree.case list = match m with
  | Case (pat, exp, rest_opt) ->
      let case_here = Builder.case ~lhs:(process_pat pat) ~guard:None ~rhs:(process_exp exp) in
      begin match rest_opt with
      | None -> [case_here]
      | Some rest -> case_here :: process_matching rest
      end

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
and process_pat ?(is_head=false) (pat : Ast.pat) : Parsetree.pattern = match pat with
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
  | PatApp (wo, p) ->
      (* Constructor application: SOME x *)
      let const_name = process_with_op wo in
      let arg_pat = process_pat p in
      Builder.ppat_construct (ghost (process_longid const_name)) (Some arg_pat)
  | PatInfix (p1, id, p2) ->
      (* Infix constructor pattern: x :: xs *)
      let op_name = process_name ~context:Constructor id in
      let p1' = process_pat p1 in
      let p2' = process_pat p2 in
      Builder.ppat_construct (ghost (process_longid op_name))
        (Some (Builder.ppat_tuple [p1'; p2']))
  | PatParen p -> process_pat p
  | PatTuple [] -> Builder.ppat_construct (ghost (Longident.Lident "()")) None
  | PatTuple ps -> Builder.ppat_tuple (List.map process_pat ps)
  | PatRecord rows ->
      let fields = List.flatten (List.map process_pat_row rows) in
      Builder.ppat_record (List.map (fun (lab, pat) ->
        (ghost (Longident.Lident lab), pat)
      ) fields) Closed
  | PatList pats ->
      (* Build list pattern from right to left *)
      List.fold_right (fun p acc ->
        Builder.ppat_construct (ghost (Longident.Lident "::"))
          (Some (Builder.ppat_tuple [process_pat p; acc]))
      ) pats (Builder.ppat_construct (ghost (Longident.Lident "[]")) None)
  | PatTyp (p, t) ->
      Builder.ppat_constraint (process_pat p) (process_type t)
  | PatAs (wo, t_opt, p) ->
      (* Layered pattern: x as SOME y *)
      let var_name = process_with_op wo in
      let var_str = idx_to_string var_name in
      let inner_pat = process_pat p in
      let final_pat = match t_opt with
        | None -> inner_pat
        | Some ty -> Builder.ppat_constraint inner_pat (process_type ty)
      in
      Builder.ppat_alias final_pat (ghost var_str)

(** Convert SML pattern rows (record pattern fields) to OCaml record patterns.

    SML record patterns have three forms:
    - Wildcard: [{..., x, y}] matches any record with at least x and y fields
    - Simple: [{x = px, y = py}] binds px and py
    - Variable shorthand: [{x, y}] is sugar for [{x = x, y = y}]

    @param row The pattern row to convert
    @return A list of field-pattern pairs
    @raise Assert_failure Currently unimplemented *)
and process_pat_row (row : Ast.pat_row) : (string * Parsetree.pattern) list = match row with
  | PatRowPoly ->
      (* Wildcard row - matches remaining fields *)
      []
  | PatRowSimple (lab, pat, rest) ->
      let lab' = process_name ~context:Label lab in
      let lab_str = idx_to_string lab' in
      let pat' = process_pat pat in
      let here = (lab_str, pat') in
      begin match rest with
      | PatRowPoly -> [here]
      | other -> here :: process_pat_row other
      end
  | PatRowVar (id, ty_opt, as_opt, rest_opt) ->
      (* {x, y} is shorthand for {x = x, y = y} *)
      let id' = process_name ~context:Label id in
      let id_str = idx_to_string id' in
      let var_pat = Builder.ppat_var (ghost id_str) in
      let pat_with_type = match ty_opt with
        | None -> var_pat
        | Some ty -> Builder.ppat_constraint var_pat (process_type ty)
      in
      let final_pat = match as_opt with
        | None -> pat_with_type
        | Some as_id ->
            let as_name = process_name ~context:PatternVar as_id in
            Builder.ppat_alias pat_with_type (ghost (idx_to_string as_name))
      in
      let here = (id_str, final_pat) in
      begin match rest_opt with
      | None -> [here]
      | Some rest -> here :: process_pat_row rest
      end

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

    Note: This function returns value_binding list, but some declarations
    (like type declarations) don't produce value bindings. Those return empty lists.
    For full program conversion, use process_prog which returns structure items.

    @param dec The SML declaration to convert
    @return A list of OCaml value bindings *)
and process_dec (dec : Ast.dec) : Parsetree.value_binding list = match dec with
  | ValDec (tvars, vb) ->
      (* Type variables in 'val' are currently ignored in conversion *)
      process_val_bind vb
  | FunDec fb ->
      process_fun_bind fb
  | TypDec tb ->
      (* Type declarations don't produce value bindings *)
      (* They should be handled at the structure level *)
      []
  | DatDec (db, tb_opt) ->
      (* Datatype declarations don't produce value bindings *)
      (* They should be handled at the structure level *)
      []
  | DataDecAlias (id1, id2) ->
      (* Datatype alias - no value bindings *)
      []
  | AbstractDec (db, tb_opt, decs) ->
      (* Abstract type declarations *)
      (* Process the inner declarations *)
      List.concat (List.map process_dec decs)
  | ExnDec eb ->
      (* Exception declarations don't produce value bindings *)
      []
  | StrDec sb ->
      (* Structure declarations don't produce value bindings *)
      []
  | SeqDec decs ->
      (* Sequential declarations - process each and concatenate *)
      List.concat (List.map process_dec decs)
  | LocalDec (d1, d2) ->
      (* Local declarations - both parts contribute to bindings *)
      process_dec d1 @ process_dec d2
  | OpenDec ids ->
      (* Open declarations don't produce value bindings *)
      []
  | FixityDec (fix, ids) ->
      (* Fixity declarations don't produce value bindings *)
      []

(** Convert SML fixity declarations to string representation.

    SML fixity: [infix 6 +], [infixr 5 ::], [nonfix f]

    @param fix The fixity specification
    @return String representation of fixity *)
and process_fixity (fix : Ast.fixity) : string = match fix with
  | Nonfix -> "nonfix"
  | Infix n -> Printf.sprintf "infix %d" n
  | Infixr n -> Printf.sprintf "infixr %d" n

(** Convert SML value bindings.

    SML: [val x = 42 and y = 43]
    OCaml: [let x = 42 and y = 43]

    @param vb The value binding(s)
    @return List of OCaml value bindings *)
and process_val_bind (vb : Ast.val_bind) : Parsetree.value_binding list = match vb with
  | ValBind (pat, exp, rest_opt) ->
      let pat' = process_pat pat in
      let exp' = process_exp exp in
      let binding = Builder.value_binding ~pat:pat' ~expr:exp' in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_val_bind r
      in
      binding :: rest
  | ValBindRec vb ->
      (* Recursive value bindings *)
      process_val_bind vb

(** Convert SML function bindings to OCaml.

    SML: [fun f 0 = 1 | f n = n * f (n-1)]
    OCaml: [let rec f = function 0 -> 1 | n -> n * f (n-1)]

    @param fb The function binding(s)
    @return List of OCaml value bindings *)
and process_fun_bind (fb : Ast.fun_bind) : Parsetree.value_binding list = match fb with
  | FunBind (fm, rest_opt) ->
      (* Get the function name from the first match *)
      let fname = match fm with
        | FunMatchPrefix (wo, _, _, _, _) -> process_with_op wo
        | FunMatchInfix (_, id, _, _, _, _) -> process_name ~context:Value id
        | FunMatchLow (_, id, _, _, _, _, _) -> process_name ~context:Value id
      in
      let fname_str = idx_to_string fname in

      (* Process all match clauses *)
      let clauses = process_fun_match fm in

      (* Build the function body *)
      let body = match clauses with
        | [] -> failwith "Function with no clauses"
        | [(pats, exp)] ->
            (* Single clause - build nested lambdas *)
            List.fold_right (fun pat acc ->
              Builder.pexp_fun Nolabel None pat acc
            ) pats exp
        | _ ->
            (* Multiple clauses - need pattern matching *)
            (* All clauses should have same number of parameters *)
            let num_params = match clauses with
              | (pats, _) :: _ -> List.length pats
              | [] -> 0
            in
            (* Generate fresh parameter patterns *)
            let param_pats = List.init num_params (fun i ->
              Builder.ppat_var (ghost (Printf.sprintf "arg%d" i))
            ) in
            (* Build function expression *)
            List.fold_right (fun pat acc ->
              Builder.pexp_fun Nolabel None pat acc
            ) param_pats (
              (* Build match expression on tuple of arguments *)
              let match_exp = Builder.pexp_tuple (
                List.init num_params (fun i ->
                  Builder.pexp_ident (ghost (Longident.Lident (Printf.sprintf "arg%d" i)))
                )
              ) in
              let cases = List.map (fun (pats, exp) ->
                let pat = Builder.ppat_tuple pats in
                Builder.case ~lhs:pat ~guard:None ~rhs:exp
              ) clauses in
              Builder.pexp_match match_exp cases
            )
      in

      let pat = Builder.ppat_var (ghost fname_str) in
      let binding = Builder.value_binding ~pat ~expr:body in

      let rest = match rest_opt with
        | None -> []
        | Some r -> process_fun_bind r
      in
      binding :: rest

(** Convert SML function match clauses.

    Helper for {!process_fun_bind}.

    @param fm The function match clause(s)
    @return List of pattern-expression pairs *)
and process_fun_match (fm : Ast.fun_match) : (Parsetree.pattern list * Parsetree.expression) list = match fm with
  | FunMatchPrefix (wo, pats, ty_opt, exp, rest_opt) ->
      (* fun f pat1 pat2 ... = exp *)
      let pats' = List.map process_pat pats in
      let exp' = process_exp exp in
      let exp_with_type = match ty_opt with
        | None -> exp'
        | Some ty -> Builder.pexp_constraint exp' (process_type ty)
      in
      let here = (pats', exp_with_type) in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_fun_match r
      in
      here :: rest
  | FunMatchInfix (p1, id, p2, ty_opt, exp, rest_opt) ->
      (* fun p1 op p2 = exp - infix function *)
      let p1' = process_pat p1 in
      let p2' = process_pat p2 in
      let exp' = process_exp exp in
      let exp_with_type = match ty_opt with
        | None -> exp'
        | Some ty -> Builder.pexp_constraint exp' (process_type ty)
      in
      let here = ([p1'; p2'], exp_with_type) in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_fun_match r
      in
      here :: rest
  | FunMatchLow (p1, id, p2, pats, ty_opt, exp, rest_opt) ->
      (* fun (p1 op p2) pat3 ... = exp - curried infix *)
      let p1' = process_pat p1 in
      let p2' = process_pat p2 in
      let pats' = List.map process_pat pats in
      let all_pats = p1' :: p2' :: pats' in
      let exp' = process_exp exp in
      let exp_with_type = match ty_opt with
        | None -> exp'
        | Some ty -> Builder.pexp_constraint exp' (process_type ty)
      in
      let here = (all_pats, exp_with_type) in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_fun_match r
      in
      here :: rest

(** Convert SML type bindings (type abbreviations).

    SML: [type 'a pair = 'a * 'a]
    OCaml: [type 'a pair = 'a * 'a]

    @param tb The type binding(s)
    @return List of OCaml type declarations *)
and process_typ_bind (tb : Ast.typ_bind) : Parsetree.type_declaration list = match tb with
  | TypBind (tvars, id, ty, rest_opt) ->
      let name = process_name ~context:ValueType id in
      let name_str = idx_to_string name in
      let params = List.map (fun tv ->
        let tv_name = process_name ~context:TypeVar tv in
        match tv_name with
        | Var v -> (Builder.ptyp_var v, (Asttypes.NoVariance, Asttypes.NoInjectivity))
        | _ -> failwith "Type variable expected"
      ) tvars in
      let manifest = Some (process_type ty) in
      let tdecl = Builder.type_declaration
        ~name:(ghost name_str)
        ~params
        ~cstrs:[]
        ~kind:Parsetree.Ptype_abstract
        ~private_:Asttypes.Public
        ~manifest
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_typ_bind r
      in
      tdecl :: rest

(** Convert SML datatype bindings to OCaml variant types.

    SML: [datatype 'a option = NONE | SOME of 'a]
    OCaml: [type 'a option = None | Some of 'a]

    @param db The datatype binding(s)
    @return List of OCaml type declarations *)
and process_dat_bind (db : Ast.dat_bind) : Parsetree.type_declaration list = match db with
  | DatBind (tvars, id, cb, rest_opt) ->
      let name = process_name ~context:ValueType id in
      let name_str = idx_to_string name in
      let params = List.map (fun tv ->
        let tv_name = process_name ~context:TypeVar tv in
        match tv_name with
        | Var v -> (Builder.ptyp_var v, (Asttypes.NoVariance, Asttypes.NoInjectivity))
        | _ -> failwith "Type variable expected"
      ) tvars in
      let constructors = process_con_bind cb in
      let tdecl = Builder.type_declaration
        ~name:(ghost name_str)
        ~params
        ~cstrs:[]
        ~kind:(Parsetree.Ptype_variant constructors)
        ~private_:Asttypes.Public
        ~manifest:None
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_dat_bind r
      in
      tdecl :: rest

(** Convert SML constructor bindings within a datatype.

    @param cb The constructor binding(s)
    @return List of OCaml constructor declarations *)
and process_con_bind (cb : Ast.con_bind) : Parsetree.constructor_declaration list = match cb with
  | ConBind (id, ty_opt, rest_opt) ->
      let name = process_name ~context:Constructor id in
      let name_str = idx_to_string name in
      let args = match ty_opt with
        | None -> Parsetree.Pcstr_tuple []
        | Some ty -> Parsetree.Pcstr_tuple [process_type ty]
      in
      let cdecl = Builder.constructor_declaration
        ~name:(ghost name_str)
        ~args
        ~res:None
      in
      let rest = match rest_opt with
        | None -> []
        | Some rest -> process_con_bind rest
      in
      cdecl :: rest

(** Convert SML exception bindings.

    SML: [exception Empty] or [exception Fail of string]
    OCaml: [exception Empty] or [exception Fail of string]

    @param eb The exception binding(s)
    @return List of OCaml extension constructors *)
and process_exn_bind (eb : Ast.exn_bind) : Parsetree.extension_constructor list = match eb with
  | ExnBind (id, ty_opt, rest_opt) ->
      let name = process_name ~context:Constructor id in
      let name_str = idx_to_string name in
      let args = match ty_opt with
        | None -> Parsetree.Pcstr_tuple []
        | Some ty -> Parsetree.Pcstr_tuple [process_type ty]
      in
      let ext_constr = Builder.extension_constructor
        ~name:(ghost name_str)
        ~kind:(Parsetree.Pext_decl ([], args, None))
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_exn_bind r
      in
      ext_constr :: rest
  | ExnBindAlias (id1, id2, rest_opt) ->
      let name1 = process_name ~context:Constructor id1 in
      let name2 = process_name ~context:Constructor id2 in
      let name1_str = idx_to_string name1 in
      let longid2 = process_longid name2 in
      let ext_constr = Builder.extension_constructor
        ~name:(ghost name1_str)
        ~kind:(Parsetree.Pext_rebind (ghost longid2))
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_exn_bind r
      in
      ext_constr :: rest

(** Extract identifier from SML [op] prefix wrapper.

    The [op] keyword in SML removes infix status: [op +] is prefix [+].

    @param wo The identifier with or without [op]
    @return The processed identifier *)
and process_with_op (wo : Ast.with_op) : idx = match wo with
  | WithOp id -> process_name ~context:Constructor id
  | WithoutOp id -> process_name ~context:Constructor id

(** {1 Structure Processing}

    Functions for converting SML structures (modules) to OCaml modules.

    SML structures are first-class modules that can be:
    - Named and bound ([structure S = struct ... end])
    - Annotated with signatures ([S : SIG] or [S :> SIG])
    - Created via functor application ([F(A)])
    - Combined with local declarations *)

(** Convert an SML structure expression to OCaml module items.

    Note: This function returns structure items (module contents) for cases
    where the structure can be inlined. For structure references and functor
    applications, this may not be fully accurate.

    @param str The SML structure to convert
    @return List of OCaml structure items *)
and process_str (str : Ast.str) : Parsetree.structure_item list = match str with
  | StrIdx _id ->
      (* Structure reference - can't inline, return empty *)
      (* This case should ideally return a module expression instead *)
      []
  | StructStr dec ->
      (* struct dec end - convert declarations to structure items *)
      dec_to_structure_items dec
  | AnotateStr (_id, _annot, s) ->
      (* Annotated structure - just process the inner structure *)
      (* The annotation would be handled at binding site *)
      process_str s
  | FunctorApp (_id, _s) ->
      (* Functor application - can't inline *)
      []
  | FunctorAppAnonymous (_id, dec) ->
      (* Functor applied to anonymous struct *)
      dec_to_structure_items dec
  | LocalDec (dec, s) ->
      (* Local declarations in structure *)
      dec_to_structure_items dec @ process_str s

(** Convert SML signature annotation type.

    - Transparent ([:]): Type equalities visible
    - Opaque ([:>]): Abstract types hidden

    @param a The annotation type
    @return String representation *)
and process_anotate (a : Ast.anotate) : string = match a with
  | Transparent -> ":"
  | Opaque -> ":>"

(** Convert SML structure bindings.

    SML: [structure S = struct ... end]
    OCaml: [module S = struct ... end]

    Note: The AST for StrBind appears incomplete (missing structure body),
    so this creates a module with empty structure.

    @param sb The structure binding(s)
    @return List of OCaml module bindings *)
and process_str_bind (sb : Ast.str_bind) : Parsetree.module_binding list = match sb with
  | StrBind (id, annot_opt, rest_opt) ->
      let name = process_name ~context:Module id in
      let name_str = idx_to_string name in
      (* Create empty module since AST doesn't include structure body *)
      let module_expr = Builder.pmod_structure [] in
      let module_expr_with_sig = match annot_opt with
        | None -> module_expr
        | Some (annot, sign) ->
            let sig_items = process_sign sign in
            let module_type = Builder.pmty_signature sig_items in
            match annot with
            | Transparent -> Builder.pmod_constraint module_expr module_type
            | Opaque -> Builder.pmod_constraint module_expr module_type
      in
      let binding = Builder.module_binding ~name:(ghost (Some name_str)) ~expr:module_expr_with_sig in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_str_bind r
      in
      binding :: rest

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
    @return List of OCaml signature items *)
and process_sign (sign : Ast.sign) : Parsetree.signature_item list = match sign with
  | SignIdx _id ->
      (* Signature identifier - can't inline, return empty *)
      (* This should ideally be handled at module type level *)
      []
  | SignSig (_s, spec) ->
      (* sig spec end - process specifications *)
      process_spec spec
  | SignWhere (s, _tr) ->
      (* Signature with where clauses *)
      (* Where clauses should be handled at module type level *)
      (* For now, just process the base signature *)
      process_sign s

(** Convert SML type refinement ([where type]) clauses.

    SML: [sig ... end where type t = int]
    OCaml: Uses [with type] constraints

    @param tr The type refinement
    @return List of type identifier-definition pairs *)
and process_typ_refine (tr : Ast.typ_refine) : (Longident.t * Parsetree.core_type) list = match tr with
  | TypRef (_tvars, id, ty, rest_opt) ->
      (* Type variables are currently ignored in refinement *)
      let name = process_name ~context:ValueType id in
      let longid = process_longid name in
      let core_type = process_type ty in
      let here = (longid, core_type) in
      let rest = match rest_opt with
        | None -> []
        | Some (_ty, tr_rest) -> process_typ_refine tr_rest
      in
      here :: rest

(** Convert SML specifications within signatures.

    @param spec The specification to convert
    @return List of OCaml signature items *)
and process_spec (spec : Ast.spec) : Parsetree.signature_item list = match spec with
  | SpecVal vd ->
      let vdescs = process_val_desc vd in
      List.map (fun vd -> Builder.psig_value vd) vdescs
  | SpecTyp td ->
      let tdecls = process_typ_desc td in
      [Builder.psig_type Asttypes.Nonrecursive tdecls]
  | SpecEqtyp td ->
      (* Equality types - in OCaml just abstract types *)
      let tdecls = process_typ_desc td in
      [Builder.psig_type Asttypes.Nonrecursive tdecls]
  | SpecTypBind tb ->
      let tdecls = process_typ_bind tb in
      [Builder.psig_type Asttypes.Nonrecursive tdecls]
  | SpecDat dd ->
      let tdecls = process_dat_desc dd in
      [Builder.psig_type Asttypes.Nonrecursive tdecls]
  | SpecDatAlias (id1, id2) ->
      (* Datatype alias in signature *)
      let name1 = process_name ~context:ValueType id1 in
      let name2 = process_name ~context:ValueType id2 in
      let name1_str = idx_to_string name1 in
      let longid2 = process_longid name2 in
      let alias_type = Builder.ptyp_constr (ghost longid2) [] in
      let tdecl = Builder.type_declaration
        ~name:(ghost name1_str)
        ~params:[]
        ~cstrs:[]
        ~kind:Parsetree.Ptype_abstract
        ~private_:Asttypes.Public
        ~manifest:(Some alias_type)
      in
      [Builder.psig_type Asttypes.Nonrecursive [tdecl]]
  | SpecExn ed ->
      let ext_constrs = process_exn_desc ed in
      List.map (fun ec ->
        let type_exn = Builder.type_exception ec in
        Builder.psig_exception type_exn
      ) ext_constrs
  | SpecStr sd ->
      let mdecls = process_str_desc sd in
      List.map (fun md -> Builder.psig_module md) mdecls
  | SpecSeq (s1, s2) ->
      process_spec s1 @ process_spec s2
  | SpecInclude s ->
      let sig_items = process_sign s in
      let module_type = Builder.pmty_signature sig_items in
      [Builder.psig_include (Builder.include_infos module_type)]
  | SpecIncludeIdx ids ->
      List.concat (List.map (fun id ->
        let name = process_name ~context:ModuleType id in
        let longid = process_longid name in
        let module_type = Builder.pmty_ident (ghost longid) in
        [Builder.psig_include (Builder.include_infos module_type)]
      ) ids)
  | SpecSharingTyp (_s, _ids) ->
      (* Sharing type constraints - complex, skip for now *)
      []
  | SpecSharingStr (_s, _ids) ->
      (* Sharing structure constraints - complex, skip for now *)
      []

(** Convert SML value descriptions in signatures.

    @param vd The value description(s)
    @return List of OCaml value descriptions *)
and process_val_desc (vd : Ast.val_desc) : Parsetree.value_description list = match vd with
  | ValDesc (id, ty, rest_opt) ->
      let name = process_name ~context:Value id in
      let name_str = idx_to_string name in
      let core_type = process_type ty in
      let vdesc = Builder.value_description
        ~name:(ghost name_str)
        ~type_:core_type
        ~prim:[]
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_val_desc r
      in
      vdesc :: rest

(** Convert SML abstract type descriptions.

    @param td The type description(s)
    @return List of OCaml type declarations *)
and process_typ_desc (td : Ast.typ_desc) : Parsetree.type_declaration list = match td with
  | TypDesc (tvars, id, rest_opt) ->
      let name = process_name ~context:ValueType id in
      let name_str = idx_to_string name in
      let params = List.map (fun tv ->
        let tv_name = process_name ~context:TypeVar tv in
        match tv_name with
        | Var v -> (Builder.ptyp_var v, (Asttypes.NoVariance, Asttypes.NoInjectivity))
        | _ -> failwith "Type variable expected"
      ) tvars in
      let tdecl = Builder.type_declaration
        ~name:(ghost name_str)
        ~params
        ~cstrs:[]
        ~kind:Parsetree.Ptype_abstract
        ~private_:Asttypes.Public
        ~manifest:None
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_typ_desc r
      in
      tdecl :: rest

(** Convert SML datatype descriptions in signatures.

    @param dd The datatype description(s)
    @return List of OCaml type declarations *)
and process_dat_desc (dd : Ast.dat_desc) : Parsetree.type_declaration list = match dd with
  | DatDesc (tvars, id, cd, rest_opt) ->
      let name = process_name ~context:ValueType id in
      let name_str = idx_to_string name in
      let params = List.map (fun tv ->
        let tv_name = process_name ~context:TypeVar tv in
        match tv_name with
        | Var v -> (Builder.ptyp_var v, (Asttypes.NoVariance, Asttypes.NoInjectivity))
        | _ -> failwith "Type variable expected"
      ) tvars in
      let constructors = process_con_desc cd in
      let tdecl = Builder.type_declaration
        ~name:(ghost name_str)
        ~params
        ~cstrs:[]
        ~kind:(Parsetree.Ptype_variant constructors)
        ~private_:Asttypes.Public
        ~manifest:None
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_dat_desc r
      in
      tdecl :: rest

(** Convert SML constructor descriptions in signatures.

    @param cd The constructor description(s)
    @return List of OCaml constructor declarations *)
and process_con_desc (cd : Ast.con_desc) : Parsetree.constructor_declaration list = match cd with
  | ConDesc (id, ty_opt, rest_opt) ->
      (* Same as process_con_bind *)
      let name = process_name ~context:Constructor id in
      let name_str = idx_to_string name in
      let args = match ty_opt with
        | None -> Parsetree.Pcstr_tuple []
        | Some ty -> Parsetree.Pcstr_tuple [process_type ty]
      in
      let cdecl = Builder.constructor_declaration
        ~name:(ghost name_str)
        ~args
        ~res:None
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_con_desc r
      in
      cdecl :: rest

(** Convert SML exception descriptions in signatures.

    @param ed The exception description(s)
    @return List of OCaml extension constructors *)
and process_exn_desc (ed : Ast.exn_desc) : Parsetree.extension_constructor list = match ed with
  | ExnDesc (id, ty_opt, rest_opt) ->
      (* Similar to process_exn_bind but for signatures *)
      let name = process_name ~context:Constructor id in
      let name_str = idx_to_string name in
      let args = match ty_opt with
        | None -> Parsetree.Pcstr_tuple []
        | Some ty -> Parsetree.Pcstr_tuple [process_type ty]
      in
      let ext_constr = Builder.extension_constructor
        ~name:(ghost name_str)
        ~kind:(Parsetree.Pext_decl ([], args, None))
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_exn_desc r
      in
      ext_constr :: rest

(** Convert SML structure descriptions in signatures.

    @param sd The structure description(s)
    @return List of OCaml module declarations *)
and process_str_desc (sd : Ast.str_desc) : Parsetree.module_declaration list = match sd with
  | StrDesc (id, s, rest_opt) ->
      let name = process_name ~context:Module id in
      let name_str = idx_to_string name in
      let sig_items = process_sign s in
      let module_type = Builder.pmty_signature sig_items in
      let mdecl = Builder.module_declaration ~name:(ghost (Some name_str)) ~type_:module_type in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_str_desc r
      in
      mdecl :: rest

(** {1 Program Processing}

    Functions for converting top-level SML programs.

    A program is a sequence of:
    - Core declarations
    - Functor declarations ([functor F(X: S) = ...])
    - Signature declarations ([signature S = ...]) *)

(** Convert a declaration to structure items.
    Helper function for process_prog. *)
and dec_to_structure_items (dec : Ast.dec) : Parsetree.structure_item list = match dec with
  | ValDec (tvars, vb) ->
      let bindings = process_val_bind vb in
      List.map (fun binding ->
        Builder.pstr_value Asttypes.Nonrecursive [binding]
      ) bindings
  | FunDec fb ->
      let bindings = process_fun_bind fb in
      List.map (fun binding ->
        Builder.pstr_value Asttypes.Recursive [binding]
      ) bindings
  | TypDec tb ->
      let tdecls = process_typ_bind tb in
      [Builder.pstr_type Asttypes.Nonrecursive tdecls]
  | DatDec (db, tb_opt) ->
      let tdecls = process_dat_bind db in
      let type_item = Builder.pstr_type Asttypes.Nonrecursive tdecls in
      begin match tb_opt with
      | None -> [type_item]
      | Some tb ->
          let tb_decls = process_typ_bind tb in
          [type_item; Builder.pstr_type Asttypes.Nonrecursive tb_decls]
      end
  | DataDecAlias (id1, id2) ->
      (* Datatype alias: datatype t = datatype u *)
      (* In OCaml, this would be: type t = u *)
      let name1 = process_name ~context:ValueType id1 in
      let name2 = process_name ~context:ValueType id2 in
      let name1_str = idx_to_string name1 in
      let longid2 = process_longid name2 in
      let alias_type = Builder.ptyp_constr (ghost longid2) [] in
      let tdecl = Builder.type_declaration
        ~name:(ghost name1_str)
        ~params:[]
        ~cstrs:[]
        ~kind:Parsetree.Ptype_abstract
        ~private_:Asttypes.Public
        ~manifest:(Some alias_type)
      in
      [Builder.pstr_type Asttypes.Nonrecursive [tdecl]]
  | AbstractDec (db, tb_opt, decs) ->
      (* Abstract type with local implementations *)
      (* The datatype is abstract, only the inner decs are visible *)
      List.concat (List.map dec_to_structure_items decs)
  | ExnDec eb ->
      let exn_constrs = process_exn_bind eb in
      List.map (fun ec ->
        let type_exn = Builder.type_exception ec in
        Builder.pstr_exception type_exn
      ) exn_constrs
  | StrDec sb ->
      let module_bindings = process_str_bind sb in
      List.map (fun mb ->
        Builder.pstr_module mb
      ) module_bindings
  | SeqDec decs ->
      List.concat (List.map dec_to_structure_items decs)
  | LocalDec (d1, d2) ->
      (* Local declarations - both visible at top level in OCaml *)
      dec_to_structure_items d1 @ dec_to_structure_items d2
  | OpenDec ids ->
      List.map (fun id ->
        let name = process_name ~context:Module id in
        let longid = process_longid name in
        let module_expr = Builder.pmod_ident (ghost longid) in
        Builder.pstr_open (Builder.open_infos ~override:Asttypes.Fresh ~expr:module_expr)
      ) ids
  | FixityDec (_fix, _ids) ->
      (* Fixity declarations don't have direct OCaml equivalent *)
      (* Skip them in the output *)
      []

(** Convert a top-level SML program to an OCaml structure.

    @param prog The SML program to convert
    @return An OCaml structure (list of structure items) *)
and process_prog (prog : Ast.prog) : Parsetree.structure = match prog with
  | ProgDec dec ->
      dec_to_structure_items dec
  | ProgFun fb ->
      let module_bindings = process_fct_bind fb in
      List.map (fun mb ->
        Builder.pstr_module mb
      ) module_bindings
  | ProgStr sb ->
      let mtdecls = process_sign_bind sb in
      List.map (fun mtd ->
        Builder.pstr_modtype mtd
      ) mtdecls
  | ProgSeq (p1, p2) ->
      process_prog p1 @ process_prog p2
  | ProgEmpty ->
      []

(** Convert SML functor bindings (parameterized modules).

    SML: [functor F(X : SIG) = struct ... end]
    OCaml: [module F(X : SIG) = struct ... end]

    @param fb The functor binding(s)
    @return List of OCaml module bindings *)
and process_fct_bind (fb : Ast.fct_bind) : Parsetree.module_binding list = match fb with
  | FctBind (name, param, sig1, annot_opt, body, rest_opt) ->
      let fname = process_name ~context:Module name in
      let fname_str = idx_to_string fname in
      let pname = process_name ~context:Module param in
      let pname_str = idx_to_string pname in

      (* Process parameter signature *)
      let param_sig_items = process_sign sig1 in
      let param_module_type = Builder.pmty_signature param_sig_items in

      (* Process functor body *)
      let body_items = process_str body in
      let body_module_expr = Builder.pmod_structure body_items in

      (* Add result signature constraint if present *)
      let final_body = match annot_opt with
        | None -> body_module_expr
        | Some (_annot, result_sig) ->
            let result_sig_items = process_sign result_sig in
            let result_module_type = Builder.pmty_signature result_sig_items in
            Builder.pmod_constraint body_module_expr result_module_type
      in

      (* Create functor *)
      let functor_param = Parsetree.Named (ghost (Some pname_str), param_module_type) in
      let functor_expr = Builder.pmod_functor functor_param final_body in

      let binding = Builder.module_binding
        ~name:(ghost (Some fname_str))
        ~expr:functor_expr
      in

      let rest = match rest_opt with
        | None -> []
        | Some r -> process_fct_bind r
      in
      binding :: rest
  | FctBindOpen (name, spec, annot_opt, body, rest_opt) ->
      (* Opened functor - parameter spec is directly visible *)
      let fname = process_name ~context:Module name in
      let fname_str = idx_to_string fname in

      (* Process parameter spec *)
      let param_sig_items = process_spec spec in
      let param_module_type = Builder.pmty_signature param_sig_items in

      (* Process functor body *)
      let body_items = process_str body in
      let body_module_expr = Builder.pmod_structure body_items in

      (* Add result signature constraint if present *)
      let final_body = match annot_opt with
        | None -> body_module_expr
        | Some (_annot, result_sig) ->
            let result_sig_items = process_sign result_sig in
            let result_module_type = Builder.pmty_signature result_sig_items in
            Builder.pmod_constraint body_module_expr result_module_type
      in

      (* Create functor with unit parameter (opened specs) *)
      let functor_param = Parsetree.Named (ghost (Some "()"), param_module_type) in
      let functor_expr = Builder.pmod_functor functor_param final_body in

      let binding = Builder.module_binding
        ~name:(ghost (Some fname_str))
        ~expr:functor_expr
      in

      let rest = match rest_opt with
        | None -> []
        | Some r -> process_fct_bind r
      in
      binding :: rest

(** Convert SML signature bindings.

    SML: [signature SIG = sig ... end]
    OCaml: [module type SIG = sig ... end]

    @param sb The signature binding(s)
    @return List of OCaml module type declarations *)
and process_sign_bind (sb : Ast.sign_bind) : Parsetree.module_type_declaration list = match sb with
  | SignBind (id, s, rest_opt) ->
      let name = process_name ~context:ModuleType id in
      let name_str = idx_to_string name in
      let sig_items = process_sign s in
      let module_type = Builder.pmty_signature sig_items in
      let mtdecl = Builder.module_type_declaration
        ~name:(ghost name_str)
        ~type_:(Some module_type)
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_sign_bind r
      in
      mtdecl :: rest

let%test_unit "process basic type" = (let _ = process_type (Ast.TypVar (IdxVar "a")) in ())

    end

