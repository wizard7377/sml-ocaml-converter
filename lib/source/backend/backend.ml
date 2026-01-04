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
module Debug = Ppxlib.Pprintast
module Make (Config : CONFIG) = struct
  
type res = Parsetree.toplevel_phrase list

module Config = Config 
  module Names = Process_names.Make(Config)
  exception BadAst of string
  let current_names : Names.t ref = ref (Names.init ())
(** Helper function to create a located value with no source location.

    @param v The value to wrap with a phantom location
    @return The value wrapped in a {!Location.loc} structure *)
let ghost (v : 'a) : 'a Location.loc = Location.mkloc v Location.none

let depth : int ref = ref 0
let comment_str (s:string) : Parsetree.structure = 
  let attr_specification : Parsetree.structure_item = Builder.pstr_eval (Builder.pexp_constant (Parsetree.Pconst_string (s,Location.none,None (* FIXME ?*) ) )) [] in
  [attr_specification]
let comment_str_name : string Location.loc = ghost "sml_comment"
let process_commented (attach:Parsetree.attributes -> 'a -> 'a) (node:'a Ast.node) : 'a = 
  let value = node.value in
  let comment = node.comments in 
  match comment with
  | [] -> value
  | [""] -> value
  | comments ->
     let comment_value = List.map (fun s -> comment_str s) comments in
     let pays = List.map (fun s' -> Parsetree.PStr s') comment_value in
    let attrs = List.map (fun p -> Builder.attribute ~name:comment_str_name ~payload:p) pays in
        attach attrs value

let trace_part ?(level = 2) ?(ast = "") ?(msg = "") ~value : 'a =
  match Config.config.verbosity with
    Some v when v >= level -> begin (* TODO use level *)
      let indent = !depth in 
      depth := indent + 1;
      Printf.eprintf "[%i] Entering %s: %s\n" indent ast (Re.replace_string ~all:true (Re.compile (Re.rep1 Re.space)) ~by:" " msg);
      let res = value () in
      Printf.eprintf "[%i] Exiting %s: %s\n" indent ast (Re.replace_string ~all:true (Re.compile (Re.rep1 Re.space)) ~by:" " msg); 
      res
    end
    | _ -> value ()
let debug_ocaml ~(format:Format.formatter -> 'a -> unit) (node:'a) : unit = match Config.config.verbosity with
    Some v when v > 1 -> format Format.err_formatter node 
   | _ -> ()
(** Convert a string identifier to a Longident.t.
    Handles dotted paths (e.g., "A.B.C" becomes Ldot(Ldot(Lident "A", "B"), "C")) *)
let string_to_longident (s : string) : Longident.t =
  match String.split_on_char '.' s with
  | [] -> Longident.Lident ""
  | [x] -> Longident.Lident x
  | first :: rest ->
      List.fold_left (fun acc part -> Longident.Ldot (acc, part))
        (Longident.Lident first) rest

(** Check if an identifier represents a variable (starts with lowercase or underscore)
    rather than a constructor (starts with uppercase) *)
let is_variable_identifier (s : string) : bool =
  if String.length s = 0 then false
  else
    let first_char = String.get s 0 in
    (first_char >= 'a' && first_char <= 'z') || first_char = '_'

(** Extract a string from an idx value *)
let rec idx_to_string (idx : Ast.idx) : string =
  match idx with
  | Ast.IdxIdx s -> s.value
  | Ast.IdxVar s -> s.value
  | Ast.IdxLab s -> s.value
  | Ast.IdxNum s -> s.value
  | Ast.IdxLong parts ->
      (* Convert long identifier to dot-separated string *)
      String.concat "." (List.map (fun (p : Ast.idx Ast.node) -> idx_to_string p.value) parts)

let process_with_state ?(ctx:context=Empty) (s:string) : string =
  let (new_name, updated_names) = Names.process_name ~ctx !current_names s in
  current_names := updated_names;
  new_name
(** Process a type variable name, preserving the ' prefix *)
let process_type_var_name (s : string) : Parsetree.core_type =
  (* Type variables in SML can be 'a or ''a (equality type vars) *)
  (* Strip leading quotes and process the name *)
  let stripped =
    if String.starts_with ~prefix:"''" s then
      String.sub s 2 (String.length s - 2)
    else if String.starts_with ~prefix:"'" s then
      String.sub s 1 (String.length s - 1)
    else s
  in
  Builder.ptyp_var stripped

(** Main entry point for converting a complete SML program.

    @param prog The SML program to convert
    @return The converted OCaml representation as toplevel phrases

    This is implemented after {!process_prog} in the mutually recursive chain. *)

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
  TypVar name -> (match name.value with
    | Ast.IdxVar v -> process_type_var_name v.value
    | _ -> failwith "Expected type variable")
  | TypCon (args, head) ->
    let head_str = idx_to_string head.value in
    let head' = process_with_state head_str in
    let args' = List.map (fun arg -> process_type_value arg.Ast.value) args in
    Builder.ptyp_constr (ghost (string_to_longident head')) args' 
  
  | TypPar ty -> process_type_value ty.value
  | TypFun (ty1, ty2) -> let ty1', ty2' = process_type_value ty1.value, process_type_value ty2.value in
    Builder.ptyp_arrow Nolabel ty1' ty2'
  | TypTuple tys -> Builder.ptyp_tuple (List.map (fun t -> process_type_value t.Ast.value) tys)
  | TypRecord fields -> let fields' = List.flatten (List.map (fun f -> process_object_field_type f.Ast.value) fields) in
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
  | Ast.TypRow (name, ty, rest) ->
    let label_name = process_with_state (idx_to_string name.value) in
    let here : Parsetree.object_field = Builder.otag (ghost label_name) (process_type_value ty.value) in
    begin match rest with
    | Some rest' -> here :: (process_object_field_type rest'.value)
    | None -> [here]
    end

(** Wrapper function for {!process_type_value}.

    @param ty The SML type to convert
    @return The corresponding OCaml core type *)
let rec process_type (ty : Ast.typ) : Parsetree.core_type = trace_part ~level:2 ~ast:"typ" ~msg:"" (* ~msg:(Ast.show_typ ty) *) ~value:(fun () -> process_type_value ty)

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

    @param constant The SML constant to convert
    @return The corresponding OCaml {!Parsetree.constant}
    @raise Assert_failure For word constants (not supported in OCaml) *)
let rec process_con (constant : Ast.constant) : Parsetree.constant = match constant with
  | ConInt i ->
      (* SML uses ~ for negation, OCaml uses - *)
      let i' = String.map (function '~' -> '-' | c -> c) i.value in
      Pconst_integer (i', None)
  | ConWord w ->
      (* Words are unsigned integers in SML, not directly supported in OCaml *)
      (* Convert to regular integer, stripping 0w or 0wx prefix *)
      let w' =
        if String.starts_with ~prefix:"0wx" w.value then
          "0x" ^ String.sub w.value 3 (String.length w.value - 3)
        else if String.starts_with ~prefix:"0w" w.value then
          String.sub w.value 2 (String.length w.value - 2)
        else w.value
      in
      Pconst_integer (w', None)
  | ConFloat r ->
      (* SML uses ~ for negation, OCaml uses - *)
      let r' = String.map (function '~' -> '-' | c -> c) r.value in
      Pconst_float (r', None)
  | ConChar c ->
      (* SML: #"a", OCaml: 'a' - the string should already be the character *)
      Pconst_char (String.get c.value 0)
  | ConString s ->
      Pconst_string (s.value, Location.none, None)

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

    @param expression The SML expression to convert
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
let rec process_exp (expression : Ast.expression) : Parsetree.expression = match expression with
  | ExpCon c -> Builder.pexp_constant (process_con c.value)
  | ExpApp (e1, e2) ->
      let e1' = process_exp e1.value in
      let e2' = process_exp e2.value in
      Builder.pexp_apply e1' [(Nolabel, e2')]
  | ExpIdx idx ->
      let name = process_with_state (idx_to_string idx.value) in
      Builder.pexp_ident (ghost (string_to_longident name))
  | InfixApp (e1, op, e2) ->
      let op' = process_with_state (idx_to_string op.value) in
      Builder.pexp_apply (Builder.pexp_ident (ghost (string_to_longident op')))
        [(Nolabel, process_exp e1.value); (Nolabel, process_exp e2.value)]
  | ParenExp e -> process_exp e.value
  | TupleExp [] -> Builder.pexp_construct (ghost (Longident.Lident "()")) None
  | TupleExp exps -> Builder.pexp_tuple (List.map (fun e -> process_exp e.Ast.value) exps)
  | RecordExp rows ->
      let fields = List.map (fun r -> match r.Ast.value with Ast.Row (lab, expression, _) ->
        let lab' = process_with_state (idx_to_string lab.value) in
        (ghost (string_to_longident lab'), process_exp expression.value)
      ) rows in
      Builder.pexp_record fields None
  | RecordSelector lab ->
      (* #label -> fun r -> r.label *)
      let lab_str = process_with_state (idx_to_string lab.value) in
      let r_pat = Builder.ppat_var (ghost "r") in
      let r_exp = Builder.pexp_ident (ghost (Longident.Lident "r")) in
      let field_exp = Builder.pexp_field r_exp (ghost (Longident.Lident lab_str)) in
      Builder.pexp_fun Nolabel None r_pat field_exp
  | ListExp exps ->
      (* Build list from right to left using :: *)
      List.fold_right (fun e acc ->
        Builder.pexp_construct (ghost (Longident.Lident "::"))
          (Some (Builder.pexp_tuple [process_exp e.Ast.value; acc]))
      ) exps (Builder.pexp_construct (ghost (Longident.Lident "[]")) None)
  | SeqExp exps ->
      (* Build sequence expression from list *)
      let rec build_seq = function
        | [] -> Builder.pexp_construct (ghost (Longident.Lident "()")) None
        | [e] -> process_exp e.Ast.value
        | e :: rest -> Builder.pexp_sequence (process_exp e.Ast.value) (build_seq rest)
      in
      build_seq exps
  | LetExp (decs, exps) ->
      (* let dec1 dec2 ... in exp1; exp2; ... end *)
      let bindings = List.flatten (List.map (fun d -> process_value_dec d.Ast.value) decs) in
      let body =
        let rec build_seq = function
          | [] -> Builder.pexp_construct (ghost (Longident.Lident "()")) None
          | [e] -> process_exp e.Ast.value
          | e :: rest -> Builder.pexp_sequence (process_exp e.Ast.value) (build_seq rest)
        in
        build_seq exps
      in
      List.fold_right (fun binding acc ->
        Builder.pexp_let Nonrecursive [binding] acc
      ) bindings body
  | TypedExp (e, ty) ->
      Builder.pexp_constraint (process_exp e.value) (process_type ty.value)
  | RaiseExp e ->
      let e' = process_exp e.value in
      Builder.pexp_apply (Builder.pexp_ident (ghost (Longident.Lident "raise")))
        [(Nolabel, e')]
  | HandleExp (e, cases) ->
      Builder.pexp_try (process_exp e.value) (process_matching cases.value)
  | AndExp (e1, e2) ->
      (* andalso -> && *)
      Builder.pexp_apply (Builder.pexp_ident (ghost (Longident.Lident "&&")))
        [(Nolabel, process_exp e1.value); (Nolabel, process_exp e2.value)]
  | OrExp (e1, e2) ->
      (* orelse -> || *)
      Builder.pexp_apply (Builder.pexp_ident (ghost (Longident.Lident "||")))
        [(Nolabel, process_exp e1.value); (Nolabel, process_exp e2.value)]
  | IfExp (e1, e2, e3) ->
      Builder.pexp_ifthenelse (process_exp e1.value) (process_exp e2.value) (Some (process_exp e3.value))
  | WhileExp (e1, e2) ->
      Builder.pexp_while (process_exp e1.value) (process_exp e2.value)
  | CaseExp (e, cases) ->
      Builder.pexp_match (process_exp e.value) (process_matching cases.value)
  | FnExp cases ->
      (* fn match -> function ... *)
      Builder.pexp_function (process_matching cases.value)


(** {2 Expression Rows and Matching}

    Helper functions for expression-related constructs. *)

(** Convert an SML expression row (record field) to an OCaml record field.

    SML: [{x = 1, y = 2}]
    OCaml: [{x = 1; y = 2}]

    @param row The expression row (field binding)
    @return A pair of field identifier and expression *)
and process_row (row : Ast.row) : (Longident.t Location.loc * Parsetree.expression) = match row with
  | Row (lab, expression, rest_opt) ->
      let lab' = process_with_state (idx_to_string lab.value) in
      (ghost (string_to_longident lab'), process_exp expression.value)

(** Convert SML match clauses to OCaml case list.

    SML: [pat1 => exp1 | pat2 => exp2]
    OCaml: [| pat1 -> exp1 | pat2 -> exp2]

    Used in [case], [fn], and [handle] expressions.

    @param m The match clause(s)
    @return A list of OCaml case expressions *)
and process_matching (m : Ast.matching) : Parsetree.case list = match m with
  | Case (pat, expression, rest_opt) ->
      let case_here = Builder.case ~lhs:(process_pat pat.value) ~guard:None ~rhs:(process_exp expression.value) in
      begin match rest_opt with
      | None -> [case_here]
      | Some rest -> case_here :: process_matching rest.value
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
  | PatCon c -> Builder.ppat_constant (process_con c.value)
  | PatWildcard -> Builder.ppat_any
  | PatIdx wo -> (match wo.value with
      | WithOp op ->
          let op_str = idx_to_string op.value in
          if is_head || (not (is_variable_identifier op_str)) then
            let name = if is_head then
              process_with_state op_str
            else
              process_with_state op_str
            in
            Builder.ppat_construct (ghost (string_to_longident name)) None
          else
            let name = process_with_state op_str in
            Builder.ppat_var (ghost name)
      | WithoutOp id ->
          let id_str = idx_to_string id.value in
          if is_head || (not (is_variable_identifier id_str)) then
            let name = if is_head then
              process_with_state id_str
            else
              process_with_state id_str
            in
            Builder.ppat_construct (ghost (string_to_longident name)) None
          else
            let name = process_with_state id_str in
            Builder.ppat_var (ghost name))
  | PatApp (wo, p) ->
      (* Constructor application: SOME x *)
      let const_name = process_with_op wo.value in
      let arg_pat = process_pat p.value in
      Builder.ppat_construct (ghost (string_to_longident const_name)) (Some arg_pat)
  | PatInfix (p1, id, p2) ->
      (* Infix constructor pattern: x :: xs *)
      let op_name = process_with_state (idx_to_string id.value) in
      let p1' = process_pat p1.value in
      let p2' = process_pat p2.value in
      Builder.ppat_construct (ghost (string_to_longident op_name))
        (Some (Builder.ppat_tuple [p1'; p2']))
  | PatParen p -> process_pat p.value
  | PatTuple [] -> Builder.ppat_construct (ghost (Longident.Lident "()")) None
  | PatTuple ps -> Builder.ppat_tuple (List.map (fun p -> process_pat p.Ast.value) ps)
  | PatRecord rows ->
      let fields = List.flatten (List.map (fun r -> process_pat_row r.Ast.value) rows) in
      Builder.ppat_record (List.map (fun (lab, pat) ->
        (ghost (Longident.Lident lab), pat)
      ) fields) Closed
  | PatList pats ->
      (* Build list pattern from right to left *)
      List.fold_right (fun p acc ->
        Builder.ppat_construct (ghost (Longident.Lident "::"))
          (Some (Builder.ppat_tuple [process_pat p.Ast.value; acc]))
      ) pats (Builder.ppat_construct (ghost (Longident.Lident "[]")) None)
  | PatTyp (p, t) ->
      Builder.ppat_constraint (process_pat p.value) (process_type t.value)
  | PatAs (wo, t_opt, p) ->
      (* Layered pattern: x as SOME y *)
      let var_str = process_with_op wo.value in
      let inner_pat = process_pat p.value in
      let final_pat = match t_opt with
        | None -> inner_pat
        | Some ty -> Builder.ppat_constraint inner_pat (process_type ty.value)
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
      (* No explicit field bindings for wildcard, return empty list *)
      []
  | PatRowSimple (lab, pat, rest) ->
      let lab_str = process_with_state (idx_to_string lab.value) in
      let pat' = process_pat pat.value in
      let here = (lab_str, pat') in
      begin match rest.value with
      | PatRowPoly -> [here]
      | other -> here :: process_pat_row other
      end
  | PatRowVar (id, ty_opt, as_opt, rest_opt) ->
      (* {x, y} is shorthand for {x = x, y = y} *)
      let id_str = process_with_state (idx_to_string id.value) in
      let var_pat = Builder.ppat_var (ghost id_str) in
      let pat_with_type = match ty_opt with
        | None -> var_pat
        | Some ty -> Builder.ppat_constraint var_pat (process_type ty.value)
      in
      let final_pat = match as_opt with
        | None -> pat_with_type
        | Some as_id ->
            let as_name = process_with_state (idx_to_string as_id.value) in
            Builder.ppat_alias pat_with_type (ghost as_name)
      in
      let here = (id_str, final_pat) in
      begin match rest_opt with
      | None -> [here]
      | Some rest -> here :: process_pat_row rest.value
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

    @param declaration The SML declaration to convert
    @return A list of OCaml value bindings *)
and process_value_dec (declaration : Ast.declaration) : Parsetree.value_binding list = match declaration with
  | ValDec (tvars, vb) ->
      (* Type variables in 'val' are currently ignored in conversion *)
      process_val_bind vb.value
  | FunDec fb ->
      process_fun_bind fb.value
  | TypDec tb ->
      (* Type declarations don't produce value bindings *)
      (* They should be handled at the structure level *)
      raise (BadAst "TypeDec is not value decleration")
  | DatDec (db, tb_opt) ->
      (* Datatype declarations don't produce value bindings *)
      (* They should be handled at the structure level *)
        raise (BadAst "DatDec is not value decleration")
  | DataDecAlias (id1, id2) ->
      (* Datatype alias - no value bindings *)
      raise (BadAst "DataDecAlias is not value decleration")
  | AbstractDec (db, tb_opt, decs) ->
      (* Abstract type declarations *)
      (* Process the inner declarations *)
      List.concat (List.map (fun d -> process_value_dec d.Ast.value) decs)
  | ExnDec eb ->
      (* Exception declarations don't produce value bindings *)
        raise (BadAst "ExnDec is not value decleration")
  | StrDec sb ->
      (* Structure declarations don't produce value bindings *)
     raise (BadAst "StrDec is not value decleration")
  | SeqDec decs ->
      (* Sequential declarations - process each and concatenate *)
      List.concat (List.map (fun d -> process_value_dec d.Ast.value) decs)
  | LocalDec (d1, d2) ->
      (* Local declarations - both parts contribute to bindings *)
      process_value_dec d1.value @ process_value_dec d2.value
  | OpenDec ids ->
      (* Open declarations don't produce value bindings *)
     raise (BadAst "OpenDec is not value decleration")
  | FixityDec (fix, ids) ->
      (* Fixity declarations don't produce value bindings *)
     raise (BadAst "FixityDec is not value decleration")

(** Convert SML fixity declarations to string representation.

    SML fixity: [infix 6 +], [infixr 5 ::], [nonfix f]

    @param fix The fixity specification
    @return String representation of fixity *)
and process_fixity (fix : Ast.fixity) : string = match fix with
  | Nonfix -> "nonfix"
  | Infix n -> Printf.sprintf "infix %d" n.value
  | Infixr n -> Printf.sprintf "infixr %d" n.value

(** Convert SML value bindings.

    SML: [val x = 42 and y = 43]
    OCaml: [let x = 42 and y = 43]

    @param vb The value binding(s)
    @return List of OCaml value bindings *)
and process_val_bind (vb : Ast.value_binding) : Parsetree.value_binding list = match vb with
  | ValBind (pat, expression, rest_opt) ->
      let pat' = process_pat pat.value in
      let expression' = process_exp expression.value in
      let binding = Builder.value_binding ~pat:pat' ~expr:expression' in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_val_bind r.value
      in
      binding :: rest
  | ValBindRec vb ->
      (* Recursive value bindings *)
      process_val_bind vb.value

(** Convert SML function bindings to OCaml.

    SML: [fun f 0 = 1 | f n = n * f (n-1)]
    OCaml: [let rec f = function 0 -> 1 | n -> n * f (n-1)]

    @param fb The function binding(s)
    @return List of OCaml value bindings *)
and process_fun_bind (fb : Ast.function_binding) : Parsetree.value_binding list = match fb with
  | FunBind (fm, rest_opt) ->
      (* Get the function name from the first match *)
      let fname_str = match fm.value with
        | FunMatchPrefix (wo, _, _, _, _) -> process_with_op wo.value
        | FunMatchInfix (_, id, _, _, _, _) -> process_with_state (idx_to_string id.value)
        | FunMatchLow (_, id, _, _, _, _, _) -> process_with_state (idx_to_string id.value)
      in

      (* Process all match clauses *)
      let clauses = process_fun_match fm.value in

      (* Build the function body *)
      let body = match clauses with
        | [] -> failwith "Function with no clauses"
        | [(pats, expression)] ->
            (* Single clause - build nested lambdas *)
            List.fold_right (fun pat acc ->
              Builder.pexp_fun Nolabel None pat acc
            ) pats expression
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
              let cases = List.map (fun (pats, expression) ->
                let pat = Builder.ppat_tuple pats in
                Builder.case ~lhs:pat ~guard:None ~rhs:expression
              ) clauses in
              Builder.pexp_match match_exp cases
            )
      in

      let pat = Builder.ppat_var (ghost fname_str) in
      let binding = Builder.value_binding ~pat ~expr:body in

      let rest = match rest_opt with
        | None -> []
        | Some r -> process_fun_bind r.value
      in
      binding :: rest

(** Convert SML function match clauses.

    Helper for {!process_fun_bind}.

    @param fm The function match clause(s)
    @return List of pattern-expression pairs *)
and process_fun_match (fm : Ast.fun_match) : (Parsetree.pattern list * Parsetree.expression) list = match fm with
  | FunMatchPrefix (wo, pats, ty_opt, expression, rest_opt) ->
      (* fun f pat1 pat2 ... = expression *)
      let pats' = List.map (fun p -> process_pat p.Ast.value) pats in
      let expression' = process_exp expression.value in
      let exp_with_type = match ty_opt with
        | None -> expression'
        | Some ty -> Builder.pexp_constraint expression' (process_type ty.value)
      in
      let here = (pats', exp_with_type) in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_fun_match r.value
      in
      here :: rest
  | FunMatchInfix (p1, id, p2, ty_opt, expression, rest_opt) ->
      (* fun p1 op p2 = expression - infix function *)
      let p1' = process_pat p1.value in
      let p2' = process_pat p2.value in
      let expression' = process_exp expression.value in
      let exp_with_type = match ty_opt with
        | None -> expression'
        | Some ty -> Builder.pexp_constraint expression' (process_type ty.value)
      in
      let here = ([p1'; p2'], exp_with_type) in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_fun_match r.value
      in
      here :: rest
  | FunMatchLow (p1, id, p2, pats, ty_opt, expression, rest_opt) ->
      (* fun (p1 op p2) pat3 ... = expression - curried infix *)
      let p1' = process_pat p1.value in
      let p2' = process_pat p2.value in
      let pats' = List.map (fun p -> process_pat p.Ast.value) pats in
      let all_pats = p1' :: p2' :: pats' in
      let expression' = process_exp expression.value in
      let exp_with_type = match ty_opt with
        | None -> expression'
        | Some ty -> Builder.pexp_constraint expression' (process_type ty.value)
      in
      let here = (all_pats, exp_with_type) in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_fun_match r.value
      in
      here :: rest

(** Convert SML type bindings (type abbreviations).

    SML: [type 'a pair = 'a * 'a]
    OCaml: [type 'a pair = 'a * 'a]

    @param tb The type binding(s)
    @return List of OCaml type declarations *)
and process_typ_bind (tb : Ast.type_binding) : Parsetree.type_declaration list = match tb with
  | TypBind (tvars, id, ty, rest_opt) ->
      let name_str = process_with_state (idx_to_string id.value) in
      let params = List.map (fun (tv : Ast.idx Ast.node) ->
        (* Type variables already have the ' prefix, just strip it *)
        let tv_str = idx_to_string tv.value in
        let var_name = if String.starts_with ~prefix:"''" tv_str then
          String.sub tv_str 2 (String.length tv_str - 2)
        else if String.starts_with ~prefix:"'" tv_str then
          String.sub tv_str 1 (String.length tv_str - 1)
        else tv_str
        in
        (Builder.ptyp_var var_name, (Asttypes.NoVariance, Asttypes.NoInjectivity))
      ) tvars in
      let manifest = Some (process_type ty.value) in
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
        | Some r -> process_typ_bind r.value
      in
      tdecl :: rest

(** Convert SML datatype bindings to OCaml variant types.

    SML: [datatype 'a option = NONE | SOME of 'a]
    OCaml: [type 'a option = None | Some of 'a]

    @param db The datatype binding(s)
    @return List of OCaml type declarations *)
and process_dat_bind (db : Ast.data_binding) : Parsetree.type_declaration list = match db with
  | DatBind (tvars, id, cb, rest_opt) ->
      let name_str = process_with_state (idx_to_string id.value) in
      let params = List.map (fun (tv : Ast.idx Ast.node) ->
        (* Type variables already have the ' prefix, just strip it *)
        let tv_str = idx_to_string tv.value in
        let var_name = if String.starts_with ~prefix:"''" tv_str then
          String.sub tv_str 2 (String.length tv_str - 2)
        else if String.starts_with ~prefix:"'" tv_str then
          String.sub tv_str 1 (String.length tv_str - 1)
        else tv_str
        in
        (Builder.ptyp_var var_name, (Asttypes.NoVariance, Asttypes.NoInjectivity))
      ) tvars in
      let constructors = process_con_bind cb.value in
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
        | Some r -> process_dat_bind r.value
      in
      tdecl :: rest

(** Convert SML constructor bindings within a datatype.

    @param cb The constructor binding(s)
    @return List of OCaml constructor declarations *)
and process_con_bind (cb : Ast.constructor_binding) : Parsetree.constructor_declaration list = match cb with
  | ConBind (id, ty_opt, rest_opt) ->
      let name_str = process_with_state (idx_to_string id.value) in
      let args = match ty_opt with
        | None -> Parsetree.Pcstr_tuple []
        | Some ty -> Parsetree.Pcstr_tuple [process_type ty.value]
      in
      let cdecl = Builder.constructor_declaration
        ~name:(ghost name_str)
        ~args
        ~res:None
      in
      let rest = match rest_opt with
        | None -> []
        | Some rest -> process_con_bind rest.value
      in
      cdecl :: rest

(** Convert SML exception bindings.

    SML: [exception Empty] or [exception Fail of string]
    OCaml: [exception Empty] or [exception Fail of string]

    @param eb The exception binding(s)
    @return List of OCaml extension constructors *)
and process_exn_bind (eb : Ast.exn_bind) : Parsetree.extension_constructor list = match eb with
  | ExnBind (id, ty_opt, rest_opt) ->
      let name_str = process_with_state (idx_to_string id.value) in
      let args = match ty_opt with
        | None -> Parsetree.Pcstr_tuple []
        | Some ty -> Parsetree.Pcstr_tuple [process_type ty.value]
      in
      let ext_constr = Builder.extension_constructor
        ~name:(ghost name_str)
        ~kind:(Parsetree.Pext_decl ([], args, None))
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_exn_bind r.value
      in
      ext_constr :: rest
  | ExnBindAlias (id1, id2, rest_opt) ->
      let name1_str = process_with_state (idx_to_string id1.value) in
      let name2 = process_with_state (idx_to_string id2.value) in
      let longid2 = string_to_longident name2 in
      let ext_constr = Builder.extension_constructor
        ~name:(ghost name1_str)
        ~kind:(Parsetree.Pext_rebind (ghost longid2))
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_exn_bind r.value
      in
      ext_constr :: rest

(** Extract identifier from SML [op] prefix wrapper.

    The [op] keyword in SML removes infix status: [op +] is prefix [+].

    @param wo The identifier with or without [op]
    @return The processed identifier *)
and process_with_op (wo : Ast.with_op) : string = match wo with
  | WithOp id -> process_with_state (idx_to_string id.value)
  | WithoutOp id -> process_with_state (idx_to_string id.value)

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

    @param structure The SML structure to convert
    @return List of OCaml structure items *)
and process_str (structure : Ast.structure) : Parsetree.structure_item list = match structure with
  | StrIdx id ->
      (* Structure reference - can't inline, needs module expression context *)
      (* This case should ideally return a module expression instead *)
      raise (BadAst (Printf.sprintf "Structure reference '%s' cannot be converted to structure items - requires module expression context"
        (idx_to_string id.value)))
  | StructStr declaration ->
      (* struct declaration end - convert declarations to structure items *)
      dec_to_structure_items declaration.value
  | AnotateStr (_id, _annot, s) ->
      (* Annotated structure - just process the inner structure *)
      (* The annotation would be handled at binding site *)
      process_str s.value
  | FunctorApp (id, s) ->
      (* Functor application - can't inline *)
      raise (BadAst (Printf.sprintf "Functor application '%s(...)' cannot be converted to structure items - requires module expression context"
        (idx_to_string id.value))) 
  | FunctorAppAnonymous (_id, declaration) ->
      (* Functor applied to anonymous struct *)
      dec_to_structure_items declaration.value
  | LocalDec (declaration, s) ->
      (* Local declarations in structure *)
      dec_to_structure_items declaration.value @ process_str s.value

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
and process_str_bind (sb : Ast.structure_binding) : Parsetree.module_binding list = trace_part ~level:2 ~ast:"structure_binding" ~msg:"" (* ~msg:(Ast.show_str_bind sb) *) ~value:(fun () -> match sb with
  | StrBind (id, annot_opt, rest_opt) ->
      let name_str = process_with_state (idx_to_string id.value) in
      (* Create empty module since AST doesn't include structure body *)
      let module_expr = Builder.pmod_structure [] in
      let module_expr_with_sig = match annot_opt with
        | None -> module_expr
        | Some (annot, signature) ->
            let module_type = process_sign signature.value in
            match annot.value with
            | Transparent -> Builder.pmod_constraint module_expr module_type
            | Opaque -> Builder.pmod_constraint module_expr module_type
      in
      let binding = Builder.module_binding ~name:(ghost (Some name_str)) ~expr:module_expr_with_sig in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_str_bind r.value
      in
      binding :: rest
)
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

    @param signature The SML signature to convert
    @return List of OCaml signature items *)
and process_sign (signature : Ast.signature) : Parsetree.module_type = trace_part ~level:2 ~ast:"signature" ~msg:"" (* ~msg:(Ast.show_sign signature) *) ~value:(fun () -> match signature with
  | SignIdx id ->
      (* Signature identifier - can't inline, needs module type context *)
      (* This should ideally be handled at module type level *)
      raise (BadAst (Printf.sprintf "Signature reference '%s' cannot be converted to signature items - requires module type expression context"
        (idx_to_string id.value)))
     
  | SignSig specification ->
      (* sig specification end - process specifications *)
      let specification' = List.flatten (List.map process_spec specification) in
      Builder.pmty_signature specification'

  | SignWhere (s, _tr) ->
      (* Signature with where clauses *)
      (* Where clauses should be handled at module type level *)
      (* For now, just process the base signature *)
      process_sign s.value
)
(** Convert SML type refinement ([where type]) clauses.

    SML: [sig ... end where type t = int]
    OCaml: Uses [with type] constraints

    @param tr The type refinement
    @return List of type identifier-definition pairs *)
and process_typ_refine (tr : Ast.typ_refine) : (Longident.t * Parsetree.core_type) list = match tr with
  | TypRef (_tvars, id, ty, rest_opt) ->
      (* Type variables are currently ignored in refinement *)
      let name_str = process_with_state (idx_to_string id.value) in
      let longid = string_to_longident name_str in
      let core_type = process_type ty.value in
      let here = (longid, core_type) in
      let rest = match rest_opt with
        | None -> []
        | Some (_ty, tr_rest) -> process_typ_refine tr_rest.value
      in
      here :: rest

(** Convert SML specifications within signatures.

    @param specification The specification to convert
    @return List of OCaml signature items *)
and process_spec (specification' : Ast.specification Ast.node) : Parsetree.signature_item list = trace_part ~level:2 ~ast:"specification" ~msg:"" (* ~msg:(let _ = Ast.show_spec specification'.value in "") *) ~value:(fun () ->
  match specification'.value with
  | SpecVal vd ->
      let vdescs = process_val_specification vd.value in
      List.map (fun vd -> Builder.psig_value vd) vdescs
  | SpecTyp td ->
      let tdecls = process_typ_specification td.value in
      [Builder.psig_type Asttypes.Nonrecursive tdecls]
  | SpecEqtyp td ->
      (* Equality types - in OCaml just abstract types *)
      let tdecls = process_typ_specification td.value in
      [Builder.psig_type Asttypes.Nonrecursive tdecls]
  | SpecTypBind tb ->
      let tdecls = process_typ_bind tb.value in
      [Builder.psig_type Asttypes.Nonrecursive tdecls]
  | SpecDat dd ->
      let tdecls = process_dat_specification dd.value in
      [Builder.psig_type Asttypes.Nonrecursive tdecls]
  | SpecDatAlias (id1, id2) ->
      (* Datatype alias in signature *)
      let name1_str = process_with_state (idx_to_string id1.value) in
      let name2_str = process_with_state (idx_to_string id2.value) in
      let longid2 = string_to_longident name2_str in
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
      let ext_constrs = process_exn_specification ed.value in
      List.map (fun ec ->
        let type_exn = Builder.type_exception ec in
        Builder.psig_exception type_exn
      ) ext_constrs
  | SpecStr sd ->
      let mdecls = process_str_specification sd.value in
      List.map (fun md -> Builder.psig_module md) mdecls
  | SpecSeq (s1, s2) ->
      List.append (process_spec s1) (process_spec s2)
  | SpecInclude s ->
      let module_type = process_sign s.value in
      [Builder.psig_include (Builder.include_infos module_type)]
  | SpecIncludeIdx ids ->
      List.concat (List.map (fun (id : Ast.idx Ast.node) ->
        let name_str = process_with_state (idx_to_string id.value) in
        let longid = string_to_longident name_str in
        let module_type = Builder.pmty_ident (ghost longid) in
        [Builder.psig_include (Builder.include_infos module_type)]
      ) ids)
  | SpecSharingTyp (_s, _ids) ->
      (* Sharing type constraints - complex, skip for now *)
      raise (BadAst "SpecSharingTyp not implemented")
  | SpecSharingStr (_s, _ids) ->
      (* Sharing structure constraints - complex, skip for now *)
      raise (BadAst "SpecSharingStr not implemented"))

(** Convert SML value descriptions in signatures.

    @param vd The value description(s)
    @return List of OCaml value descriptions *)
and process_val_specification (vd : Ast.val_specification) : Parsetree.value_description list = trace_part ~level:2 ~ast:"val_specification" ~msg:"" (* ~msg:(Ast.show_val_specification vd) *) ~value:(fun () ->
  match vd with
  | ValDesc (id, ty, rest_opt) ->
      let name_str = process_with_state (idx_to_string id.value) in
      let core_type = process_type ty.value in
      let vdesc = Builder.value_description
        ~name:(ghost name_str)
        ~type_:core_type
        ~prim:[]
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_val_specification r.value
      in
      vdesc :: rest
)
(** Convert SML abstract type descriptions.

    @param td The type description(s)
    @return List of OCaml type declarations *)
and process_typ_specification (td : Ast.typ_specification) : Parsetree.type_declaration list = trace_part ~level:2 ~ast:"typ_specification" ~msg:"" (* ~msg:(Ast.show_typ_specification td) *) ~value:(fun () -> match td with
  | TypDesc (tvars, id, rest_opt) ->
      let name_str = process_with_state (idx_to_string id.value) in
      let params = List.map (fun (tv : Ast.idx Ast.node) ->
        (* Type variables already have the ' prefix, just strip it *)
        let tv_str = idx_to_string tv.value in
        let var_name = if String.starts_with ~prefix:"''" tv_str then
          String.sub tv_str 2 (String.length tv_str - 2)
        else if String.starts_with ~prefix:"'" tv_str then
          String.sub tv_str 1 (String.length tv_str - 1)
        else tv_str
        in
        (Builder.ptyp_var var_name, (Asttypes.NoVariance, Asttypes.NoInjectivity))
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
        | Some r -> process_typ_specification r.value
      in
      tdecl :: rest
)
(** Convert SML datatype descriptions in signatures.

    @param dd The datatype description(s)
    @return List of OCaml type declarations *)
and process_dat_specification (dd : Ast.dat_specification) : Parsetree.type_declaration list = trace_part ~level:2 ~ast:"dat_specification" ~msg:"" (* ~msg:(Ast.show_dat_specification dd) *) ~value:(fun () -> match dd with
  | DatDesc (tvars, id, cd, rest_opt) ->
      let name_str = process_with_state (idx_to_string id.value) in
      let params = List.map (fun (tv : Ast.idx Ast.node) ->
        (* Type variables already have the ' prefix, just strip it *)
        let tv_str = idx_to_string tv.value in
        let var_name = if String.starts_with ~prefix:"''" tv_str then
          String.sub tv_str 2 (String.length tv_str - 2)
        else if String.starts_with ~prefix:"'" tv_str then
          String.sub tv_str 1 (String.length tv_str - 1)
        else tv_str
        in
        (Builder.ptyp_var var_name, (Asttypes.NoVariance, Asttypes.NoInjectivity))
      ) tvars in
      let constructors = process_con_specification cd.value in
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
        | Some r -> process_dat_specification r.value
      in
      tdecl :: rest
)
(** Convert SML constructor descriptions in signatures.

    @param cd The constructor description(s)
    @return List of OCaml constructor declarations *)
and process_con_specification (cd : Ast.con_specification) : Parsetree.constructor_declaration list = trace_part ~level:2 ~ast:"con_specification" ~msg:"" (* ~msg:(Ast.show_con_specification cd) *) ~value:(fun () -> match cd with
  | ConDesc (id, ty_opt, rest_opt) ->
      (* Same as process_con_bind *)
      let name_str = process_with_state (idx_to_string id.value) in
      let args = match ty_opt with
        | None -> Parsetree.Pcstr_tuple []
        | Some ty -> Parsetree.Pcstr_tuple [process_type ty.value]
      in
      let cdecl = Builder.constructor_declaration
        ~name:(ghost name_str)
        ~args
        ~res:None
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_con_specification r.value
      in
      cdecl :: rest
)
(** Convert SML exception descriptions in signatures.

    @param ed The exception description(s)
    @return List of OCaml extension constructors *)
and process_exn_specification (ed : Ast.exn_specification) : Parsetree.extension_constructor list = trace_part ~level:2 ~ast:"exn_specification" ~msg:"" (* ~msg:(Ast.show_exn_specification ed) *) ~value:(fun () -> match ed with
  | ExnDesc (id, ty_opt, rest_opt) ->
      (* Similar to process_exn_bind but for signatures *)
      let name_str = process_with_state (idx_to_string id.value) in
      let args = match ty_opt with
        | None -> Parsetree.Pcstr_tuple []
        | Some ty -> Parsetree.Pcstr_tuple [process_type ty.value]
      in
      let ext_constr = Builder.extension_constructor
        ~name:(ghost name_str)
        ~kind:(Parsetree.Pext_decl ([], args, None))
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_exn_specification r.value
      in
      ext_constr :: rest
)
(** Convert SML structure descriptions in signatures.

    @param sd The structure description(s)
    @return List of OCaml module declarations *)
and process_str_specification (sd : Ast.str_specification) : Parsetree.module_declaration list = trace_part ~level:2 ~ast:"str_specification" ~msg:"" (* ~msg:(Ast.show_str_specification sd) *) ~value:(fun () -> match sd with
  | StrDesc (id, s, rest_opt) ->
      let name_str = process_with_state (idx_to_string id.value) in
      let module_type = process_sign s.value in
      let mdecl = Builder.module_declaration ~name:(ghost (Some name_str)) ~type_:module_type in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_str_specification r.value
      in
      mdecl :: rest
)
(** {1 Program Processing}

    Functions for converting top-level SML programs.

    A program is a sequence of:
    - Core declarations
    - Functor declarations ([functor F(X: S) = ...])
    - Signature declarations ([signature S = ...]) *)

(** Convert a declaration to structure items.
    Helper function for process_prog. *)
and dec_to_structure_items (declaration : Ast.declaration) : Parsetree.structure_item list = trace_part ~level:5 ~ast:"declaration" ~msg:"" (* ~msg:(Ast.show_dec declaration) *) ~value:(fun () -> match declaration with
  | ValDec (tvars, vb) ->
      let bindings = process_val_bind vb.value in
      List.map (fun binding ->
        Builder.pstr_value Asttypes.Nonrecursive [binding]
      ) bindings
  | FunDec fb ->
      let bindings = process_fun_bind fb.value in
      List.map (fun binding ->
        Builder.pstr_value Asttypes.Recursive [binding]
      ) bindings
  | TypDec tb ->
      let tdecls = process_typ_bind tb.value in
      [Builder.pstr_type Asttypes.Nonrecursive tdecls]
  | DatDec (db, tb_opt) ->
      let tdecls = process_dat_bind db.value in
      let type_item = Builder.pstr_type Asttypes.Nonrecursive tdecls in
      begin match tb_opt with
      | None -> [type_item]
      | Some tb ->
          let tb_decls = process_typ_bind tb.value in
          [type_item; Builder.pstr_type Asttypes.Nonrecursive tb_decls]
      end
  | DataDecAlias (id1, id2) ->
      (* Datatype alias: datatype t = datatype u *)
      (* In OCaml, this would be: type t = u *)
      let name1_str = process_with_state (idx_to_string id1.value) in
      let name2_str = process_with_state (idx_to_string id2.value) in
      let longid2 = string_to_longident name2_str in
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
      List.concat (List.map (fun d -> dec_to_structure_items d.Ast.value) decs)
  | ExnDec eb ->
      let exn_constrs = process_exn_bind eb.value in
      List.map (fun ec ->
        let type_exn = Builder.type_exception ec in
        Builder.pstr_exception type_exn
      ) exn_constrs
  | StrDec sb ->
      let module_bindings = process_str_bind sb.value in
      List.map (fun mb ->
        Builder.pstr_module mb
      ) module_bindings
  | SeqDec decs ->
      List.concat (List.map (fun d -> dec_to_structure_items d.Ast.value) decs)
  | LocalDec (d1, d2) ->
      (* Local declarations - both visible at top level in OCaml *)
      dec_to_structure_items d1.value @ dec_to_structure_items d2.value
  | OpenDec ids ->
      List.map (fun (id : Ast.idx Ast.node) ->
        let name_str = process_with_state (idx_to_string id.value) in
        let longid = string_to_longident name_str in
        let module_expr = Builder.pmod_ident (ghost longid) in
        Builder.pstr_open (Builder.open_infos ~override:Asttypes.Fresh ~expr:module_expr)
      ) ids
  | FixityDec (_fix, _ids) ->
      (* Fixity declarations don't have direct OCaml equivalent *)
      (* Skip them in the output *)
      raise (BadAst "TODO 1")
)
(** Convert a top-level SML program to an OCaml structure.

    @param prog The SML program to convert
    @return An OCaml structure (list of structure items) *)
and process_prog (prog : Ast.prog) : Parsetree.structure = trace_part ~level:5 ~ast:"prog" ~msg:"" ~value:(fun () -> match prog with
  | ProgDec declaration ->
      dec_to_structure_items declaration.value
  | ProgFun fb ->
      let module_bindings = process_functor_binding fb.value in
      List.map (fun mb ->
        Builder.pstr_module mb
      ) module_bindings
  | ProgStr sb ->
      let mtdecls = process_signature_binding sb.value in
      List.map (fun mtd ->
        Builder.pstr_modtype mtd
      ) mtdecls
  | ProgSeq (p1, p2) ->
      process_prog p1.value @ process_prog p2.value
  | ProgEmpty ->
      []
)
(** Convert SML functor bindings (parameterized modules).

    SML: [functor F(X : SIG) = struct ... end]
    OCaml: [module F(X : SIG) = struct ... end]

    @param fb The functor binding(s)
    @return List of OCaml module bindings *)
and process_functor_binding (fb : Ast.functor_binding) : Parsetree.module_binding list = trace_part ~level:2 ~ast:"functor_binding" ~msg:"" (* ~msg:(Ast.show_functor_binding fb) *) ~value:(fun () -> let res = match fb with
  | FctBind (name, param, sig1, annot_opt, body, rest_opt) ->
      let fname_str = process_with_state (idx_to_string name.value) in
      let pname_str = process_with_state (idx_to_string param.value) in

      (* Process parameter signature *)
      let param_module_type = process_sign sig1.value in

      (* Process functor body *)
      let body_items = process_str body.value in
      let body_module_expr = Builder.pmod_structure body_items in

      (* Add result signature constraint if present *)
      let final_body = match annot_opt with
        | None -> body_module_expr
        | Some (_annot, result_sig) ->
            let result_module_type = process_sign result_sig.value in
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
        | Some r -> process_functor_binding r.value
      in
      binding :: rest
  | FctBindOpen (name, specification, annot_opt, body, rest_opt) ->
      (* Opened functor - parameter specification is directly visible *)
      let fname_str = process_with_state (idx_to_string name.value) in

      (* Process parameter specification *)
      let param_module_spec = process_spec specification in
      let param_module_type = Builder.pmty_signature param_module_spec in

      (* Process functor body *)
      let body_items = process_str body.value in
      let body_module_expr = Builder.pmod_structure body_items in

      (* Add result signature constraint if present *)
      let final_body = match annot_opt with
        | None -> body_module_expr
        | Some (_annot, result_sig) ->
            let result_module_type = process_sign result_sig.value in
            Builder.pmod_constraint body_module_expr result_module_type
      in

      (* Create functor with unit parameter (opened specs) *)
      let functor_param = Parsetree.Named (ghost (Some fname_str), param_module_type) in
      let functor_expr = Builder.pmod_functor functor_param final_body in

      let binding = Builder.module_binding
        ~name:(ghost (Some fname_str))
        ~expr:functor_expr
      in

      let rest = match rest_opt with
        | None -> []
        | Some r -> process_functor_binding r.value
      in
      binding :: rest
in res
)
(** Convert SML signature bindings.

    SML: [signature SIG = sig ... end]
    OCaml: [module type SIG = sig ... end]

    @param sb The signature binding(s)
    @return List of OCaml module type declarations *)
and process_signature_binding (sb : Ast.signature_binding) : Parsetree.module_type_declaration list = trace_part ~level:2 ~ast:"signature_binding" ~msg:"" (* ~msg:(Ast.show_signature_binding sb) *) ~value:(fun () -> match sb with
  | SignBind (id, s, rest_opt) ->
      let name_str = process_with_state (idx_to_string id.value) in
      let module_type = process_sign s.value in
      let mtdecl = Builder.module_type_declaration
        ~name:(ghost name_str)
        ~type_:(Some module_type)
      in
      let rest = match rest_opt with
        | None -> []
        | Some r -> process_signature_binding r.value
      in
      mtdecl :: rest
)
(** Main entry point for converting a complete SML program.
    Wraps the converted structure in a toplevel phrase for output. *)
and process_sml ~(prog:Ast.prog): res =
  let structure = process_prog prog in
  [Parsetree.Ptop_def structure]


    end


