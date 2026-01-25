(** OCaml AST post-processing for SML to OCaml conversion.

    This module performs name transformations on the OCaml Parsetree after
    the backend has done literal translation. It handles:
    - OCaml keyword escaping with cascading renames
    - Capitalization conventions (constructors, types, values)
    - SML basis library constructor mapping
    - Context-aware name processing

    The transformation is implemented as a tree traversal that infers context
    from the structure of the Parsetree. *)

open Ppxlib
open Common
(** Helper functions for capitalization *)
module Capital = struct
  let process_lowercase (s : string) : string = String.uncapitalize_ascii s
  let process_uppercase (s : string) : string = String.capitalize_ascii s
  let process_caps (s : string) : string = String.uppercase_ascii s

  let is_variable_identifier (s : string) : bool =
    if String.length s = 0 then false
    else
      let first_char = String.get s 0 in
      (first_char >= 'a' && first_char <= 'z') || first_char = '_'

  let is_operator_name (s : string) : bool =
    if String.length s = 0 then false
    else
      let c = String.get s 0 in
      not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_')
end

(** Context for name processing - tracks where we are in the tree *)
type name_context =
  | EmptyContext    (** No specific context *)
  | InPattern        (** Inside a pattern (may be constructor or variable) *)
  | InPatternHead    (** Head position of pattern application (always constructor) *)
  | InExpression     (** Inside an expression *)
  | InTypeDecl       (** Inside a type declaration name *)
  | InTypeName       (** Inside a type use (constructor reference) *)
  | InConstructorDecl (** Inside a constructor declaration *)
  | InModuleName of name_context    (** Inside a module identifier *)
  | InQualifiedName of name_context  (** Inside a qualified name *)
  | InLabel          (** Inside a record label *)
  | InValue          (** Inside a value identifier *)

let context_to_string (ctx : name_context) : string =
  match ctx with
  | EmptyContext -> "EmptyContext"
  | InPattern -> "InPattern"
  | InPatternHead -> "InPatternHead"
  | InExpression -> "InExpression"
  | InTypeDecl -> "InTypeDecl"
  | InTypeName -> "InTypeName"
  | InConstructorDecl -> "InConstructorDecl"
  | InModuleName _ -> "InModuleName"
  | InQualifiedName _ -> "InQualifiedName"
  | InLabel -> "InLabel"
  | InValue -> "InValue"

(** Overall context for processing, including configuration options *)
type context = {
  name_ctx : name_context;
  config : Common.options;
}

let default_context : context = {
  name_ctx = EmptyContext;
  config = Common.mkOptions ();
}

(** Track identifiers that need cascading renames *)
module StringSet = Set.Make(String)
module Builder = Ast_builder.Make(struct
  let loc = Location.none
end)
module Log = Common.Make (struct
  let config = Common.mkOptions ()
  let group = "process_names"
end)
class process_ocaml ~(opts : Common.options) =
  object (self)
    inherit [context] Ast_traverse.map_with_context as super

    val config = opts
    val mutable constructor_names : StringSet.t = StringSet.empty
    val mutable processing_depth : int = 0
    val max_depth : int = 1024

    (** {1 Currying Transformations}

        SML uses uncurried functions: fn (x, y) => body
        OCaml idiomatically uses curried functions: fun x y -> body

        When curry_expressions is enabled, we transform:
        - Function expressions: fun (x, y) -> body  →  fun x -> fun y -> body
        - Function applications: f (a, b)  →  f a b
        - Value bindings are handled via expression transformation *)

    (** Check if currying is enabled *)
    method private should_curry : bool =
      Common.engaged (Common.get_curry_expressions config)

    (** Extract patterns from a tuple pattern for currying.
        Handles nested parentheses and type constraints. *)
    method private extract_tuple_patterns (pat : Parsetree.pattern) : Parsetree.pattern list option =
      match pat.ppat_desc with
      | Ppat_tuple pats when List.length pats > 0 -> Some pats
      | Ppat_constraint (inner, _) ->
          (* Type constraint on tuple - don't curry, keep as-is *)
          None
      | _ -> None

    (** Build a curried function from multiple patterns and a body.
        fun p1 -> fun p2 -> ... -> body
        Note: patterns will be processed by the normal traversal. *)
    method private build_curried_function (pats : Parsetree.pattern list) (body : Parsetree.expression) : Parsetree.expression =
      List.fold_right
        (fun pat acc ->
          { pexp_desc = Pexp_fun (Nolabel, None, pat, acc);
            pexp_loc = Location.none;
            pexp_loc_stack = [];
            pexp_attributes = [] })
        pats
        body

    (** Detect if an expression is a tuple that should be uncurried in application *)
    method private is_uncurryable_tuple (expr : Parsetree.expression) : Parsetree.expression list option =
      match expr.pexp_desc with
      | Pexp_tuple args when List.length args > 0 -> Some args
      | Pexp_constraint (inner, _) ->
          (* Look through type constraints *)
          (match inner.pexp_desc with
           | Pexp_tuple args when List.length args > 0 -> Some args
           | _ -> None)
      | _ -> None

    (** Handle operator prefixes like "op+" -> "+" *)
    method private process_op (input : string) : string =
      let is_op = String.starts_with ~prefix:"op" input && (if String.length input > 2 then
          let c = String.get input 2 in
          not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_')
        else false) in
      Log.log_with ~cfg:config ~level:Debug ~kind:Neutral
        ~msg:(Printf.sprintf "Processing operator prefix for: %s (is_op=%b)" input is_op) ();
      let res0 = if is_op then String.sub input 2 (String.length input - 2) else input in
      let res1 = match res0 with
        | "~" -> "~-"
        | _ -> res0 in
      res1

    (** Check if a name needs keyword escaping *)
    method private needs_keyword_escape (name : string) : bool =
      Keyword.is_keyword name && Common.is_flag_enabled (Common.get_convert_keywords config)

    (** Apply cascading rename: if name_ exists, become name__, otherwise become name_ *)
    method private escape_keyword (name : string) : string =
      if self#needs_keyword_escape name then
        name ^ "_"
      else
        name

    (** Process a simple identifier based on context *)
    method private process_identifier (ctx : name_context) (name : string) : string =
      processing_depth <- processing_depth + 1;
      if processing_depth > max_depth then begin
        processing_depth <- processing_depth - 1;
        let ctx_str = match ctx with
          | EmptyContext -> "EmptyContext"
          | InPattern -> "InPattern"
          | InPatternHead -> "InPatternHead"
          | InExpression -> "InExpression"
          | InTypeDecl -> "InTypeDecl"
          | InTypeName -> "InTypeName"
          | InConstructorDecl -> "InConstructorDecl"
          | InModuleName _ -> "InModuleName"
          | InQualifiedName _ -> "InQualifiedName"
          | InLabel -> "InLabel"
          | InValue -> "InValue"
        in
        failwith (Printf.sprintf
          "Maximum recursion depth (%d) exceeded while processing identifier '%s' in context %s. \
           This likely indicates circular references in the AST generated by the backend."
          max_depth name ctx_str)
      end;
      Log.log_with ~cfg:config ~level:Debug ~kind:Neutral
        ~msg:(Printf.sprintf "Processing identifier (depth=%d): %s in context %s" processing_depth name (context_to_string ctx)) ();
      let name_without_op = self#process_op name in
      let result = begin

      match ctx with
      | InTypeDecl when Common.is_flag_enabled (Common.get_rename_types config) ->
          (* Type declarations must be lowercase *)
          let lowered = Capital.process_lowercase name_without_op in
          if Capital.is_variable_identifier lowered then lowered
          else lowered ^ "_"

      | InConstructorDecl when Common.is_flag_enabled (Common.get_convert_names config) ->
          Log.log_with ~cfg:config ~level:Debug ~kind:Neutral
            ~msg:("Processing constructor declaration name: " ^ name_without_op) ();
          (* Map SML basis constructors to OCaml equivalents *)
          let mapped = match name_without_op with
            | "SOME" -> "Some"
            | "NONE" -> "None"
            | "true" -> "true"
            | "false" -> "false"
            | "nil" -> "[]"
            | "LESS" -> "Less"
            | "EQUAL" -> "Equal"
            | "GREATER" -> "Greater"
            | _ -> name_without_op
          in
          Log.log_with ~cfg:config ~level:Debug ~kind:Neutral
            ~msg:(Printf.sprintf "Mapped constructor name: %s -> %s" name_without_op mapped) ();
          (* Constructors must be uppercase *)
          Capital.process_uppercase mapped

      | InPatternHead when Common.is_flag_enabled (Common.get_guess_pattern config) ->
          (* Pattern heads are always constructors *)
          Capital.process_uppercase name_without_op

      | InPattern ->
          (* In patterns, guess based on capitalization if --guess-var is set *)
          (match Common.get_guess_var config with
           | Some pattern ->
               let regex = Re.Str.regexp ({|\b|} ^ pattern ^ {|\b|}) in
               if Re.Str.string_match regex name_without_op 0 then
                 (* Matches guess pattern - treat as variable *)
                 if Common.is_flag_enabled (Common.get_convert_names config) then
                   Capital.process_lowercase name_without_op
                 else
                   name_without_op
               else
                 (* Doesn't match - treat as constructor *)
                 Capital.process_uppercase name_without_op
           | None ->
               (* No guess pattern - use SML capitalization as-is *)
               name_without_op)

      | InValue | InLabel when Common.is_flag_enabled (Common.get_convert_names config) ->
          (* Values and labels should be lowercase *)
          Capital.process_lowercase name_without_op

      | InModuleName inner_ctx ->
          (* Modules should be uppercase *)
          Capital.process_uppercase name_without_op
      | InQualifiedName inner_ctx ->
          name_without_op

      | _ ->
          Log.log_with ~cfg:config ~level:Debug ~kind:Neutral
            ~msg:("No specific processing for identifier: " ^ name_without_op) ();
          name_without_op
      end in
      
      Log.log_with ~cfg:config ~level:Debug ~kind:Neutral
        ~msg:(Printf.sprintf "Processed identifier: %s -> %s" name result) ();
      processing_depth <- processing_depth - 1;
      result

    (** Process a Longident recursively *)
    method private process_longident (ctx : name_context) (lid : Longident.t) : Longident.t =
      Log.log_with ~cfg:config ~level:Debug ~kind:Neutral
        ~msg:("Processing long identifier in context " ^ (context_to_string ctx)) ();
      match lid with
      | Lident name ->
          Log.log_with ~cfg:config ~level:Debug ~kind:Neutral
            ~msg:("Processing simple identifier: " ^ name) ();
          let processed = self#process_identifier ctx name in
          let escaped = self#escape_keyword processed in
          Lident escaped

      | Ldot (prefix, name) ->
          let processed_name = self#process_identifier (InQualifiedName ctx) name in
          let escaped_name = self#escape_keyword processed_name in
          (* Module prefixes stay uppercase - avoid infinite nesting of InModuleName *)
          let module_ctx = match ctx with
            | InModuleName inner -> inner
            | InQualifiedName inner -> inner
            | _ -> ctx
          in
          let processed_prefix = self#process_longident (InModuleName ctx) prefix in
          Ldot (processed_prefix, escaped_name)

      | Lapply (f, arg) ->
          (* Avoid infinite nesting of InModuleName *)
          let module_ctx = match ctx with
            | InModuleName inner -> inner
            | InQualifiedName inner -> inner
            | _ -> ctx
          in
          Lapply (
            self#process_longident (InModuleName ctx) f,
            self#process_longident (InModuleName ctx) arg
          )

    (** Process a located Longident *)
    method private process_loc_longident (ctx : name_context) (loc_lid : Longident.t Location.loc) : Longident.t Location.loc =
      { loc_lid with txt = self#process_longident ctx loc_lid.txt }

    (** Override pattern traversal to infer context *)
    method! pattern ctx pat =
      Log.log_with ~cfg:config ~level:Debug ~kind:Neutral
        ~msg:("Processing pattern") ();
      match pat.ppat_desc with
      | Ppat_var name ->
          (* Variable pattern - process as value *)
          let new_name = self#process_identifier InValue name.txt in
          let escaped = self#escape_keyword new_name in
          { pat with ppat_desc = Ppat_var { name with txt = escaped } }

      | Ppat_alias (inner, name) ->
          (* Alias pattern *)
          let new_name = self#process_identifier InValue name.txt in
          let escaped = self#escape_keyword new_name in
          let new_inner = self#pattern ctx inner in
          { pat with ppat_desc = Ppat_alias (new_inner, { name with txt = escaped }) }

      | Ppat_construct (lid, arg_opt) ->
          (* Constructor pattern - head is always constructor *)
          let new_lid = self#process_loc_longident InConstructorDecl lid in
          let new_arg =
            match arg_opt with
            | None -> None
            | Some (labels, p) ->
                Some (labels, self#pattern ctx p)
          in
          { pat with ppat_desc = Ppat_construct (new_lid, new_arg) }

      | Ppat_or (p1, p2) ->
          let new_p1 = self#pattern ctx p1 in
          let new_p2 = self#pattern ctx p2 in
          { pat with ppat_desc = Ppat_or (new_p1, new_p2) }

      | _ ->
          super#pattern ctx pat

    (** Override expression traversal *)
    method! expression ctx expr =
      (* First check if this expression needs currying transformation *)
      let should_transform_curry =
        if self#should_curry then
          match expr.pexp_desc with
          | Pexp_fun (_, _, pat, _) ->
              (match self#extract_tuple_patterns pat with
               | Some _ -> true
               | None -> false)
          | Pexp_apply (_, [(Nolabel, arg)]) ->
              (match self#is_uncurryable_tuple arg with
               | Some _ -> true
               | None -> false)
          | Pexp_function cases ->
              (match cases with
               | [{ pc_lhs; pc_guard = None; pc_rhs = _ }] ->
                   (match self#extract_tuple_patterns pc_lhs with
                    | Some _ -> true
                    | None -> false)
               | _ -> false)
          | _ -> false
        else
          false
      in

      (* Apply transformations if needed *)
      let expr_with_curry =
        if should_transform_curry then
          match expr.pexp_desc with
          | Pexp_fun (_, _, pat, body) ->
              (match self#extract_tuple_patterns pat with
               | Some pats -> self#build_curried_function pats body
               | None -> expr)
          | Pexp_apply (f, [(Nolabel, arg)]) ->
              (match self#is_uncurryable_tuple arg with
               | Some tuple_args ->
                   { expr with pexp_desc = Pexp_apply (f, List.map (fun a -> (Nolabel, a)) tuple_args) }
               | None -> expr)
          | Pexp_function cases ->
              (match cases with
               | [{ pc_lhs; pc_guard = None; pc_rhs }] ->
                   (match self#extract_tuple_patterns pc_lhs with
                    | Some pats -> self#build_curried_function pats pc_rhs
                    | None -> expr)
               | _ -> expr)
          | _ -> expr
        else
          expr
      in

      (* If we transformed, re-process the new structure; otherwise use super *)
      let expr_after_curry =
        if should_transform_curry then
          self#expression ctx expr_with_curry
        else
          super#expression ctx expr
      in

      (* Then apply name processing transformations to the result *)
      match expr_after_curry.pexp_desc with
      | Pexp_ident lid ->
          (* Could be constructor or value - infer from capitalization *)
          let is_constructor = match lid.txt with
            | Lident name -> not (Capital.is_variable_identifier name)
            | Ldot (_, name) -> not (Capital.is_variable_identifier name)
            | Lapply _ -> false
          in
          let name_ctx = if is_constructor then InConstructorDecl else InValue in
          let new_lid = self#process_loc_longident name_ctx lid in
          { expr_after_curry with pexp_desc = Pexp_ident new_lid }

      | Pexp_construct (lid, arg_opt) ->
          (* Constructor expression *)
          let new_lid = self#process_loc_longident InConstructorDecl lid in
          let new_arg = Option.map (super#expression ctx) arg_opt in
          { expr_after_curry with pexp_desc = Pexp_construct (new_lid, new_arg) }

      | Pexp_field (e, lid) ->
          (* Record field access *)
          let new_e = super#expression ctx e in
          let new_lid = self#process_loc_longident InLabel lid in
          { expr_after_curry with pexp_desc = Pexp_field (new_e, new_lid) }

      | _ ->
          expr_after_curry

    (** Override type declaration *)
    method! type_declaration ctx td =
      let new_name = self#process_identifier InTypeDecl td.ptype_name.txt in
      let escaped = self#escape_keyword new_name in
      let new_td = super#type_declaration ctx td in
      { new_td with ptype_name = { td.ptype_name with txt = escaped } }

    (** Override constructor declaration *)
    method! constructor_declaration ctx cd =
      Log.log_with ~cfg:config ~level:Debug ~kind:Neutral
        ~msg:("Processing constructor declaration: " ^ cd.pcd_name.txt) ();
      let new_name = self#process_identifier InConstructorDecl cd.pcd_name.txt in
      let escaped = self#escape_keyword new_name in
      let new_cd = super#constructor_declaration ctx cd in
      { new_cd with pcd_name = { cd.pcd_name with txt = escaped } }

    (** Override label declaration (record fields) *)
    method! label_declaration ctx ld =
      let new_name = self#process_identifier InLabel ld.pld_name.txt in
      let escaped = self#escape_keyword new_name in
      let new_ld = super#label_declaration ctx ld in
      { new_ld with pld_name = { ld.pld_name with txt = escaped } }

    (** Override value binding *)
    method! value_binding ctx vb =
      (* Process the pattern which contains the binding name *)
      let new_vb = super#value_binding ctx vb in
      new_vb

    (** Override module binding *)
    method! module_binding ctx mb =
      let new_name = match mb.pmb_name.txt with
        | Some name ->
            let processed = self#process_identifier (InModuleName ctx.name_ctx) name in
            let escaped = self#escape_keyword processed in
            Some escaped
        | None -> None
      in
      let new_mb = super#module_binding ctx mb in
      { new_mb with pmb_name = { mb.pmb_name with txt = new_name } }

    (** Override module declaration *)
    method! module_declaration ctx md =
      let new_name = match md.pmd_name.txt with
        | Some name ->
            let processed = self#process_identifier (InModuleName ctx.name_ctx) name in
            let escaped = self#escape_keyword processed in
            Some escaped
        | None -> None
      in
      let new_md = super#module_declaration ctx md in
      { new_md with pmd_name = { md.pmd_name with txt = new_name } }

    (** Override core type to handle type references *)
    method! core_type ctx ct =
      match ct.ptyp_desc with
      | Ptyp_constr (lid, args) ->
          (* Type constructor reference *)
          let new_lid = self#process_loc_longident InTypeName lid in
          let new_args = List.map (self#core_type ctx) args in
          { ct with ptyp_desc = Ptyp_constr (new_lid, new_args) }

      | _ ->
          super#core_type ctx ct

    (** Main entry point - process a list of toplevel phrases *)
    method run_process (phrases : Parsetree.toplevel_phrase list) : Parsetree.toplevel_phrase list =
      let initial_ctx = {
        name_ctx = InExpression;
        config = config;
      } in
      List.map (self#toplevel_phrase initial_ctx) phrases
  end
