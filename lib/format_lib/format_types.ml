open Astlib.Ast_414
open Asttypes
open Location
open Parsetree

module type CORE_TYPE = sig
  val core_type : core_type Fmt.t
  val core_type_parens : core_type Fmt.t
  val type_with_label : Stdlib.Format.formatter -> (arg_label * core_type) -> unit
end

module type PATTERN = sig
  val pattern : pattern Fmt.t
  val pattern_parens : pattern Fmt.t
end

module type EXPR = sig
  val expression : expression Fmt.t
  val expression_parens : expression Fmt.t
  val label_exp : (arg_label * expression option * pattern) Fmt.t
  val label_x_expression : (arg_label * expression) Fmt.t
  val bindings : (rec_flag * value_binding list) Fmt.t
  val binding_body : value_binding Fmt.t
end

module type ATTRS = sig
  val attributes : attributes Fmt.t
  val item_attributes : attributes Fmt.t
  val floating_attribute : attribute Fmt.t
  val extension : (string loc * payload) Fmt.t
  val item_extension : (string loc * payload) Fmt.t
  val payload : payload Fmt.t
  val value_description : value_description Fmt.t
  val exception_declaration : type_exception Fmt.t
end

module type CLASS_TYPE = sig
  val class_type : class_type Fmt.t
  val class_signature : class_signature Fmt.t
  val class_type_field : class_type_field Fmt.t
  val class_type_declaration_list : class_type_declaration list Fmt.t
  val class_params_def : (core_type * (variance * injectivity)) list Fmt.t
  val type_param : (core_type * (variance * injectivity)) Fmt.t
end

module type CLASS_EXPR = sig
  val class_expr : class_expr Fmt.t
  val class_field : class_field Fmt.t
  val class_structure : class_structure Fmt.t
end

module type MODULE_TYPE = sig
  val module_type : module_type Fmt.t
  val signature : signature Fmt.t
  val signature_item : signature_item Fmt.t
end

module type MODULE_EXPR = sig
  val module_expr : module_expr Fmt.t
  val structure : structure Fmt.t
  val structure_item : structure_item Fmt.t
end

module type TYPE_DECL = sig
  val type_def_list : (rec_flag * bool * type_declaration list) Fmt.t
  val type_declaration : type_declaration Fmt.t
  val type_extension : type_extension Fmt.t
  val extension_constructor : extension_constructor Fmt.t
  val record_declaration : label_declaration list Fmt.t
end
