open! Ppxlib.Parsetree
open Common
include Ast

module type CONTEXT = sig
  val lexbuf : string
  val context : Context.t
end

module type BACKEND = sig
  module Config : Common.CONFIG

  exception BadAst of (Lexing.position * Lexing.position) option * string

  type res = Parsetree.toplevel_phrase list

  val process_sml : prog:Ast.prog -> res

  val process_type_value : Ast.typ Ast.node -> Parsetree.core_type
  (** Exported for testing *)

  val process_object_field_type :
    Ast.typ_row Ast.node -> Parsetree.object_field list

  val process_type : Ast.typ Ast.node -> Parsetree.core_type
  val process_con : Ast.constant Ast.node -> Parsetree.constant
  val process_exp : Ast.expression Ast.node -> Parsetree.expression

  val process_pat :
    ?is_arg:bool -> ?is_head:bool -> Ast.pat Ast.node -> Parsetree.pattern

  val process_value_dec : Ast.declaration -> Parsetree.value_binding list
  val process_val_bind : Ast.value_binding -> Parsetree.value_binding list
  val process_fun_bind : Ast.function_binding -> Parsetree.value_binding list
  val process_typ_bind : Ast.type_binding -> Parsetree.type_declaration list
  val process_dat_bind : Ast.data_binding -> Parsetree.type_declaration list
  val process_exn_bind : Ast.exn_bind -> Parsetree.extension_constructor list
  val process_prog : Ast.prog -> Parsetree.structure

  val get_all_constructors :
    unit -> Context.Constructor_registry.constructor_info list
end
