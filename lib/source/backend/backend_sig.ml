open Common 
open Ast 
module type STORE = sig 
    val store : Names.Store.t
end
module type BACKEND = sig 
    module Config : Common.CONFIG 
    exception BadAst of string
    type res = Parsetree.toplevel_phrase list
    val process_sml : prog:Ast.prog -> res

    (** Exported for testing *)
    val process_type_value : Ast.typ -> Parsetree.core_type
    val process_object_field_type : Ast.typ_row -> Parsetree.object_field list
    val process_type : Ast.typ -> Parsetree.core_type
    val process_con : Ast.constant -> Parsetree.constant
    val process_exp : Ast.expression -> Parsetree.expression
    val process_pat : ?is_head:bool -> Ast.pat -> Parsetree.pattern
    val process_value_dec : Ast.declaration -> Parsetree.value_binding list
    val process_val_bind : Ast.value_binding -> Parsetree.value_binding list
    val process_fun_bind : Ast.function_binding -> Parsetree.value_binding list
    val process_typ_bind : Ast.type_binding -> Parsetree.type_declaration list
    val process_dat_bind : Ast.data_binding -> Parsetree.type_declaration list
    val process_exn_bind : Ast.exn_bind -> Parsetree.extension_constructor list
    val process_prog : Ast.prog -> Parsetree.structure
end
