open Common 
open Ast 
module type BACKEND = sig 
    module Config : CONFIG 
    type res 
    val process_sml : prog:Ast.prog -> res

    (** Exported for testing *)
    val process_type_value : Ast.typ -> Parsetree.core_type
    val process_object_field_type : Ast.typ_row -> Parsetree.object_field list
    val process_type : Ast.typ -> Parsetree.core_type
    val process_con : Ast.con -> Parsetree.constant
    val process_exp : Ast.exp -> Parsetree.expression
    val process_pat : ?is_head:bool -> Ast.pat -> Parsetree.pattern
    val process_dec : Ast.dec -> Parsetree.value_binding list
    val process_val_bind : Ast.val_bind -> Parsetree.value_binding list
    val process_fun_bind : Ast.fun_bind -> Parsetree.value_binding list
    val process_typ_bind : Ast.typ_bind -> Parsetree.type_declaration list
    val process_dat_bind : Ast.dat_bind -> Parsetree.type_declaration list
    val process_exn_bind : Ast.exn_bind -> Parsetree.extension_constructor list
    val process_prog : Ast.prog -> Parsetree.structure
end
