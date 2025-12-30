
type res

val process_sml : prog:Ast.prog -> res

(** Exported for testing *)
val process_type_value : Ast.typ -> Parsetree.core_type
val process_object_field_type : Ast.typ_row -> Parsetree.object_field list
val process_type : Ast.typ -> Parsetree.core_type
val process_con : Ast.con -> Parsetree.constant
val process_exp : Ast.exp -> Parsetree.expression
