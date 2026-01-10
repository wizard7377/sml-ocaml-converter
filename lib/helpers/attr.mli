
type atag = string
type attr = Parsetree.attribute
type cite = Parsetree.payload
type 'a citer = 'a -> attr -> 'a


exception CommentNotFound
val expression : Parsetree.expression citer
val pattern : Parsetree.pattern citer
val core_type : Parsetree.core_type citer
val module_type : Parsetree.module_type citer
val module_expr : Parsetree.module_expr citer
val type_declaration : Parsetree.type_declaration citer
val value_binding : Parsetree.value_binding citer
val row_field : Parsetree.row_field citer
val object_field : Parsetree.object_field citer
val value_description : Parsetree.value_description citer
val label_declaration : Parsetree.label_declaration citer
val constructor_declaration : Parsetree.constructor_declaration citer
val type_extension : Parsetree.type_extension citer
val exception_constructor : Parsetree.extension_constructor citer
val exception_declaration : Parsetree.type_exception citer
val class_type : Parsetree.class_type citer
val class_type_field : Parsetree.class_type_field citer
val class_infos : 'a Parsetree.class_infos citer 
val class_expr : Parsetree.class_expr citer
val class_field : Parsetree.class_field citer
val module_declaration : Parsetree.module_declaration citer
val module_type_declaration : Parsetree.module_type_declaration citer
val module_substitution : Parsetree.module_substitution citer
val open_infos : 'a Parsetree.open_infos citer
val include_infos : 'a Parsetree.include_infos citer
val value_binding : Parsetree.value_binding citer
val module_binding : Parsetree.module_binding citer
