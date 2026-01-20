
type space_formatter = (unit, Stdlib.Format.formatter, unit) format
type 'a t = 'a Fmt.t

val longident : Longident.t t
val constr : Longident.t t

val expression : Parsetree.expression t
val string_of_expression : Parsetree.expression -> string

val pattern: Parsetree.pattern t

val core_type: Parsetree.core_type t

val signature: Parsetree.signature t
val structure: Parsetree.structure t
val string_of_structure: Parsetree.structure -> string

val module_expr: Parsetree.module_expr t

val toplevel_phrase : Parsetree.toplevel_phrase t
val top_phrase: Parsetree.toplevel_phrase t

val class_field: Parsetree.class_field t
val class_type_field: Parsetree.class_type_field t
val class_expr: Parsetree.class_expr t
val class_type: Parsetree.class_type t
val module_type: Parsetree.module_type t
val structure_item: Parsetree.structure_item t
val signature_item: Parsetree.signature_item t
val binding: Parsetree.value_binding t
val payload: Parsetree.payload t

val tyvar_of_name : string -> string
  (** Turn a type variable name into a valid identifier, taking care of the
      special treatment required for the single quote character in second
      position, or for keywords by escaping them with \#. No-op on "_". *)

val tyvar: string t
  (** Print a type variable name as a valid identifier, taking care of the
      special treatment required for the single quote character in second
      position, or for keywords by escaping them with \#. No-op on "_". *)

