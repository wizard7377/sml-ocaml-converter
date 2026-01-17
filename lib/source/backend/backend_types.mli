(** Backend type processing - converts SML types to OCaml types. *)

(** Module type for type processing dependencies *)
module type TYPE_DEPS = sig
  val labeller : Process_label.process_label
  val process_name_to_longident : ctx:Process_names.context -> string list -> Ppxlib.Longident.t
  val process_name_to_string : ctx:Process_names.context -> string list -> string
  val ghost : 'a -> 'a Location.loc
end

(** Module type for type processing interface *)
module type TYPE_PROCESSOR = sig
  val process_type_value : Ast.typ Ast.node -> Parsetree.core_type
  val process_object_field_type : Ast.typ_row Ast.node -> Parsetree.object_field list
  val process_type : Ast.typ Ast.node -> Parsetree.core_type
end

(** Functor to create type processing functions *)
module Make (Deps : TYPE_DEPS) : TYPE_PROCESSOR
