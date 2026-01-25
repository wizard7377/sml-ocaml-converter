(** Backend type processing - converts SML types to OCaml types.
    
    SML and OCaml have similar type systems, but with syntactic differences:
    - SML: [int * string -> bool] vs OCaml: [int * string -> bool]
    - SML: ['a list] vs OCaml: ['a list]
    - SML: [{x: int, y: int}] (records) vs OCaml: [< x: int; y: int >] (objects) *)

include Helpers

(** Module type for type processing dependencies *)
module type TYPE_DEPS = sig
  val labeller : Process_label.process_label
  val build_longident : string list -> Ppxlib.Longident.t
  val name_to_string : string list -> string
  val ghost : 'a -> 'a Location.loc
  val config : Common.options
end

(** Module type for type processing interface *)
module type TYPE_PROCESSOR = sig
  val process_type_value : Ast.typ Ast.node -> Parsetree.core_type
  val process_object_field_type : Ast.typ_row Ast.node -> Parsetree.object_field list
  val process_type : Ast.typ Ast.node -> Parsetree.core_type
end

(** Functor to create type processing functions *)
module Make (Deps : TYPE_DEPS) : TYPE_PROCESSOR = struct
  open Deps

  
  (** Convert an SML type to an OCaml core type. *)
  let rec process_type_value (ty : Ast.typ Ast.node) : Parsetree.core_type =
    (labeller#cite Helpers.Attr.core_type ty.pos)
      (match ty.value with
      | TypVar name -> (
          match name.value with
          | Ast.IdxVar v -> Type_var_utils.process_type_var_name v.value
          | _ -> failwith "Expected type variable")
      | TypCon (args, head) ->
          let head_longident =
            build_longident (Idx_utils.idx_to_name head.value)
          in
          let args' = List.map (fun arg -> process_type_value arg) args in
          Builder.ptyp_constr (ghost head_longident) args'
      | TypPar ty' ->
          labeller#cite Helpers.Attr.core_type ty.pos (process_type_value ty')
      | TypFun (ty1, ty2) -> make_arrow ty1 ty2
      | TypTuple tys ->
          Builder.ptyp_tuple (List.map (fun t -> process_type_value t) tys)
      | TypRecord fields ->
          let fields' =
            List.flatten
              (List.map (fun f -> process_object_field_type f) fields)
          in
          Builder.ptyp_object fields' Closed)

  (** Convert SML record type rows to OCaml object fields. *)
  and process_object_field_type (field : Ast.typ_row Ast.node) :
      Parsetree.object_field list =
    List.map (labeller#cite Helpers.Attr.object_field field.pos)
    @@
    match field.value with
    | Ast.TypRow (name, ty, rest) -> (
        let label_name =
          name_to_string (Idx_utils.idx_to_name name.value)
        in
        let here : Parsetree.object_field =
          Builder.otag (ghost label_name) (process_type_value ty)
        in
        match rest with
        | Some rest' -> here :: process_object_field_type rest'
        | None -> [ here ])
  and make_arrow (ty1 : Ast.typ Ast.node) (ty2 : Ast.typ Ast.node) = 
    if not @@ Common.engaged @@ Common.get_curry_types config then 
      Builder.ptyp_arrow Nolabel (process_type_value ty1) (process_type_value ty2)
    else
    begin match ty1.value with
          | TypPar inner_ty -> make_arrow inner_ty ty2
          | TypTuple tys ->  
            make_arrows tys ty2
          | _ -> 
              let ty1', ty2' = (process_type_value ty1, process_type_value ty2) in
              Builder.ptyp_arrow Nolabel ty1' ty2'
          end 
  and make_arrows (ty1s : Ast.typ Ast.node list) (ty2 : Ast.typ Ast.node) =
    match ty1s with
    | [] -> process_type_value ty2
    | ty1 :: rest -> 
        Builder.ptyp_arrow Nolabel (process_type_value ty1) (make_arrows rest ty2)
  (** Wrapper function for process_type_value. *)
  let process_type (ty : Ast.typ Ast.node) : Parsetree.core_type =
    process_type_value ty
end
