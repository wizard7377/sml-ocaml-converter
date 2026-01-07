
(** Traverse the AST to collect constructor information from con_specification nodes *)

open Ast

let empty_context : Context.t = Context.create []
(** Extract the string name from an idx node *)
let rec idx_to_string (idx : idx) : string =
  match idx with
  | IdxIdx s -> unbox_node s
  | IdxVar s -> unbox_node s
  | IdxLab s -> unbox_node s
  | IdxNum s -> unbox_node s
  | IdxLong ids ->
    ids
    |> List.map (fun n -> idx_to_string (unbox_node n))
    |> String.concat "."

(** Process a con_specification node and collect constructor info.
    Parameters:
    - path: current module path context
    - type_name: the name of the datatype this constructor belongs to
    - con_spec: the constructor specification node
*)
let rec process_con_specification ~(path : string list) ~(type_name : string) (con_spec : con_specification node) : Context.t =
  match unbox_node con_spec with
  | ConDesc (name_node, typ_opt, rest_opt) -> (
    let name = idx_to_string (unbox_node name_node) in
    let arity = match typ_opt with
      | Some ty -> (match unbox_node ty with
        | TypTuple tys -> Some (List.length tys)
        | _ -> Some 1)
      | Some _ -> Some 1
      | None -> Some 0
in 
    let info : Info.name_info = Info.ConstructorInfo {
      arity = arity ;
    } in
    let rest = match rest_opt with
      | Some rest_node -> process_con_specification ~path ~type_name rest_node
      | None -> Context.create []
    in
    let full_name : Info.name = {
      path = path ;
      root = name ;
    } in
    let context1 = Context.create [full_name, info] in 
    let context2 = match rest_opt with
      | Some rest_node -> process_con_specification ~path ~type_name rest_node
      | None -> Context.create []
  in 
    Context.merge context1 context2
  )

(** Process a dat_specification node *)
let rec process_dat_specification ~(path : string list) (dat_spec : dat_specification node) : Context.t =
  match unbox_node dat_spec with
  | DatDesc (_type_vars, type_name_node, con_spec, rest_opt) ->
    let type_name = idx_to_string (unbox_node type_name_node) in
    let cons = process_con_specification ~path ~type_name con_spec in
    let rest = match rest_opt with
      | Some rest_node -> process_dat_specification ~path rest_node
      | None -> Context.create []
    in
    Info.merge cons rest

(** Process a specification node *)
let rec process_specification ~(path : string list) (spec : specification node) : Context.t =
  match unbox_node spec with
  | SpecVal _ -> Context.create []
  | SpecTyp _ -> Context.create []
  | SpecEqtyp _ -> Context.create []
  | SpecTypBind _ -> Context.create []
  | SpecDat dat_spec -> process_dat_specification ~path dat_spec
  | SpecDatAlias (_, _) -> Context.create []
  | SpecExn _ -> Context.create []
  | SpecStr str_spec -> process_str_specification ~path str_spec
  | SpecSeq (s1, s2) ->
    Context.merge (process_specification ~path s1) (process_specification ~path s2)
  | SpecInclude sig_node -> process_signature ~path sig_node
  | SpecIncludeIdx _ -> Context.create []
  | SpecSharingTyp (spec_node, _) -> process_specification ~path spec_node
  | SpecSharingStr (spec_node, _) -> process_specification ~path spec_node

(** Process a str_specification node *)
and process_str_specification ~(path : string list) (str_spec : str_specification node) : Context.t =
  match unbox_node str_spec with
  | StrDesc (name_node, sig_node, rest_opt) ->
    let name = idx_to_string (unbox_node name_node) in
    let new_path = path @ [name] in
    let sigs = process_signature ~path:new_path sig_node in
    let rest = match rest_opt with
      | Some rest_node -> process_str_specification ~path rest_node
      | None -> Context.create []
    in
    Info.merge sigs rest

(** Process a signature node *)
and process_signature ~(path : string list) (sig_node : signature node) : Context.t =
  match unbox_node sig_node with
  | SignIdx _ -> Context.create []
  | SignSig specs ->
    specs |> List.fold_left (fun acc spec -> Info.merge acc (process_specification ~path spec)) (Context.create [])
  | SignWhere (sig_inner, _) -> process_signature ~path sig_inner

(** Process a constructor_binding node (for datatypes in declarations) *)
let rec process_constructor_binding ~(path : string list) ~(type_name : string) (con_bind : constructor_binding node) : Context.t =
  match unbox_node con_bind with
  | ConBind (name_node, typ_opt, rest_opt) ->
    let name = idx_to_string (unbox_node name_node) in
    let arity = match typ_opt with
      | Some _ -> Some 1
      | None -> Some 0
    in
    let info : Info.name_info = Info.ConstructorInfo {
      arity;
    } in
    let full_name : Info.name = {
      path = path ;
      root = name ;
    } in
    let info = Context.create [full_name, info] in
    let rest = match rest_opt with
      | Some rest_node -> process_constructor_binding ~path ~type_name rest_node
      | None -> Context.create []
    in
    Info.merge info rest

(** Process a data_binding node *)
let rec process_data_binding ~(path : string list) (dat_bind : data_binding node) : Context.t =
  match unbox_node dat_bind with
  | DatBind (_type_vars, type_name_node, con_bind, rest_opt) ->
    let type_name = idx_to_string (unbox_node type_name_node) in
    let cons = process_constructor_binding ~path ~type_name con_bind in
    let rest = match rest_opt with
      | Some rest_node -> process_data_binding ~path rest_node
      | None -> Context.create []
    in
    Info.merge cons rest

(** Process a structure node *)
let rec process_structure ~(path : string list) (str : structure node) : Context.t =
  match unbox_node str with
  | StrIdx _ -> Context.create []
  | StructStr dec -> process_declaration ~path dec
  | AnotateStr (_, _, str_inner) -> process_structure ~path str_inner
  | FunctorApp (_, str_inner) -> process_structure ~path str_inner
  | FunctorAppAnonymous (_, dec) -> process_declaration ~path dec
  | LocalDec (dec, str_inner) ->
    Info.merge (process_declaration ~path dec) (process_structure ~path str_inner)

(** Process a structure_binding node *)
and process_structure_binding ~(path : string list) (str_bind : structure_binding node) : Context.t =
  match unbox_node str_bind with
  | StrBind (name_node, sig_opt, rest_opt) ->
    let name = idx_to_string (unbox_node name_node) in
    let new_path = path @ [name] in
    let sigs = match sig_opt with
      | Some (_, sig_node) -> process_signature ~path:new_path sig_node
      | None -> Context.create []
    in
    let rest = match rest_opt with
      | Some rest_node -> process_structure_binding ~path rest_node
      | None -> Context.create []
    in
    Info.merge sigs rest

(** Process a declaration node *)
and process_declaration ~(path : string list) (dec : declaration node) : Context.t =
  match unbox_node dec with
  | ValDec (_, _) -> Context.create []
  | FunDec _ -> Context.create []
  | TypDec _ -> Context.create []
  | DatDec (dat_bind, _) -> process_data_binding ~path dat_bind
  | DataDecAlias (_, _) -> Context.create []
  | AbstractDec (dat_bind, _, decs) ->
    Info.merge (process_data_binding ~path dat_bind) (decs |> List.fold_left (fun acc dec -> Info.merge acc (process_declaration ~path dec)) (Context.create []))
  | ExnDec _ -> Context.create []
  | StrDec str_bind -> process_structure_binding ~path str_bind
  | SeqDec decs -> decs |> List.fold_left (fun acc dec -> Info.merge acc (process_declaration ~path dec)) (Context.create [])
  | LocalDec (dec1, dec2) ->
    Info.merge (process_declaration ~path dec1) (process_declaration ~path dec2)  
  | OpenDec _ -> Context.create []
  | FixityDec (_, _) -> Context.create []
(** Process a functor_binding node *)
let rec process_functor_binding ~(path : string list) (fct_bind : functor_binding node) : Context.t =
  match unbox_node fct_bind with
  | FctBind (name_node, _, param_sig, _result_sig_opt, body, rest_opt) ->
    let name = idx_to_string (unbox_node name_node) in
    let new_path = path @ [name] in
    let param_sigs = process_signature ~path:new_path param_sig in
    let body_result = process_structure ~path:new_path body in
    let rest = match rest_opt with
      | Some rest_node -> process_functor_binding ~path rest_node
      | None -> Context.create []
    in
    Info.merge (Info.merge param_sigs body_result) rest
  | FctBindOpen (name_node, spec, _result_sig_opt, body, rest_opt) ->
    let name = idx_to_string (unbox_node name_node) in
    let new_path = path @ [name] in
    let specs = process_specification ~path:new_path spec in
    let body_result = process_structure ~path:new_path body in
    let rest = match rest_opt with
      | Some rest_node -> process_functor_binding ~path rest_node
      | None -> Context.create []
    in
    Info.merge (Info.merge specs body_result) rest

(** Process a signature_binding node *)
let rec process_signature_binding ~(path : string list) (sig_bind : signature_binding node) : Context.t =
  match unbox_node sig_bind with
  | SignBind (name_node, sig_node, rest_opt) ->
    let name = idx_to_string (unbox_node name_node) in
    let new_path = path @ [name] in
    let sigs = process_signature ~path:new_path sig_node in
    let rest = match rest_opt with
      | Some rest_node -> process_signature_binding ~path rest_node
      | None -> Context.create []
    in
    Info.merge sigs rest

(** Main entry point: traverse the AST and collect all constructor info *)
let rec get_context (prog : Ast.prog) : Info.t =
  let path = [] in
  let infos = process_prog ~path prog in
  infos

and process_prog ~(path : string list) (prog : Ast.prog) : Context.t =
  match prog with
  | ProgDec dec -> process_declaration ~path dec
  | ProgFun fct_bind -> process_functor_binding ~path fct_bind
  | ProgStr sig_bind -> process_signature_binding ~path sig_bind
  | ProgSeq (p1, p2) ->
    Info.merge (process_prog ~path (unbox_node p1)) (process_prog ~path (unbox_node p2))
  | ProgEmpty -> Context.create []

