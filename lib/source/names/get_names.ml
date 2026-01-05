(** Implementation of AST name extraction.

    See {!Get_names} for the public interface documentation.

    This module implements a recursive visitor that traverses the SML AST
    and extracts all identifiers along with their syntactic contexts. *)

(** Helper to create a single-entry store *)
let make_entry ~(context : Store.name_context) ~(path : Name.path) (name : string) : Store.entry =
  { Store.name = Name.make_name ~package:Name.Global ~path ~root:name;
    action = Store.Noted;
    context }

let make_store ~context ~path name : Store.t =
  Store.create [make_entry ~context ~path name]

let merge_list (lists : Store.t list) : Store.t =
  List.fold_left Store.combine (Store.create []) lists

(** Extract the string from an idx node *)
let rec get_idx_name (idx : Ast.idx) : string =
  match idx with
  | IdxIdx s -> Ast.unbox_node s
  | IdxVar s -> Ast.unbox_node s
  | IdxLab s -> Ast.unbox_node s
  | IdxNum s -> Ast.unbox_node s
  | IdxLong parts ->
      (* For long ids, we typically want the last component *)
      (match List.rev parts with
       | [] -> ""
       | last :: _ -> get_idx_name (Ast.unbox_node last))

let rec get_names_prog (context : Store.name_context) (path : Name.path) (prog : Ast.prog) : Store.t =
  match prog with
  | ProgDec dec -> get_names_declaration context path dec
  | ProgFun body -> get_names_functor_binding path body
  | ProgStr str -> get_names_signature_binding path str
  | ProgSeq (p1, p2) ->
      merge_list [
        get_names_prog context path (Ast.unbox_node p1);
        get_names_prog context path (Ast.unbox_node p2)
      ]
  | ProgEmpty -> Store.create []

and get_names_declaration (context : Store.name_context) (path : Name.path) (dec : Ast.declaration Ast.node) : Store.t =
  match Ast.unbox_node dec with
  | ValDec (_tyvars, vb) -> get_names_value_binding path vb
  | FunDec fb -> get_names_function_binding path fb
  | TypDec td -> get_names_type_binding path td
  | DatDec (db, withtype_opt) ->
      merge_list [
        get_names_data_binding path db;
        (match withtype_opt with
         | Some tb -> get_names_type_binding path tb
         | None -> Store.create [])
      ]
  | ExnDec eb -> get_names_exn_bind path eb
  | StrDec sb -> get_names_structure_binding path sb
  | SeqDec ds -> merge_list (List.map (get_names_declaration context path) ds)
  | DataDecAlias (new_name, old_name) ->
      merge_list [
        make_store ~context:Store.Type ~path (get_idx_name (Ast.unbox_node new_name));
        make_store ~context:Store.Type ~path (get_idx_name (Ast.unbox_node old_name))
      ]
  | AbstractDec (db, withtype_opt, decs) ->
      merge_list [
        get_names_data_binding path db;
        (match withtype_opt with
         | Some tb -> get_names_type_binding path tb
         | None -> Store.create []);
        merge_list (List.map (get_names_declaration context path) decs)
      ]
  | LocalDec (d1, d2) ->
      merge_list [
        get_names_declaration context path d1;
        get_names_declaration context path d2
      ]
  | OpenDec names ->
      merge_list (List.map (fun n -> make_store ~context:Store.Structure ~path (get_idx_name (Ast.unbox_node n))) names)
  | FixityDec (_fixity, ids) ->
      merge_list (List.map (fun n -> make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node n))) ids)

(** Value binding: val x = e and y = f ... *)
and get_names_value_binding (path : Name.path) (vb : Ast.value_binding Ast.node) : Store.t =
  match Ast.unbox_node vb with
  | ValBind (pat, exp, rest) ->
      merge_list [
        get_names_pat path pat;
        get_names_expression path exp;
        (match rest with
         | Some vb' -> get_names_value_binding path vb'
         | None -> Store.create [])
      ]
  | ValBindRec vb' -> get_names_value_binding path vb'

(** Function binding: fun f x = e and g y = f ... *)
and get_names_function_binding (path : Name.path) (fb : Ast.function_binding Ast.node) : Store.t =
  match Ast.unbox_node fb with
  | FunBind (fm, rest) ->
      merge_list [
        get_names_fun_match path fm;
        (match rest with
         | Some fb' -> get_names_function_binding path fb'
         | None -> Store.create [])
      ]

(** Function match clauses *)
and get_names_fun_match (path : Name.path) (fm : Ast.fun_match Ast.node) : Store.t =
  match Ast.unbox_node fm with
  | FunMatchPrefix (with_op, pats, typ_opt, exp, rest) ->
      merge_list [
        get_names_with_op Store.Value path with_op;
        merge_list (List.map (get_names_pat path) pats);
        (match typ_opt with Some t -> get_names_typ path t | None -> Store.create []);
        get_names_expression path exp;
        (match rest with Some fm' -> get_names_fun_match path fm' | None -> Store.create [])
      ]
  | FunMatchInfix (pat1, op, pat2, typ_opt, exp, rest) ->
      merge_list [
        get_names_pat path pat1;
        make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node op));
        get_names_pat path pat2;
        (match typ_opt with Some t -> get_names_typ path t | None -> Store.create []);
        get_names_expression path exp;
        (match rest with Some fm' -> get_names_fun_match path fm' | None -> Store.create [])
      ]
  | FunMatchLow (pat1, op, pat2, pats, typ_opt, exp, rest) ->
      merge_list [
        get_names_pat path pat1;
        make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node op));
        get_names_pat path pat2;
        merge_list (List.map (get_names_pat path) pats);
        (match typ_opt with Some t -> get_names_typ path t | None -> Store.create []);
        get_names_expression path exp;
        (match rest with Some fm' -> get_names_fun_match path fm' | None -> Store.create [])
      ]

(** Type binding: type 'a t = ... *)
and get_names_type_binding (path : Name.path) (tb : Ast.type_binding Ast.node) : Store.t =
  match Ast.unbox_node tb with
  | TypBind (tyvars, name, typ, rest) ->
      merge_list [
        merge_list (List.map (fun tv -> make_store ~context:Store.Variable ~path (get_idx_name (Ast.unbox_node tv))) tyvars);
        make_store ~context:Store.Type ~path (get_idx_name (Ast.unbox_node name));
        get_names_typ path typ;
        (match rest with Some tb' -> get_names_type_binding path tb' | None -> Store.create [])
      ]

(** Datatype binding: datatype 'a t = A | B of ... *)
and get_names_data_binding (path : Name.path) (db : Ast.data_binding Ast.node) : Store.t =
  match Ast.unbox_node db with
  | DatBind (tyvars, name, conbind, rest) ->
      let type_name = get_idx_name (Ast.unbox_node name) in
      let new_path = path @ [type_name] in
      merge_list [
        merge_list (List.map (fun tv -> make_store ~context:Store.Variable ~path (get_idx_name (Ast.unbox_node tv))) tyvars);
        make_store ~context:Store.Type ~path type_name;
        get_names_constructor_binding new_path conbind;
        (match rest with Some db' -> get_names_data_binding path db' | None -> Store.create [])
      ]

(** Constructor binding: A | B of int | C of string * int *)
and get_names_constructor_binding (path : Name.path) (cb : Ast.constructor_binding Ast.node) : Store.t =
  match Ast.unbox_node cb with
  | ConBind (name, typ_opt, rest) ->
      merge_list [
        make_store ~context:Store.Constructor ~path (get_idx_name (Ast.unbox_node name));
        (match typ_opt with Some t -> get_names_typ path t | None -> Store.create []);
        (match rest with Some cb' -> get_names_constructor_binding path cb' | None -> Store.create [])
      ]

(** Exception binding: exception E | exception E of string *)
and get_names_exn_bind (path : Name.path) (eb : Ast.exn_bind Ast.node) : Store.t =
  match Ast.unbox_node eb with
  | ExnBind (name, typ_opt, rest) ->
      merge_list [
        make_store ~context:Store.Constructor ~path (get_idx_name (Ast.unbox_node name));
        (match typ_opt with Some t -> get_names_typ path t | None -> Store.create []);
        (match rest with Some eb' -> get_names_exn_bind path eb' | None -> Store.create [])
      ]
  | ExnBindAlias (new_name, old_name, rest) ->
      merge_list [
        make_store ~context:Store.Constructor ~path (get_idx_name (Ast.unbox_node new_name));
        make_store ~context:Store.Constructor ~path (get_idx_name (Ast.unbox_node old_name));
        (match rest with Some eb' -> get_names_exn_bind path eb' | None -> Store.create [])
      ]

(** Structure binding: structure S = struct ... end *)
and get_names_structure_binding (path : Name.path) (sb : Ast.structure_binding Ast.node) : Store.t =
  match Ast.unbox_node sb with
  | StrBind (name, annot_opt, rest) ->
      merge_list [
        make_store ~context:Store.Structure ~path (get_idx_name (Ast.unbox_node name));
        (match annot_opt with
         | Some (_annot, sig_node) -> get_names_signature path sig_node
         | None -> Store.create []);
        (match rest with Some sb' -> get_names_structure_binding path sb' | None -> Store.create [])
      ]

(** with_op handling: op id or id *)
and get_names_with_op (context : Store.name_context) (path : Name.path) (wo : Ast.with_op Ast.node) : Store.t =
  match Ast.unbox_node wo with
  | WithOp idx -> make_store ~context ~path (get_idx_name (Ast.unbox_node idx))
  | WithoutOp idx -> make_store ~context ~path (get_idx_name (Ast.unbox_node idx))

(** Expression visitor *)
and get_names_expression (path : Name.path) (exp : Ast.expression Ast.node) : Store.t =
  match Ast.unbox_node exp with
  | ExpCon _ -> Store.create []  (* Constants don't introduce names *)
  | ExpIdx idx -> make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node idx))
  | ExpApp (e1, e2) ->
      merge_list [get_names_expression path e1; get_names_expression path e2]
  | InfixApp (e1, op, e2) ->
      merge_list [
        get_names_expression path e1;
        make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node op));
        get_names_expression path e2
      ]
  | ParenExp e -> get_names_expression path e
  | TupleExp es -> merge_list (List.map (get_names_expression path) es)
  | RecordExp rows -> merge_list (List.map (get_names_row path) rows)
  | RecordSelector idx -> make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node idx))
  | ListExp es -> merge_list (List.map (get_names_expression path) es)
  | SeqExp es -> merge_list (List.map (get_names_expression path) es)
  | LetExp (decs, es) ->
      merge_list [
        merge_list (List.map (get_names_declaration Store.Value path) decs);
        merge_list (List.map (get_names_expression path) es)
      ]
  | TypedExp (e, t) ->
      merge_list [get_names_expression path e; get_names_typ path t]
  | RaiseExp e -> get_names_expression path e
  | HandleExp (e, m) ->
      merge_list [get_names_expression path e; get_names_matching path m]
  | AndExp (e1, e2) ->
      merge_list [get_names_expression path e1; get_names_expression path e2]
  | OrExp (e1, e2) ->
      merge_list [get_names_expression path e1; get_names_expression path e2]
  | IfExp (e1, e2, e3) ->
      merge_list [
        get_names_expression path e1;
        get_names_expression path e2;
        get_names_expression path e3
      ]
  | WhileExp (e1, e2) ->
      merge_list [get_names_expression path e1; get_names_expression path e2]
  | CaseExp (e, m) ->
      merge_list [get_names_expression path e; get_names_matching path m]
  | FnExp m -> get_names_matching path m

(** Row visitor for record expressions *)
and get_names_row (path : Name.path) (row : Ast.row Ast.node) : Store.t =
  match Ast.unbox_node row with
  | Row (label, exp, rest) ->
      merge_list [
        make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node label));
        get_names_expression path exp;
        (match rest with Some r -> get_names_row path r | None -> Store.create [])
      ]

(** Match/case visitor *)
and get_names_matching (path : Name.path) (m : Ast.matching Ast.node) : Store.t =
  match Ast.unbox_node m with
  | Case (pat, exp, rest) ->
      merge_list [
        get_names_pat path pat;
        get_names_expression path exp;
        (match rest with Some m' -> get_names_matching path m' | None -> Store.create [])
      ]

(** Pattern visitor *)
and get_names_pat (path : Name.path) (pat : Ast.pat Ast.node) : Store.t =
  match Ast.unbox_node pat with
  | PatCon _ -> Store.create []  (* Constants don't introduce names *)
  | PatWildcard -> Store.create []  (* Wildcards don't introduce names *)
  | PatIdx wo -> get_names_with_op Store.Value path wo
  | PatApp (wo, p) ->
      merge_list [
        get_names_with_op Store.Constructor path wo;
        get_names_pat path p
      ]
  | PatInfix (p1, op, p2) ->
      merge_list [
        get_names_pat path p1;
        make_store ~context:Store.Constructor ~path (get_idx_name (Ast.unbox_node op));
        get_names_pat path p2
      ]
  | PatParen p -> get_names_pat path p
  | PatTuple ps -> merge_list (List.map (get_names_pat path) ps)
  | PatRecord rows -> merge_list (List.map (get_names_pat_row path) rows)
  | PatList ps -> merge_list (List.map (get_names_pat path) ps)
  | PatTyp (p, t) ->
      merge_list [get_names_pat path p; get_names_typ path t]
  | PatAs (wo, typ_opt, p) ->
      merge_list [
        get_names_with_op Store.Value path wo;
        (match typ_opt with Some t -> get_names_typ path t | None -> Store.create []);
        get_names_pat path p
      ]

(** Pattern row visitor *)
and get_names_pat_row (path : Name.path) (row : Ast.pat_row Ast.node) : Store.t =
  match Ast.unbox_node row with
  | PatRowPoly -> Store.create []
  | PatRowSimple (label, pat, rest) ->
      merge_list [
        make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node label));
        get_names_pat path pat;
        get_names_pat_row path rest
      ]
  | PatRowVar (label, typ_opt, as_opt, rest) ->
      merge_list [
        make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node label));
        (match typ_opt with Some t -> get_names_typ path t | None -> Store.create []);
        (match as_opt with Some idx -> make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node idx)) | None -> Store.create []);
        (match rest with Some r -> get_names_pat_row path r | None -> Store.create [])
      ]

(** Type visitor *)
and get_names_typ (path : Name.path) (typ : Ast.typ Ast.node) : Store.t =
  match Ast.unbox_node typ with
  | TypVar idx -> make_store ~context:Store.Variable ~path (get_idx_name (Ast.unbox_node idx))
  | TypCon (typs, idx) ->
      merge_list [
        merge_list (List.map (get_names_typ path) typs);
        make_store ~context:Store.Type ~path (get_idx_name (Ast.unbox_node idx))
      ]
  | TypPar t -> get_names_typ path t
  | TypFun (t1, t2) ->
      merge_list [get_names_typ path t1; get_names_typ path t2]
  | TypTuple ts -> merge_list (List.map (get_names_typ path) ts)
  | TypRecord rows -> merge_list (List.map (get_names_typ_row path) rows)

(** Type row visitor *)
and get_names_typ_row (path : Name.path) (row : Ast.typ_row Ast.node) : Store.t =
  match Ast.unbox_node row with
  | TypRow (label, typ, rest) ->
      merge_list [
        make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node label));
        get_names_typ path typ;
        (match rest with Some r -> get_names_typ_row path r | None -> Store.create [])
      ]

(** Structure visitor *)
and get_names_structure (path : Name.path) (str : Ast.structure Ast.node) : Store.t =
  match Ast.unbox_node str with
  | StrIdx idx -> make_store ~context:Store.Structure ~path (get_idx_name (Ast.unbox_node idx))
  | StructStr dec -> get_names_declaration Store.Value path dec
  | AnotateStr (name, _annot, str') ->
      merge_list [
        make_store ~context:Store.Structure ~path (get_idx_name (Ast.unbox_node name));
        get_names_structure path str'
      ]
  | FunctorApp (name, str') ->
      merge_list [
        make_store ~context:Store.Structure ~path (get_idx_name (Ast.unbox_node name));
        get_names_structure path str'
      ]
  | FunctorAppAnonymous (name, dec) ->
      merge_list [
        make_store ~context:Store.Structure ~path (get_idx_name (Ast.unbox_node name));
        get_names_declaration Store.Value path dec
      ]
  | LocalDec (dec, str') ->
      merge_list [
        get_names_declaration Store.Value path dec;
        get_names_structure path str'
      ]

(** Signature visitor *)
and get_names_signature (path : Name.path) (sig_ : Ast.signature Ast.node) : Store.t =
  match Ast.unbox_node sig_ with
  | SignIdx idx -> make_store ~context:Store.Signature ~path (get_idx_name (Ast.unbox_node idx))
  | SignSig specs -> merge_list (List.map (get_names_specification path) specs)
  | SignWhere (sig', refine) ->
      merge_list [
        get_names_signature path sig';
        get_names_typ_refine path refine
      ]

(** Type refinement visitor *)
and get_names_typ_refine (path : Name.path) (refine : Ast.typ_refine Ast.node) : Store.t =
  match Ast.unbox_node refine with
  | TypRef (tyvars, name, typ, rest) ->
      merge_list [
        merge_list (List.map (fun tv -> make_store ~context:Store.Variable ~path (get_idx_name (Ast.unbox_node tv))) tyvars);
        make_store ~context:Store.Type ~path (get_idx_name (Ast.unbox_node name));
        get_names_typ path typ;
        (match rest with Some (t, r) -> merge_list [get_names_typ path t; get_names_typ_refine path r] | None -> Store.create [])
      ]

(** Specification visitor *)
and get_names_specification (path : Name.path) (spec : Ast.specification Ast.node) : Store.t =
  match Ast.unbox_node spec with
  | SpecVal vs -> get_names_val_specification path vs
  | SpecTyp ts -> get_names_typ_specification path ts
  | SpecEqtyp ts -> get_names_typ_specification path ts
  | SpecTypBind tb -> get_names_type_binding path tb
  | SpecDat ds -> get_names_dat_specification path ds
  | SpecDatAlias (new_name, old_name) ->
      merge_list [
        make_store ~context:Store.Type ~path (get_idx_name (Ast.unbox_node new_name));
        make_store ~context:Store.Type ~path (get_idx_name (Ast.unbox_node old_name))
      ]
  | SpecExn es -> get_names_exn_specification path es
  | SpecStr ss -> get_names_str_specification path ss
  | SpecSeq (s1, s2) ->
      merge_list [get_names_specification path s1; get_names_specification path s2]
  | SpecInclude sig_ -> get_names_signature path sig_
  | SpecIncludeIdx ids ->
      merge_list (List.map (fun id -> make_store ~context:Store.Signature ~path (get_idx_name (Ast.unbox_node id))) ids)
  | SpecSharingTyp (spec, ids) ->
      merge_list [
        get_names_specification path spec;
        merge_list (List.map (fun id -> make_store ~context:Store.Type ~path (get_idx_name (Ast.unbox_node id))) ids)
      ]
  | SpecSharingStr (spec, ids) ->
      merge_list [
        get_names_specification path spec;
        merge_list (List.map (fun id -> make_store ~context:Store.Structure ~path (get_idx_name (Ast.unbox_node id))) ids)
      ]

(** Value specification visitor *)
and get_names_val_specification (path : Name.path) (vs : Ast.val_specification Ast.node) : Store.t =
  match Ast.unbox_node vs with
  | ValDesc (name, typ, rest) ->
      merge_list [
        make_store ~context:Store.Value ~path (get_idx_name (Ast.unbox_node name));
        get_names_typ path typ;
        (match rest with Some vs' -> get_names_val_specification path vs' | None -> Store.create [])
      ]

(** Type specification visitor *)
and get_names_typ_specification (path : Name.path) (ts : Ast.typ_specification Ast.node) : Store.t =
  match Ast.unbox_node ts with
  | TypDesc (tyvars, name, rest) ->
      merge_list [
        merge_list (List.map (fun tv -> make_store ~context:Store.Variable ~path (get_idx_name (Ast.unbox_node tv))) tyvars);
        make_store ~context:Store.Type ~path (get_idx_name (Ast.unbox_node name));
        (match rest with Some ts' -> get_names_typ_specification path ts' | None -> Store.create [])
      ]

(** Datatype specification visitor *)
and get_names_dat_specification (path : Name.path) (ds : Ast.dat_specification Ast.node) : Store.t =
  match Ast.unbox_node ds with
  | DatDesc (tyvars, name, cs, rest) ->
      let type_name = get_idx_name (Ast.unbox_node name) in
      let new_path = path @ [type_name] in
      merge_list [
        merge_list (List.map (fun tv -> make_store ~context:Store.Variable ~path (get_idx_name (Ast.unbox_node tv))) tyvars);
        make_store ~context:Store.Type ~path type_name;
        get_names_con_specification new_path cs;
        (match rest with Some ds' -> get_names_dat_specification path ds' | None -> Store.create [])
      ]

(** Constructor specification visitor *)
and get_names_con_specification (path : Name.path) (cs : Ast.con_specification Ast.node) : Store.t =
  match Ast.unbox_node cs with
  | ConDesc (name, typ_opt, rest) ->
      merge_list [
        make_store ~context:Store.Constructor ~path (get_idx_name (Ast.unbox_node name));
        (match typ_opt with Some t -> get_names_typ path t | None -> Store.create []);
        (match rest with Some cs' -> get_names_con_specification path cs' | None -> Store.create [])
      ]

(** Exception specification visitor *)
and get_names_exn_specification (path : Name.path) (es : Ast.exn_specification Ast.node) : Store.t =
  match Ast.unbox_node es with
  | ExnDesc (name, typ_opt, rest) ->
      merge_list [
        make_store ~context:Store.Constructor ~path (get_idx_name (Ast.unbox_node name));
        (match typ_opt with Some t -> get_names_typ path t | None -> Store.create []);
        (match rest with Some es' -> get_names_exn_specification path es' | None -> Store.create [])
      ]

(** Structure specification visitor *)
and get_names_str_specification (path : Name.path) (ss : Ast.str_specification Ast.node) : Store.t =
  match Ast.unbox_node ss with
  | StrDesc (name, sig_, rest) ->
      merge_list [
        make_store ~context:Store.Structure ~path (get_idx_name (Ast.unbox_node name));
        get_names_signature path sig_;
        (match rest with Some ss' -> get_names_str_specification path ss' | None -> Store.create [])
      ]

(** Functor binding visitor *)
and get_names_functor_binding (path : Name.path) (fb : Ast.functor_binding Ast.node) : Store.t =
  match Ast.unbox_node fb with
  | FctBind (name, param, param_sig, result_opt, body, rest) ->
      let fct_name = get_idx_name (Ast.unbox_node name) in
      let new_path = path @ [fct_name] in
      merge_list [
        make_store ~context:Store.Structure ~path fct_name;
        make_store ~context:Store.Structure ~path:new_path (get_idx_name (Ast.unbox_node param));
        get_names_signature new_path param_sig;
        (match result_opt with
         | Some (_annot, sig_) -> get_names_signature new_path sig_
         | None -> Store.create []);
        get_names_structure new_path body;
        (match rest with Some fb' -> get_names_functor_binding path fb' | None -> Store.create [])
      ]
  | FctBindOpen (name, spec, result_opt, body, rest) ->
      let fct_name = get_idx_name (Ast.unbox_node name) in
      let new_path = path @ [fct_name] in
      merge_list [
        make_store ~context:Store.Structure ~path fct_name;
        get_names_specification new_path spec;
        (match result_opt with
         | Some (_annot, sig_) -> get_names_signature new_path sig_
         | None -> Store.create []);
        get_names_structure new_path body;
        (match rest with Some fb' -> get_names_functor_binding path fb' | None -> Store.create [])
      ]

(** Signature binding visitor *)
and get_names_signature_binding (path : Name.path) (sb : Ast.signature_binding Ast.node) : Store.t =
  match Ast.unbox_node sb with
  | SignBind (name, sig_, rest) ->
      merge_list [
        make_store ~context:Store.Signature ~path (get_idx_name (Ast.unbox_node name));
        get_names_signature path sig_;
        (match rest with Some sb' -> get_names_signature_binding path sb' | None -> Store.create [])
      ]




let get_names (prog : Ast.prog) : Store.t = get_names_prog Store.Structure [] prog 