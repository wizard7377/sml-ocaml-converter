open Astlib.Ast_414
open Asttypes
open Location
open Parsetree
open Format_utils
open Format_ident
open Format_flags

module Make
    (CT : Format_types.CORE_TYPE)
    (Attrs : Format_types.ATTRS)
    (ClassT : Format_types.CLASS_TYPE) = struct

  let rec type_params f = function
    | [] -> ()
    | [ p ] -> (ClassT.type_param ++ Fmt.sp) f p
    | l -> (parens (list ClassT.type_param ~sep:(comma ++ Fmt.sp)) ++ Fmt.sp) f l

  and type_def_list f (rf, exported, l) =
    let type_decl kwd rf f x =
      let eq =
        if x.ptype_kind = Ptype_abstract && x.ptype_manifest = None then ""
        else if exported then " ="
        else " :="
      in
      box 2
        (fun f () ->
          Fmt.string f kwd;
          Fmt.sp f ();
          nonrec_flag rf f ();
          type_params f x.ptype_params;
          Fmt.string f x.ptype_name.txt;
          Fmt.string f eq;
          type_declaration f x;
          Attrs.item_attributes f x.ptype_attributes)
        f ()
    in
    match l with
    | [] -> assert false
    | [ x ] -> type_decl "type" rf f x
    | x :: xs ->
        Fmt.vbox
          (fun f (x, xs) ->
            type_decl "type" rf f x;
            Fmt.sp f ();
            list ~sep:Fmt.sp (type_decl "and" Recursive) f xs)
          f (x, xs)

  and record_declaration f lbls =
    let type_record_field f pld =
      box 2
        (fun f () ->
          mutable_flag pld.pld_mutable f ();
          Fmt.string f pld.pld_name.txt;
          Fmt.string f ":";
          Fmt.sp f ();
          CT.core_type f pld.pld_type;
          Fmt.sp f ();
          Attrs.attributes f pld.pld_attributes)
        f ()
    in
    Fmt.string f "{";
    Fmt.sp f ();
    list type_record_field
      ~sep:(fun f () ->
        Fmt.string f ";";
        Fmt.sp f ())
      f lbls;
    Fmt.string f "}"

  and type_declaration f x =
    let priv f =
      if x.ptype_private = Private then (
        Fmt.sp f ();
        Fmt.string f "private")
    in
    let ctor f pcd =
      Fmt.string f "|";
      Fmt.sp f ();
      constructor_decl f
        ( pcd.pcd_name.txt,
          pcd.pcd_vars,
          pcd.pcd_args,
          pcd.pcd_res,
          pcd.pcd_attributes )
    in
    (match x.ptype_manifest with
    | None -> ()
    | Some y ->
        if x.ptype_kind = Ptype_abstract then (
          priv f;
          Fmt.sp f ();
          CT.core_type f y)
        else (
          Fmt.sp f ();
          CT.core_type f y));
    let intro f =
      if x.ptype_manifest <> None then (
        Fmt.sp f ();
        Fmt.string f "=")
    in
    (match x.ptype_kind with
    | Ptype_variant [] ->
        intro f;
        priv f;
        Fmt.string f "|"
    | Ptype_variant xs ->
        intro f;
        priv f;
        Fmt.sp f ();
        list ~sep:Fmt.sp ctor f xs
    | Ptype_abstract -> ()
    | Ptype_record l ->
        intro f;
        priv f;
        Fmt.sp f ();
        record_declaration f l
    | Ptype_open ->
        intro f;
        priv f;
        Fmt.sp f ();
        Fmt.string f "..");
    List.iter
      (fun (ct1, ct2, _) ->
        box 2
          (fun f () ->
            Fmt.string f " constraint ";
            CT.core_type f ct1;
            Fmt.string f " = ";
            CT.core_type f ct2)
          f ())
      x.ptype_cstrs

  and type_extension f x =
    let extension_constructor_item f x =
      Fmt.sp f ();
      Fmt.string f "|";
      Fmt.sp f ();
      extension_constructor f x
    in
    box 2
      (fun f () ->
        Fmt.string f "type ";
        (match x.ptyext_params with
        | [] -> ()
        | l ->
            Fmt.string f "(";
            list ClassT.type_param ~sep:(fun f () -> Fmt.string f ",") f l;
            Fmt.string f ")";
            Fmt.sp f ());
        longident_loc f x.ptyext_path;
        Fmt.string f " += ";
        private_flag x.ptyext_private f ();
        Fmt.sp f ();
        (list ~sep:Fmt.nop extension_constructor_item) f x.ptyext_constructors;
        Attrs.item_attributes f x.ptyext_attributes)
      f ()

  and constructor_decl f (name, vars, args, res, attrs : _ * _ * constructor_arguments * _ * _) =
    let name = if name = "::" then "(::)" else name in
    let pp_args f = function
      | Pcstr_tuple [] -> ()
      | Pcstr_tuple l ->
          list (Fmt.parens CT.core_type)
            ~sep:(fun f () ->
              Fmt.sp f ();
              Fmt.string f "*";
              Fmt.sp f ())
            f l
      | Pcstr_record l -> record_declaration f l
    in
    match res with
    | None ->
        Fmt.string f name;
        (match args with
        | Pcstr_tuple [] -> ()
        | a ->
            Fmt.sp f ();
            Fmt.string f "of";
            Fmt.sp f ();
            pp_args f a);
        Fmt.sp f ();
        Attrs.attributes f attrs
    | Some r ->
        let pp_vars f = function
          | [] -> ()
          | vs ->
              list tyvar_loc ~sep:(fun f () -> Fmt.sp f ()) f vs;
              Fmt.sp f ();
              Fmt.string f ".";
              Fmt.sp f ()
        in
        Fmt.string f name;
        Fmt.string f ":";
        Fmt.sp f ();
        pp_vars f vars;
        (match args with
        | Pcstr_tuple [] -> CT.core_type f r
        | a ->
            pp_args f a;
            Fmt.sp f ();
            Fmt.string f "->";
            Fmt.sp f ();
            CT.core_type f r);
        Fmt.sp f ();
        Attrs.attributes f attrs

  and extension_constructor f x =
    match x.pext_kind with
    | Pext_decl (v, l, r) ->
        constructor_decl f (x.pext_name.txt, v, l, r, x.pext_attributes)
    | Pext_rebind li ->
        Fmt.string f x.pext_name.txt;
        Fmt.sp f ();
        Fmt.string f "=";
        Fmt.sp f ();
        longident_loc f li;
        Attrs.attributes f x.pext_attributes
end
