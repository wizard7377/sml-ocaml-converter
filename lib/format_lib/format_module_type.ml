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
    (ClassT : Format_types.CLASS_TYPE)
    (TD : Format_types.TYPE_DECL)
    (ME : Format_types.MODULE_EXPR) = struct

  let rec module_type f x =
    if x.pmty_attributes <> [] then (
      Fmt.string f "((";
      module_type f { x with pmty_attributes = [] };
      Fmt.string f ")";
      Attrs.attributes f x.pmty_attributes;
      Fmt.string f ")")
    else
      match x.pmty_desc with
      | Pmty_ident li -> longident_loc f li
      | Pmty_alias li ->
          Fmt.string f "(module ";
          longident_loc f li;
          Fmt.string f ")"
      | Pmty_signature s ->
          Fmt.hvbox
            (fun f s ->
              Fmt.hvbox
                (fun f s ->
                  Fmt.string f "sig";
                  Fmt.sp f ();
                  list signature_item f s)
                f s;
              Fmt.sp f ();
              Fmt.string f "end")
            f s
      | Pmty_functor (Unit, mt2) ->
          Fmt.hvbox
            (fun f mt2 ->
              Fmt.string f "functor () ->";
              Fmt.sp f ();
              module_type f mt2)
            f mt2
      | Pmty_functor (Named (s, mt1), mt2) -> (
          match s.txt with
          | None ->
              Fmt.hvbox
                (fun f (mt1, mt2) ->
                  module_type f mt1;
                  Fmt.sp f ();
                  Fmt.string f "->";
                  Fmt.sp f ();
                  module_type f mt2)
                f (mt1, mt2)
          | Some name ->
              Fmt.hvbox
                (fun f (name, mt1, mt2) ->
                  Fmt.string f "functor";
                  Fmt.sp f ();
                  Fmt.string f "(";
                  Fmt.string f name;
                  Fmt.sp f ();
                  Fmt.string f ":";
                  Fmt.sp f ();
                  module_type f mt1;
                  Fmt.string f ")";
                  Fmt.sp f ();
                  Fmt.string f "->";
                  Fmt.sp f ();
                  module_type f mt2)
                f (name, mt1, mt2))
      | Pmty_with (mt, []) -> module_type f mt
      | Pmty_with (mt, l) ->
          Fmt.hvbox
            (fun f (mt, l) ->
              module_type f mt;
              Fmt.sp f ();
              Fmt.string f "with";
              Fmt.sp f ();
              list with_constraint
                ~sep:(fun f () ->
                  Fmt.sp f ();
                  Fmt.string f "and";
                  Fmt.sp f ())
                f l)
            f (mt, l)
      | Pmty_typeof me ->
          Fmt.hvbox
            (fun f me ->
              Fmt.string f "module";
              Fmt.sp f ();
              Fmt.string f "type";
              Fmt.sp f ();
              Fmt.string f "of";
              Fmt.sp f ();
              ME.module_expr f me)
            f me
      | Pmty_extension e -> Attrs.extension f e

  and with_constraint f =
    let type_params f = function
      | [] -> ()
      | [ p ] -> CT.core_type f p
      | ps -> (parens (list CT.core_type ~sep:(comma ++ Fmt.sp))) f ps
    in
    function
    | Pwith_type (li, td) ->
        Fmt.string f "type";
        Fmt.sp f ();
        type_params f (List.map fst td.ptype_params);
        Fmt.sp f ();
        longident_loc f li;
        Fmt.sp f ();
        Fmt.string f "=";
        Fmt.sp f ();
        TD.type_declaration f td
    | Pwith_typesubst (li, td) ->
        Fmt.string f "type";
        Fmt.sp f ();
        type_params f (List.map fst td.ptype_params);
        Fmt.sp f ();
        longident_loc f li;
        Fmt.sp f ();
        Fmt.string f ":=";
        Fmt.sp f ();
        TD.type_declaration f td
    | Pwith_module (li, li2) ->
        Fmt.string f "module";
        Fmt.sp f ();
        longident_loc f li;
        Fmt.sp f ();
        Fmt.string f "=";
        Fmt.sp f ();
        longident_loc f li2
    | Pwith_modsubst (li, li2) ->
        Fmt.string f "module";
        Fmt.sp f ();
        longident_loc f li;
        Fmt.sp f ();
        Fmt.string f ":=";
        Fmt.sp f ();
        longident_loc f li2
    | Pwith_modtype (li, mty) ->
        (fstr "module type" ++ Fmt.sp
        ++ Fmt.using fst longident_loc
        ++ Fmt.using snd module_type)
          f (li, mty)
    | Pwith_modtypesubst (li, mty) ->
        Fmt.string f "module type";
        Fmt.sp f ();
        longident_loc f li;
        Fmt.sp f ();
        Fmt.string f ":=";
        Fmt.sp f ();
        module_type f mty

  and signature f x = list ~sep:Fmt.sp signature_item f x

  and signature_item f x =
    match x.psig_desc with
    | Psig_type (rf, l) -> TD.type_def_list f (rf, true, l)
    | Psig_typesubst l -> TD.type_def_list f (Recursive, false, l)
    | Psig_value vd ->
        let intro = if vd.pval_prim = [] then "val" else "external" in
        box 2
          (fun f () ->
            Fmt.string f intro;
            Fmt.sp f ();
            protect_ident vd.pval_name.txt f vd.pval_name.txt;
            Fmt.string f " : ";
            CT.core_type f vd.pval_type;
            Attrs.value_description f vd;
            Attrs.item_attributes f vd.pval_attributes)
          f ()
    | Psig_typext te -> TD.type_extension f te
    | Psig_exception ed -> Attrs.exception_declaration f ed
    | Psig_class l -> (
        let class_desc kwd f x =
          box 2
            (fun f () ->
              Fmt.string f kwd;
              Fmt.sp f ();
              virtual_flag x.pci_virt f ();
              ClassT.class_params_def f x.pci_params;
              Fmt.string f x.pci_name.txt;
              Fmt.string f " :";
              Fmt.sp f ();
              ClassT.class_type f x.pci_expr;
              Attrs.item_attributes f x.pci_attributes)
            f ()
        in
        match l with
        | [] -> ()
        | x :: xs ->
            Fmt.vbox
              (fun f (x, xs) ->
                class_desc "class" f x;
                Fmt.sp f ();
                list ~sep:Fmt.sp (class_desc "and") f xs)
              f (x, xs))
    | Psig_module
        ({ pmd_type = { pmty_desc = Pmty_alias alias; pmty_attributes = [] }; _ }
         as pmd) ->
        Fmt.hvbox
          (fun f () ->
            Fmt.string f "module ";
            Fmt.string f (Option.value pmd.pmd_name.txt ~default:"_");
            Fmt.string f " = ";
            longident_loc f alias;
            Attrs.item_attributes f pmd.pmd_attributes)
          f ()
    | Psig_module pmd ->
        Fmt.hvbox
          (fun f () ->
            Fmt.string f "module ";
            Fmt.string f (Option.value pmd.pmd_name.txt ~default:"_");
            Fmt.string f " : ";
            module_type f pmd.pmd_type;
            Attrs.item_attributes f pmd.pmd_attributes)
          f ()
    | Psig_modsubst pms ->
        Fmt.hvbox
          (fun f () ->
            Fmt.string f "module ";
            Fmt.string f pms.pms_name.txt;
            Fmt.string f " := ";
            longident_loc f pms.pms_manifest;
            Attrs.item_attributes f pms.pms_attributes)
          f ()
    | Psig_open od ->
        box 2
          (fun f () ->
            Fmt.string f "open";
            Fmt.string f (override od.popen_override);
            Fmt.sp f ();
            longident_loc f od.popen_expr;
            Attrs.item_attributes f od.popen_attributes)
          f ()
    | Psig_include incl ->
        box 2
          (fun f () ->
            Fmt.string f "include ";
            module_type f incl.pincl_mod;
            Attrs.item_attributes f incl.pincl_attributes)
          f ()
    | Psig_modtype { pmtd_name = s; pmtd_type; pmtd_attributes } ->
        box 2
          (fun f () ->
            Fmt.string f "module type ";
            Fmt.string f s.txt;
            option ~first:(fun f () -> Fmt.string f " = ") module_type f pmtd_type;
            Attrs.item_attributes f pmtd_attributes)
          f ()
    | Psig_modtypesubst { pmtd_name = s; pmtd_type = Some mt; pmtd_attributes } ->
        box 2
          (fun f () ->
            Fmt.string f "module type ";
            Fmt.string f s.txt;
            Fmt.string f " := ";
            module_type f mt;
            Attrs.item_attributes f pmtd_attributes)
          f ()
    | Psig_modtypesubst { pmtd_type = None; _ } -> assert false
    | Psig_class_type l -> ClassT.class_type_declaration_list f l
    | Psig_recmodule decls -> (
        let pmd_item kwd f pmd =
          box 2
            (fun f () ->
              Fmt.string f kwd;
              Fmt.sp f ();
              Fmt.string f (opt_val pmd.pmd_name.txt ~default:"_");
              Fmt.string f ": ";
              module_type f pmd.pmd_type;
              Attrs.item_attributes f pmd.pmd_attributes)
            f ()
        in
        match decls with
        | [] -> ()
        | x :: xs ->
            pmd_item "module rec" f x;
            list ~sep:Fmt.sp (pmd_item "and") f xs)
    | Psig_attribute a -> Attrs.floating_attribute f a
    | Psig_extension (e, a) ->
        Attrs.item_extension f e;
        Attrs.item_attributes f a
end
