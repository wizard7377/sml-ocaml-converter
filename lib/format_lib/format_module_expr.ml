open Astlib.Ast_414
open Asttypes
open Location
open Parsetree
open Format_utils
open Format_ident
open Format_flags

module Make
    (CT : Format_types.CORE_TYPE)
    (Expr : Format_types.EXPR)
    (Attrs : Format_types.ATTRS)
    (ClassT : Format_types.CLASS_TYPE)
    (CE : Format_types.CLASS_EXPR)
    (MT : Format_types.MODULE_TYPE)
    (TD : Format_types.TYPE_DECL) = struct

  let rec module_expr f x =
    if x.pmod_attributes <> [] then (
      Fmt.string f "((";
      module_expr f { x with pmod_attributes = [] };
      Fmt.string f ")";
      Attrs.attributes f x.pmod_attributes;
      Fmt.string f ")")
    else
      match x.pmod_desc with
      | Pmod_ident li -> longident_loc f li
      | Pmod_structure s ->
          Fmt.hvbox ~indent:2
            (fun f s ->
              Fmt.string f "struct";
              Fmt.sp f ();
              Fmt.box ~indent:0
                (fun f s -> list structure_item ~sep:Fmt.sp f s)
                f s;
              Fmt.sp f ();
              Fmt.string f "end")
            f s
      | Pmod_constraint (me, mt) ->
          box 2
            (fun f (me, mt) ->
              Fmt.string f "(";
              module_expr f me;
              Fmt.string f " : ";
              MT.module_type f mt;
              Fmt.string f ")")
            f (me, mt)
      | Pmod_functor (Unit, me) ->
          Fmt.hvbox
            (fun f me ->
              Fmt.string f "functor ()";
              Fmt.sp f ();
              Fmt.string f "->";
              Fmt.sp f ();
              module_expr f me)
            f me
      | Pmod_functor (Named (s, mt), me) ->
          Fmt.hvbox
            (fun f (s, mt, me) ->
              Fmt.string f "functor (";
              Fmt.string f (Option.value s ~default:"_");
              Fmt.string f " : ";
              MT.module_type f mt;
              Fmt.string f ")";
              Fmt.sp f ();
              Fmt.string f "->";
              Fmt.sp f ();
              module_expr f me)
            f (s.txt, mt, me)
      | Pmod_apply (me1, me2) ->
          (fstr "(" ++ Fmt.using fst module_expr ++ fstr ")("
         ++ Fmt.using snd module_expr ++ fstr ")")
            f (me1, me2)
      | Pmod_unpack e -> (fstr "(val " ++ Expr.expression ++ fstr ")") f e
      | Pmod_extension e -> Attrs.extension f e

  and structure f x = list ~sep:Fmt.sp structure_item f x

  and structure_item f x =
    begin match x.pstr_desc with
    | Pstr_eval (e, attrs) ->
        box 2
          (fun f () ->
            Fmt.string f ";;";
            Expr.expression f e;
            Attrs.item_attributes f attrs)
          f ()
    | Pstr_type (_, []) -> assert false
    | Pstr_type (rf, l) -> TD.type_def_list f (rf, true, l)
    | Pstr_value (rf, l) -> box 2 (fun f () -> Expr.bindings f (rf, l)) f ()
    | Pstr_typext te -> TD.type_extension f te
    | Pstr_exception ed -> Attrs.exception_declaration f ed
    | Pstr_module x ->
        let rec module_helper f = function
          | { pmod_desc = Pmod_functor (arg_opt, me'); pmod_attributes = [] } ->
              (match arg_opt with
              | Unit -> Fmt.string f "()"
              | Named (s, mt) ->
                  Fmt.string f "(";
                  Fmt.string f (Option.value s.txt ~default:"_");
                  Fmt.string f ":";
                  MT.module_type f mt;
                  Fmt.string f ")");
              module_helper f me'
          | me -> me
        in
        box 2
          (fun f () ->
            Fmt.string f "module ";
            Fmt.string f (Option.value x.pmb_name.txt ~default:"_");
            let me = module_helper f x.pmb_expr in
            (match me with
            | {
             pmod_desc =
               Pmod_constraint
                 (me', ({ pmty_desc = Pmty_ident _ | Pmty_signature _; _ } as mt));
             pmod_attributes = [];
            } ->
                Fmt.string f " :";
                Fmt.sp f ();
                MT.module_type f mt;
                Fmt.sp f ();
                Fmt.string f "=";
                Fmt.sp f ();
                module_expr f me'
            | _ ->
                Fmt.string f " = ";
                module_expr f me);
            Attrs.item_attributes f x.pmb_attributes)
          f ()
    | Pstr_open od ->
        box 2
          (fun f () ->
            Fmt.string f "open";
            Fmt.string f (override od.popen_override);
            Fmt.sp f ();
            module_expr f od.popen_expr;
            Attrs.item_attributes f od.popen_attributes)
          f ()
    | Pstr_modtype { pmtd_name = s; pmtd_type; pmtd_attributes } ->
        box 2
          (fun f () ->
            Fmt.string f "module type ";
            Fmt.string f s.txt;
            option ~first:(fun f () -> Fmt.string f " = ") MT.module_type f pmtd_type;
            Attrs.item_attributes f pmtd_attributes)
          f ()
    | Pstr_class l -> (
        let extract_class_args cl =
          let rec loop acc = function
            | { pcl_desc = Pcl_fun (l, eo, p, cl'); pcl_attributes = [] } ->
                loop ((l, eo, p) :: acc) cl'
            | cl -> (List.rev acc, cl)
          in
          let args, cl = loop [] cl in
          match cl with
          | { pcl_desc = Pcl_constraint (cl', ct); pcl_attributes = [] } ->
              (args, Some ct, cl')
          | _ -> (args, None, cl)
        in
        let class_decl kwd f x =
          let args, constr, cl = extract_class_args x.pci_expr in
          box 2
            (fun f () ->
              Fmt.string f kwd;
              Fmt.sp f ();
              virtual_flag x.pci_virt f ();
              ClassT.class_params_def f x.pci_params;
              Fmt.string f x.pci_name.txt;
              Fmt.sp f ();
              (list Expr.label_exp) f args;
              (option (fun f ct ->
                   Fmt.string f ": ";
                   Fmt.box (fun f ct -> ClassT.class_type f ct) f ct;
                   Fmt.string f " "))
                f constr;
              Fmt.string f "=";
              Fmt.sp f ();
              CE.class_expr f cl;
              Attrs.item_attributes f x.pci_attributes)
            f ()
        in
        match l with
        | [] -> ()
        | x :: xs ->
            Fmt.vbox
              (fun f (x, xs) ->
                class_decl "class" f x;
                Fmt.sp f ();
                list ~sep:Fmt.sp (class_decl "and") f xs)
              f (x, xs))
    | Pstr_class_type l -> ClassT.class_type_declaration_list f l
    | Pstr_primitive vd ->
        box 2
          (fun f () ->
            Fmt.string f "external ";
            protect_ident vd.pval_name.txt f vd.pval_name.txt;
            Fmt.string f " : ";
            CT.core_type f vd.pval_type;
            Attrs.value_description f vd;
            Attrs.item_attributes f vd.pval_attributes)
          f ()
    | Pstr_include incl ->
        box 2
          (fun f () ->
            Fmt.string f "include ";
            module_expr f incl.pincl_mod;
            Attrs.item_attributes f incl.pincl_attributes)
          f ()
    | Pstr_recmodule decls -> (
        let pmb_item kwd f pmb =
          let name = opt_val pmb.pmb_name.txt ~default:"_" in
          box 2
            (fun f () ->
              Fmt.string f kwd;
              Fmt.sp f ();
              Fmt.string f name;
              (match pmb.pmb_expr.pmod_desc with
              | Pmod_constraint (expr, typ) ->
                  Fmt.string f ":";
                  MT.module_type f typ;
                  Fmt.string f " = ";
                  module_expr f expr
              | _ ->
                  Fmt.string f " = ";
                  module_expr f pmb.pmb_expr);
              Attrs.item_attributes f pmb.pmb_attributes)
            f ()
        in
        match decls with
        | [] -> assert false
        | x :: xs ->
            Fmt.hvbox
              (fun f (x, xs) ->
                pmb_item "module rec" f x;
                list ~sep:(fun f () -> Fmt.sp f ()) (pmb_item "and") f xs)
              f (x, xs))
    | Pstr_attribute a -> Attrs.floating_attribute f a
    | Pstr_extension (e, a) ->
        Attrs.item_extension f e;
        Attrs.item_attributes f a
    end;
    top_sep f ()
end
