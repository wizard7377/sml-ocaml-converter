open Astlib.Ast_414
open Location
open Parsetree
open Format_utils
open Format_ident
open Format_flags

module Make
    (CT : Format_types.CORE_TYPE)
    (Pat : Format_types.PATTERN)
    (Expr : Format_types.EXPR)
    (Attrs : Format_types.ATTRS)
    (ClassT : Format_types.CLASS_TYPE) = struct

  let rec class_field f x =
    let attrs = x.pcf_attributes in
    match x.pcf_desc with
    | Pcf_inherit (ovf, ce, so) ->
        box 2
          (fun f () ->
            Fmt.string f "inherit ";
            Fmt.string f (override ovf);
            Fmt.sp f ();
            class_expr f ce;
            (option
               ~first:(fun f () -> Fmt.string f " as ")
               (fun f s -> Fmt.string f s.txt))
              f so;
            Attrs.item_attributes f attrs)
          f ()
    | Pcf_val (s, mf, Cfk_concrete (ovf, e)) ->
        box 2
          (fun f () ->
            Fmt.string f "val";
            Fmt.string f (override ovf);
            Fmt.sp f ();
            mutable_flag mf f ();
            Fmt.string f s.txt;
            Fmt.string f " =";
            Fmt.sp f ();
            Expr.expression f e;
            Attrs.item_attributes f attrs)
          f ()
    | Pcf_val (s, mf, Cfk_virtual ct) ->
        box 2
          (fun f () ->
            Fmt.string f "val virtual ";
            mutable_flag mf f ();
            Fmt.string f s.txt;
            Fmt.string f " : ";
            CT.core_type f ct;
            Attrs.item_attributes f attrs)
          f ()
    | Pcf_method (s, pf, Cfk_virtual ct) ->
        box 2
          (fun f () ->
            Fmt.string f "method virtual ";
            private_flag pf f ();
            Fmt.string f s.txt;
            Fmt.string f " :";
            Fmt.sp f ();
            CT.core_type f ct;
            Attrs.item_attributes f attrs)
          f ()
    | Pcf_method (s, pf, Cfk_concrete (ovf, e)) ->
        box 2
          (fun f () ->
            Fmt.string f "method";
            Fmt.string f (override ovf);
            Fmt.sp f ();
            private_flag pf f ();
            Fmt.string f s.txt;
            Fmt.string f " =";
            Fmt.sp f ();
            Expr.expression f e;
            Attrs.item_attributes f attrs)
          f ()
    | Pcf_constraint (ct1, ct2) ->
        box 2
          (fun f () ->
            Fmt.string f "constraint ";
            CT.core_type f ct1;
            Fmt.string f " =";
            Fmt.sp f ();
            CT.core_type f ct2;
            Attrs.item_attributes f attrs)
          f ()
    | Pcf_initializer e ->
        box 2
          (fun f () ->
            Fmt.string f "initializer ";
            Expr.expression f e;
            Attrs.item_attributes f attrs)
          f ()
    | Pcf_attribute a -> Attrs.floating_attribute f a
    | Pcf_extension e ->
        Attrs.item_extension f e;
        Attrs.item_attributes f attrs

  and class_structure f { pcstr_self = p; pcstr_fields = l } =
    let self f p =
      match p.ppat_desc with
      | Ppat_any -> ()
      | Ppat_constraint _ ->
          Fmt.sp f ();
          Pat.pattern f p
      | _ ->
          Fmt.sp f ();
          Fmt.string f "(";
          Pat.pattern f p;
          Fmt.string f ")"
    in
    Fmt.hvbox
      (fun f (p, l) ->
        Fmt.hvbox
          (fun f (p, l) ->
            Fmt.string f "object";
            self f p;
            Fmt.sp f ();
            list class_field f l)
          f (p, l);
        Fmt.sp f ();
        Fmt.string f "end")
      f (p, l)

  and class_expr f x =
    if x.pcl_attributes <> [] then (
      Fmt.string f "((";
      class_expr f { x with pcl_attributes = [] };
      Fmt.string f ")";
      Attrs.attributes f x.pcl_attributes;
      Fmt.string f ")")
    else
      match x.pcl_desc with
      | Pcl_structure cs -> class_structure f cs
      | Pcl_fun (l, eo, p, e) ->
          Fmt.string f "(fun";
          Fmt.sp f ();
          Expr.label_exp f (l, eo, p);
          Fmt.sp f ();
          Fmt.string f "->";
          Fmt.sp f ();
          class_expr f e;
          Fmt.string f ")"
      | Pcl_let (rf, l, ce) ->
          Fmt.string f "(";
          Expr.bindings f (rf, l);
          Fmt.sp f ();
          Fmt.string f "in";
          Fmt.sp f ();
          class_expr f ce;
          Fmt.string f ")"
      | Pcl_apply (ce, l) ->
          Fmt.string f "((";
          class_expr f ce;
          Fmt.string f ")";
          Fmt.sp f ();
          list Expr.label_x_expression f l;
          Fmt.string f ")"
      | Pcl_constr (li, l) ->
          (fun f l ->
            if l <> [] then (
              Fmt.string f "[";
              list CT.core_type ~sep:(fun f () -> Fmt.string f ",") f l;
              Fmt.string f "]";
              Fmt.sp f ()))
            f l;
          longident_loc f li
      | Pcl_constraint (ce, ct) ->
          Fmt.string f "(";
          class_expr f ce;
          Fmt.sp f ();
          Fmt.string f ":";
          Fmt.sp f ();
          ClassT.class_type f ct;
          Fmt.string f ")"
      | Pcl_extension e -> Attrs.extension f e
      | Pcl_open (o, e) ->
          box 2
            (fun f (o, e) ->
              Fmt.string f "(let open";
              Fmt.string f (override o.popen_override);
              Fmt.sp f ();
              longident_loc f o.popen_expr;
              Fmt.sp f ();
              Fmt.string f "in";
              Fmt.sp f ();
              class_expr f e;
              Fmt.string f ")")
            f (o, e)
end
