open Astlib.Ast_414
open Asttypes
open Location
open Parsetree
open Format_utils
open Format_ident
open Format_flags

module Make
    (CT : Format_types.CORE_TYPE)
    (Attrs : Format_types.ATTRS) = struct

  let rec class_params_def f = function
    | [] -> ()
    | l -> (brackets (list type_param ~sep:comma) ++ Fmt.sp) f l

  and type_param f (ct, (a, b)) =
    (fstr (type_variance a) ++ fstr (type_injectivity b) ++ CT.core_type) f ct

  and class_type_field f x =
    match x.pctf_desc with
    | Pctf_inherit ct ->
        (box 2 (kwd "inherit" ++ Fmt.using fst class_type)
        ++ Fmt.using snd Attrs.item_attributes)
          f (ct, x.pctf_attributes)
    | Pctf_val (s, mf, vf, ct) ->
        box 2
          (fun f () ->
            Fmt.string f "val ";
            mutable_flag mf f ();
            virtual_flag vf f ();
            Fmt.string f s.txt;
            Fmt.string f " : ";
            CT.core_type f ct;
            Attrs.item_attributes f x.pctf_attributes)
          f ()
    | Pctf_method (s, pf, vf, ct) ->
        box 2
          (fun f () ->
            Fmt.string f "method ";
            private_flag pf f ();
            virtual_flag vf f ();
            Fmt.string f s.txt;
            Fmt.string f " :";
            Fmt.sp f ();
            CT.core_type f ct;
            Attrs.item_attributes f x.pctf_attributes)
          f ()
    | Pctf_constraint (ct1, ct2) ->
        box 2
          (fun f () ->
            Fmt.string f "constraint ";
            CT.core_type f ct1;
            Fmt.string f " = ";
            CT.core_type f ct2;
            Attrs.item_attributes f x.pctf_attributes)
          f ()
    | Pctf_attribute a -> Attrs.floating_attribute f a
    | Pctf_extension e ->
        Attrs.item_extension f e;
        Attrs.item_attributes f x.pctf_attributes

  and class_signature f { pcsig_self = ct; pcsig_fields = l; _ } =
    let self f = function
      | { ptyp_desc = Ptyp_any; ptyp_attributes = [] } -> ()
      | ct ->
          Fmt.sp f ();
          Fmt.string f "(";
          CT.core_type f ct;
          Fmt.string f ")"
    in
    Fmt.hvbox
      (fun f (ct, l) ->
        Fmt.hvbox
          (fun f (ct, l) ->
            Fmt.string f "object";
            Fmt.hbox (fun f ct -> self f ct) f ct;
            Fmt.sp f ();
            list class_type_field ~sep:(fun f () -> Fmt.sp f ()) f l)
          f (ct, l);
        Fmt.sp f ();
        Fmt.string f "end")
      f (ct, l)

  and class_type f x =
    match x.pcty_desc with
    | Pcty_signature cs ->
        class_signature f cs;
        Attrs.attributes f x.pcty_attributes
    | Pcty_constr (li, l) ->
        (fun f l ->
          match l with
          | [] -> ()
          | _ ->
              Fmt.string f "[";
              list CT.core_type ~sep:(fun f () -> Fmt.string f ",") f l;
              Fmt.string f "]";
              Fmt.sp f ())
          f l;
        longident_loc f li;
        Attrs.attributes f x.pcty_attributes
    | Pcty_arrow (l, co, cl) ->
        box 2
          (fun f ((l, co), cl) ->
            CT.type_with_label f (l, co);
            Fmt.sp f ();
            Fmt.string f "->";
            Fmt.sp f ();
            class_type f cl)
          f
          ((l, co), cl)
    | Pcty_extension e ->
        Attrs.extension f e;
        Attrs.attributes f x.pcty_attributes
    | Pcty_open (o, e) ->
        box 2
          (fun f (o, e) ->
            Fmt.string f "let open";
            Fmt.string f (override o.popen_override);
            Fmt.sp f ();
            longident_loc f o.popen_expr;
            Fmt.sp f ();
            Fmt.string f "in";
            Fmt.sp f ();
            class_type f e)
          f (o, e)

  and class_type_declaration_list f l =
    let ctd kwd f x =
      box 2
        (fun f () ->
          Fmt.string f kwd;
          Fmt.sp f ();
          virtual_flag x.pci_virt f ();
          class_params_def f x.pci_params;
          Fmt.string f x.pci_name.txt;
          Fmt.string f " = ";
          class_type f x.pci_expr;
          Attrs.item_attributes f x.pci_attributes)
        f ()
    in
    match l with
    | [] -> ()
    | x :: xs ->
        Fmt.vbox
          (fun f (x, xs) ->
            ctd "class type" f x;
            Fmt.comma f ();
            list ~sep:(fun f () -> Fmt.comma f ()) (ctd "and") f xs)
          f (x, xs)
end
