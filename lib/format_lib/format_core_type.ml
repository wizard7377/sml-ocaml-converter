open Astlib.Ast_414
open Asttypes
open Parsetree
open Format_utils
open Format_ident
open Format_flags
open Format_views

module Make (Attrs : Format_types.ATTRS) = struct
  let rec flat_arrow (typ : core_type) : (core_type * arg_label) list * core_type =
    match typ.ptyp_desc with
    | Ptyp_arrow (l, ct1, ct2) ->
        let (args, ret) = flat_arrow ct2 in
        ((ct1, l) :: args, ret)
    | _ -> ([], typ)

  let rec core_type f x =
    if x.ptyp_attributes <> [] then
      (parens (Fmt.using fst (parens core_type) ++ Fmt.using snd Attrs.attributes))
        f
        ({ x with ptyp_attributes = [] }, x.ptyp_attributes)
    else
      match x.ptyp_desc with
      | Ptyp_any -> Fmt.string f "_"
      | Ptyp_var s -> tyvar f s
      | (Ptyp_arrow (_, _, _))  ->
          let (args, ret) = flat_arrow x in
          assert (args <> []);
          let tys = List.map (fun (ct, l) f x -> type_with_label f (l, ct)) args in
          let ret' = Fmt.const core_type ret in
          Fmt.parens (Fmt.concat ~sep:(op "->") (tys @ [ret'])) f ()
      | Ptyp_tuple l -> (list core_type ~sep:(Fmt.sp ++ op "*")) f l
      | Ptyp_constr (li, l) -> (
          match l with
          | [] -> longident_loc f li
          | [ x ] ->
              (Fmt.using fst core_type_parens ++ sp ++ Fmt.using snd longident_loc)
                f (x, li)
          | _ ->
              (Fmt.using fst (parens (list core_type ~sep:(comma ++ sp)))
              ++ sp
              ++ Fmt.using snd longident_loc)
                f (l, li))
      | Ptyp_object (l, o) ->
          let core_field_type f x =
            match x.pof_desc with
            | Otag (l, ct) ->
                (hvbox
                   (Fmt.using (fun (s, _, _) -> s) Fmt.string
                   ++ sep ": "
                   ++ (Fmt.parens @@ Fmt.using (fun (_, ct, _) -> ct) core_type)
                   ++ Fmt.sp
                   ++ Fmt.using (fun (_, _, attrs) -> attrs) Attrs.attributes))
                  f
                  (l.txt, ct, x.pof_attributes)
            | Oinherit ct -> hvbox core_type f ct
          in
          let field_var f = function
            | Closed -> ()
            | Open -> (
                match l with [] -> Fmt.string f ".." | _ -> Fmt.string f " ;..")
          in
          hvbox
            (fun f l ->
              Fmt.string f "<";
              Fmt.sp f ();
              list core_field_type ~sep:semi f l;
              field_var f o;
              Fmt.sp f ();
              Fmt.string f ">")
            f l
      | Ptyp_class (li, l) ->
          (hvbox
             (Fmt.using fst (parens (list core_type ~sep:comma))
             ++ fstr "#"
             ++ Fmt.using snd longident_loc))
            f (l, li)
      | Ptyp_alias (ct, s) ->
          (box 2
             (Fmt.using fst core_type_parens
             ++ Fmt.sp ++ kwd "as" ++ Fmt.using snd tyvar))
            f (ct, s)
      | Ptyp_poly ([], ct) -> core_type f ct
      | Ptyp_poly (sl, ct) ->
          (box 2
             (Fmt.using fst (list tyvar_loc ~sep:Fmt.sp)
             ++ fstr "." ++ Fmt.sp ++ Fmt.using snd core_type))
            f (sl, ct)
      | Ptyp_package (lid, cstrs) -> (
          let aux f (s, ct) =
            (kwd "type"
            ++ Fmt.using fst longident_loc
            ++ Fmt.sp ++ op "=" ++ Fmt.using snd core_type)
              f (s, ct)
          in
          match cstrs with
          | [] -> (hvbox (parens (kwd "module" ++ longident_loc))) f lid
          | _ ->
              (hvbox
                 (parens
                    (kwd "module"
                    ++ Fmt.using fst longident_loc
                    ++ Fmt.sp ++ kwd "with"
                    ++ Fmt.using snd (list aux ~sep:(Fmt.sp ++ kwd "and")))))
                f (lid, cstrs))
      | Ptyp_extension e -> Attrs.extension f e
      | Ptyp_variant (l, closed, low) ->
          let row_field f x =
            match x.prf_desc with
            | Rtag (lbl, _, ctl) ->
                box 2
                  (fun f (txt, ctl, attrs) ->
                    Fmt.string f "`";
                    Fmt.string f txt;
                    (match ctl with
                    | [] -> ()
                    | l ->
                        Fmt.sp f ();
                        Fmt.string f "of";
                        Fmt.sp f ();
                        list core_type ~sep:(fstr "&") f l);
                    Fmt.sp f ();
                    Attrs.attributes f attrs)
                  f
                  (lbl.txt, ctl, x.prf_attributes)
            | Rinherit ct -> core_type f ct
          in
          let prefix =
            match (l, closed, low) with
            | [], Open, _ -> ">"
            | [], Closed, _ -> ""
            | _, Open, _ -> ">"
            | _, Closed, Some _ -> "<"
            | { prf_desc = Rinherit _ } :: _, Closed, None -> " |"
            | _, Closed, None -> ""
          in
          let low_formatter f = function
            | Some (_ :: _ as xs) ->
                (fstr ">" ++ Fmt.sp
                ++ list (fun f x -> (fstr "`" ++ Fmt.string) f x))
                  f xs
            | _ -> ()
          in
          box 2
            (fun f l ->
              Fmt.string f "[";
              Fmt.string f prefix;
              Fmt.sp f ();
              alts row_field f l;
              low_formatter f low;
              Fmt.string f "]")
            f l

  and core_type_parens f x = (parens_if (needs_parens_type x) core_type) f x

  and type_with_label f (label, c) : unit =
    match label with
    | Nolabel -> core_type f c
    | Labelled s ->
        (Fmt.using fst Fmt.string ++ sep ":" ++ Fmt.using snd core_type_parens)
          f (s, c)
    | Optional s ->
        (fstr "?" ++ Fmt.using fst Fmt.string ++ sep ":"
        ++ Fmt.using snd core_type_parens)
          f (s, c)
end
