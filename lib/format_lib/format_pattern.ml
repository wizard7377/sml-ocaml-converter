open Astlib.Ast_414
open Location
open Parsetree
open Format_utils
open Format_ident
open Format_flags
open Format_views

module Make
    (CT : Format_types.CORE_TYPE)
    (Attrs : Format_types.ATTRS) = struct

  let rec pattern f x =
    if x.ppat_attributes <> [] then
      (parens (Fmt.using fst (parens pattern) ++ Fmt.using snd Attrs.attributes))
        f
        ({ x with ppat_attributes = [] }, x.ppat_attributes)
    else
      match x.ppat_desc with
      | Ppat_any -> Fmt.string f "_"
      | Ppat_var { txt; _ } -> protect_ident txt f txt
      | Ppat_constant c -> constant f c
      | Ppat_interval (c1, c2) ->
          (Fmt.using fst constant ++ Fmt.using snd constant) f (c1, c2)
      | Ppat_tuple l -> parens (list ~sep:comma pattern) f l
      | Ppat_construct ({ txt = Lident (("()" | "[]") as x); _ }, None) ->
          Fmt.string f x
      | Ppat_construct (li, None) -> longident_loc f li
      | Ppat_construct
          ( { txt = Lident "::"; _ },
            Some ([], { ppat_desc = Ppat_tuple [ p1; p2 ]; _ }) ) ->
          (parens (Fmt.using fst pattern ++ op "::" ++ Fmt.using snd pattern))
            f (p1, p2)
      | Ppat_construct (li, Some ([], p)) ->
          (parens
             (Fmt.using fst longident_loc
             ++ Fmt.sp
             ++ Fmt.using snd pattern_parens))
            f (li, p)
      | Ppat_construct (li, Some (vl, p)) ->
          (parens
             (Fmt.using (fun (li, _, _) -> li) longident_loc
             ++ Fmt.sp
             ++ Fmt.using
                  (fun (_, vl, _) -> vl)
                  (parens
                     (kwd "type"
                     ++ list ~sep:Fmt.sp (fun f x -> Fmt.string f x.Location.txt)))
             ++ Fmt.sp
             ++ Fmt.using (fun (_, _, p) -> p) pattern_parens))
            f (li, vl, p)
      | Ppat_variant (l, None) -> (fstr "`" ++ Fmt.string) f l
      | Ppat_variant (l, Some p) ->
          (box 2
             (parens
                (fstr "`" ++ Fmt.using fst Fmt.string ++ Fmt.sp
                ++ Fmt.using snd pattern_parens)))
            f (l, p)
      | Ppat_record (l, closed) ->
          let field f (li, p) =
            match (li, p) with
            | ( { txt = Longident.Lident s; _ },
                { ppat_desc = Ppat_var { txt; _ }; ppat_attributes = []; _ } )
              when s = txt ->
                longident_loc f li
            | _ ->
                (box 2
                   (Fmt.using fst longident_loc
                   ++ Fmt.sp ++ op "=" ++ Fmt.using snd pattern))
                  f (li, p)
          in
          let suffix = if closed = Asttypes.Closed then "" else ";_" in
          (box 2
             (braces (Fmt.sp ++ list field ~sep:(semi ++ Fmt.sp) ++ fstr suffix)))
            f l
      | Ppat_array l -> (box 2 (bracks (list pattern ~sep:semi))) f l
      | Ppat_or (p1, p2) ->
          (Fmt.hvbox
             (parens
                (Fmt.using fst pattern ++ Fmt.sp ++ fstr "| "
               ++ Fmt.using snd pattern)))
            f (p1, p2)
      | Ppat_constraint (p, ct) ->
          (box 2
             (parens
                (Fmt.using fst pattern ++ Fmt.sp ++ sep ":" ++ Fmt.sp
               ++ Fmt.using snd CT.core_type)))
            f (p, ct)
      | Ppat_type li -> (fstr "#" ++ longident_loc) f li
      | Ppat_lazy p -> (box 2 (parens (kwd "lazy" ++ pattern_parens))) f p
      | Ppat_unpack { txt } ->
          (parens (kwd "module" ++ Fmt.string)) f (opt_val txt ~default:"_")
      | Ppat_exception p -> (box 2 (parens (kwd "exception" ++ pattern))) f p
      | Ppat_extension e -> Attrs.extension f e
      | Ppat_open (lid, p) ->
          (box 2
             (Fmt.using fst longident_loc
             ++ fstr "."
             ++ Fmt.using snd (parens pattern)))
            f (lid, p)
      | Ppat_alias (p, s) ->
          box 2
            (fun f p ->
              Fmt.string f "(";
              pattern f p;
              Fmt.sp f ();
              Fmt.string f "as";
              Fmt.sp f ();
              protect_ident s.txt f s.txt;
              Fmt.string f ")")
            f p

  and pattern_parens f x = (parens_if (needs_parens_pat x) pattern) f x
end
