open Astlib.Ast_414
open Asttypes
open Longident
open Parsetree
open Format_utils
open Format_ident
open Format_flags
open Format_views

module Make
    (CT : Format_types.CORE_TYPE)
    (Pat : Format_types.PATTERN)
    (Attrs : Format_types.ATTRS)
    (CE : Format_types.CLASS_EXPR)
    (ME : Format_types.MODULE_EXPR)
    (TD : Format_types.TYPE_DECL) = struct

  let rec expression f x =
    if x.pexp_attributes <> [] then
      (Fmt.using fst expression ++ Fmt.sp ++ Fmt.using snd Attrs.attributes)
        f
        ({ x with pexp_attributes = [] }, x.pexp_attributes)
    else
      match x.pexp_desc with
      | Pexp_ident li -> longident_loc f li
      | Pexp_constant c -> constant f c
      | Pexp_let (rf, l, e) ->
          (box 2
             (parens
                (Fmt.using fst bindings ++ Fmt.sp ++ kwd "in" ++ Fmt.sp
               ++ Fmt.using snd expression)))
            f
            ((rf, l), e)
      | Pexp_function cases ->
          (hbox (parens (kwd "function" ++ case_list))) f cases
      | Pexp_fun (l, e0, p, e) ->
          (box 2
             (parens
                (kwd "fun" ++ Fmt.using fst label_exp ++ op "->"
               ++ Fmt.using snd expression)))
            f
            ((l, e0, p), e)
      | Pexp_apply (e, l) -> print_apply f e l
      | Pexp_match (e, cases) ->
          (Fmt.hvbox
             (block
                (kwd "match" ++ Fmt.using fst expression ++ Fmt.sp ++ kwd "with"
               ++ Fmt.using snd case_list)))
            f (e, cases)
      | Pexp_try (e, cases) ->
          (Fmt.box ~indent:0
             (parens
                (kwd "try" ++ Fmt.using fst expression ++ Fmt.sp ++ kwd "with"
               ++ Fmt.using snd case_list)))
            f (e, cases)
      | Pexp_tuple l ->
          (hvbox (parens (list expression ~sep:(comma ++ Fmt.sp)))) f l
      | Pexp_construct _ when is_simple_construct (view_expr x) -> (
          match view_expr x with
          | `nil -> Fmt.string f "[]"
          | `tuple -> Fmt.string f "()"
          | `list xs ->
              (Fmt.hvbox (brackets (list expression ~sep:(semi ++ Fmt.sp)))) f xs
          | `simple x -> longident f x
          | _ -> assert false)
      | Pexp_construct (li, None) -> longident_loc f li
      | Pexp_construct (li, Some eo) -> (
          match view_expr x with
          | `cons ls -> (parens (list expression ~sep:(Fmt.sp ++ op "::"))) f ls
          | _ ->
              (box 2
                 (parens
                    (Fmt.using fst longident_loc
                    ++ Fmt.sp
                    ++ Fmt.using snd expression_parens)))
                f (li, eo))
      | Pexp_variant (l, None) -> (fstr "`" ++ Fmt.string) f l
      | Pexp_variant (l, Some eo) ->
          (box 2
             (parens
                (fstr "`" ++ Fmt.using fst Fmt.string ++ Fmt.sp
                ++ Fmt.using snd expression_parens)))
            f (l, eo)
      | Pexp_record (l, eo) ->
          let longident_x_expression f (li, e) =
            match e with
            | { pexp_desc = Pexp_ident { txt; _ }; pexp_attributes = [] }
              when li.txt = txt ->
                (hvbox longident_loc) f li
            | _ ->
                (hvbox
                   (Fmt.using fst longident_loc
                   ++ Fmt.sp ++ op "=" ++ Fmt.using snd expression))
                  f (li, e)
          in
          Fmt.hvbox
            (fun f (eo, l) ->
              hvbox
                (fun f () ->
                  Fmt.string f "{";
                  Fmt.sp f ();
                  option ~last:(Fmt.sp ++ kwd "with") expression f eo;
                  list longident_x_expression ~sep:(semi ++ Fmt.sp) f l;
                  Fmt.string f "}")
                f ();
              Fmt.sp f ())
            f (eo, l)
      | Pexp_field (e, li) ->
          (hvbox
             (Fmt.using fst expression_parens
             ++ fstr "."
             ++ Fmt.using snd longident_loc))
            f (e, li)
      | Pexp_setfield (e1, li, e2) ->
          (box 2
             (parens
                (Fmt.using (fun (e1, _, _) -> e1) expression_parens
                ++ fstr "."
                ++ Fmt.using (fun (_, li, _) -> li) longident_loc
                ++ Fmt.sp ++ op "<-"
                ++ Fmt.using (fun (_, _, e2) -> e2) expression)))
            f (e1, li, e2)
      | Pexp_array l ->
          (Fmt.box ~indent:0 (box 2 (bracks (list expression ~sep:semi)))) f l
      | Pexp_ifthenelse (e1, e2, eo) ->
          let else_part f = function
            | Some x ->
                Fmt.sp f ();
                Fmt.string f "else";
                Fmt.sp f ();
                expression f x
            | None -> ()
          in
          block
            (fun f (e1, e2, eo) ->
              Fmt.string f "if";
              Fmt.sp f ();
              expression f e1;
              Fmt.sp f ();
              Fmt.string f "then";
              Fmt.sp f ();
              expression f e2;
              else_part f eo)
            f (e1, e2, eo)
      | Pexp_sequence (e1, e2) ->
          block
            (fun f (e1, e2) ->
              Fmt.sp f ();
              expression f e1;
              semi f ();
              Fmt.sp f ();
              expression f e2;
              Fmt.sp f ())
            f (e1, e2)
      | Pexp_while (e1, e2) ->
          block
            (fun f (e1, e2) ->
              Fmt.string f "(";
              Fmt.string f "while";
              Fmt.sp f ();
              expression f e1;
              Fmt.sp f ();
              Fmt.string f "do";
              Fmt.sp f ();
              expression f e2;
              Fmt.sp f ();
              Fmt.string f "done";
              Fmt.string f ")")
            f (e1, e2)
      | Pexp_for (s, e1, e2, df, e3) ->
          Fmt.hvbox
            (fun f (s, e1, df, e2, e3) ->
              hvbox
                (fun f () ->
                  box 2
                    (fun f () ->
                      Fmt.string f "(";
                      Fmt.string f "for";
                      Fmt.sp f ();
                      Pat.pattern f s;
                      Fmt.sp f ();
                      Fmt.string f "=";
                      Fmt.sp f ();
                      expression f e1;
                      Fmt.sp f ();
                      direction_flag df f ();
                      expression f e2;
                      Fmt.sp f ();
                      Fmt.string f "do")
                    f ();
                  Fmt.sp f ();
                  expression f e3)
                f ();
              Fmt.sp f ();
              Fmt.string f "done";
              Fmt.string f ")")
            f (s, e1, df, e2, e3)
      | Pexp_constraint (e, ct) ->
          (parens
             (Fmt.using fst expression ++ sep " : " ++ Fmt.using snd CT.core_type))
            f (e, ct)
      | Pexp_coerce (e, cto1, ct) ->
          (parens
             (Fmt.using (fun (e, _, _) -> e) expression
             ++ Fmt.using
                  (fun (_, cto1, _) -> cto1)
                  (option CT.core_type ~first:(sep " : ") ~last:Fmt.sp)
             ++ sep " :> "
             ++ Fmt.using (fun (_, _, ct) -> ct) CT.core_type))
            f (e, cto1, ct)
      | Pexp_send (e, s) ->
          (hvbox
             (Fmt.using fst expression_parens
             ++ fstr "#" ++ Fmt.using snd Fmt.string))
            f (e, s.txt)
      | Pexp_new li -> (hvbox (parens (kwd "new" ++ longident_loc))) f li
      | Pexp_setinstvar (s, e) ->
          (hvbox
             (parens
                (Fmt.using fst Fmt.string ++ Fmt.sp ++ op "<-"
               ++ Fmt.using snd expression)))
            f (s.txt, e)
      | Pexp_override l ->
          let string_x_expression f (s, e) =
            (hvbox
               (Fmt.using fst Fmt.string ++ Fmt.sp ++ op "="
              ++ Fmt.using snd expression))
              f (s.txt, e)
          in
          (hvbox (fstr "{<" ++ list string_x_expression ~sep:semi ++ fstr ">}"))
            f l
      | Pexp_letmodule (s, me, e) ->
          (hvbox
             (parens
                (kwd "let" ++ kwd "module"
                ++ Fmt.using (fun (s, _, _) -> s) Fmt.string
                ++ Fmt.sp ++ op "="
                ++ Fmt.using (fun (_, me, _) -> me) ME.module_expr
                ++ Fmt.sp ++ kwd "in"
                ++ Fmt.using (fun (_, _, e) -> e) expression)))
            f
            (Option.value s.txt ~default:"_", me, e)
      | Pexp_letexception (cd, e) ->
          (hvbox
             (parens
                (kwd "let" ++ kwd "exception"
                ++ Fmt.using fst TD.extension_constructor
                ++ Fmt.sp ++ kwd "in" ++ Fmt.using snd expression)))
            f (cd, e)
      | Pexp_assert e -> (hvbox (parens (kwd "assert" ++ expression_parens))) f e
      | Pexp_lazy e -> (hvbox (parens (kwd "lazy" ++ expression_parens))) f e
      | Pexp_object cs -> CE.class_structure f cs
      | Pexp_newtype (lid, e) ->
          (box 2
             (parens
                (kwd "fun"
                ++ Fmt.using fst (parens (kwd "type" ++ Fmt.string))
                ++ Fmt.sp ++ op "->" ++ Fmt.using snd expression)))
            f (lid.txt, e)
      | Pexp_pack me -> (parens (kwd "module" ++ ME.module_expr)) f me
      | Pexp_open (o, e) ->
          (box 2
             (parens
                (kwd "let" ++ fstr "open"
                ++ fstr (override o.popen_override)
                ++ Fmt.sp ++ Fmt.using fst ME.module_expr ++ Fmt.sp ++ kwd "in" ++ Fmt.sp
                ++ Fmt.using snd expression)))
            f (o.popen_expr, e)
      | Pexp_letop { let_; ands; body } ->
          box 2
            (fun f (let_, ands, body) ->
              Fmt.string f "(";
              binding_op f let_;
              Fmt.comma f ();
              list ~sep:Fmt.sp binding_op f ands;
              Fmt.sp f ();
              Fmt.string f "in";
              Fmt.sp f ();
              expression f body;
              Fmt.string f ")")
            f (let_, ands, body)
      | Pexp_extension e -> Attrs.extension f e
      | Pexp_unreachable -> Fmt.string f "."
      | Pexp_poly (e, None) ->
          Fmt.hvbox
            (fun f e ->
              Fmt.string f "(!poly!";
              Fmt.sp f ();
              expression_parens f e;
              Fmt.string f ")")
            f e
      | Pexp_poly (e, Some ct) ->
          Fmt.hvbox
            (fun f (e, ct) ->
              Fmt.string f "(!poly!";
              Fmt.sp f ();
              expression_parens f e;
              Fmt.sp f ();
              Fmt.string f ":";
              Fmt.sp f ();
              CT.core_type f ct;
              Fmt.string f ")")
            f (e, ct)

  and expression_parens f x = (parens_if (needs_parens_expr x) expression) f x

  and print_apply f e l =
    match view_fixity_of_exp e with
    | `Infix s -> (
        match l with
        | [ (Nolabel, arg1); (Nolabel, arg2) ] ->
            box 2
              (fun f (arg1, s, arg2) ->
                Fmt.string f "(";
                expression_parens f arg1;
                space f ();
                Fmt.string f s;
                space f ();
                expression_parens f arg2;
                Fmt.string f ")")
              f (arg1, s, arg2)
        | [ (_, arg1); (_, arg2) ] when List.mem s [ "::"; "+"; "*"; "-" ] ->
            box 2
              (fun f (arg1, s, arg2) ->
                Fmt.string f "(";
                expression_parens f arg1;
                space f ();
                Fmt.string f s;
                space f ();
                expression_parens f arg2;
                Fmt.string f ")")
              f (arg1, s, arg2)
        | _ ->
            box 2
              (fun f (e, l) ->
                Fmt.string f "(";
                expression_parens f e;
                space f ();
                list label_x_expression f l;
                Fmt.string f ")")
              f (e, l))
    | `Prefix s -> (
        let s =
          if List.mem s [ "~+"; "~-"; "~+."; "~-." ] then
            match l with
            | [ (_, { pexp_desc = Pexp_constant _ }) ] -> s
            | _ -> String.sub s 1 (String.length s - 1)
          else s
        in
        match l with
        | [ (Nolabel, x) ] ->
            box 2
              (fun f (s, x) ->
                Fmt.string f "(";
                Fmt.string f s;
                Fmt.sp f ();
                expression_parens f x;
                Fmt.string f ")")
              f (s, x)
        | _ ->
            box 2
              (fun f (e, l) ->
                Fmt.string f "(";
                expression_parens f e;
                Fmt.sp f ();
                list label_x_expression f l;
                Fmt.string f ")")
              f (e, l))
    | _ ->
        Fmt.hvbox
          (fun f (e, l) ->
            Fmt.string f "(";
            expression_parens f e;
            Fmt.sp f ();
            list label_x_expression f l;
            Fmt.string f ")")
          f (e, l)

  and label_exp f (l, opt, p) =
    match l with
    | Nolabel -> (Pat.pattern_parens ++ Fmt.sp) f p
    | Optional rest -> (
        match p with
        | { ppat_desc = Ppat_var { txt; _ }; ppat_attributes = [] }
          when txt = rest -> (
            match opt with
            | Some o ->
                (fstr "?"
                ++ parens
                     (Fmt.using fst Fmt.string ++ op "="
                    ++ Fmt.using snd expression)
                ++ Fmt.sp)
                  f (rest, o)
            | None -> (fstr "?" ++ Fmt.string ++ Fmt.sp) f rest)
        | _ -> (
            match opt with
            | Some o ->
                (fstr "?"
                ++ Fmt.using (fun (r, _, _) -> r) Fmt.string
                ++ sep ":"
                ++ Fmt.using
                     (fun (_, p, o) -> (p, o))
                     (parens
                        (Fmt.using fst Pat.pattern ++ op "="
                       ++ Fmt.using snd expression))
                ++ Fmt.sp)
                  f (rest, p, o)
            | None ->
                (fstr "?" ++ Fmt.using fst Fmt.string ++ sep ":"
                ++ Fmt.using snd Pat.pattern_parens
                ++ Fmt.sp)
                  f (rest, p)))
    | Labelled l -> (
        match p with
        | { ppat_desc = Ppat_var { txt; _ }; ppat_attributes = [] } when txt = l
          ->
            (fstr "~" ++ Fmt.string ++ Fmt.sp) f l
        | _ ->
            (fstr "~" ++ Fmt.using fst Fmt.string ++ sep ":"
            ++ Fmt.using snd Pat.pattern_parens
            ++ Fmt.sp)
              f (l, p))

  and label_x_expression f (l, e) =
    match l with
    | Nolabel -> expression_parens f e
    | Optional str -> (
        match e with
        | { pexp_desc = Pexp_ident { txt = Lident l; _ }; pexp_attributes = [] }
          when str = l ->
            (fstr "?" ++ Fmt.string) f str
        | _ ->
            (fstr "?" ++ Fmt.using fst Fmt.string ++ sep ":"
            ++ Fmt.using snd expression_parens)
              f (str, e))
    | Labelled lbl -> (
        match e with
        | { pexp_desc = Pexp_ident { txt = Lident l; _ }; pexp_attributes = [] }
          when lbl = l ->
            (fstr "~" ++ Fmt.string) f lbl
        | _ ->
            (fstr "~" ++ Fmt.using fst Fmt.string ++ sep ":"
            ++ Fmt.using snd expression_parens)
              f (lbl, e))

  and case_list f l =
    let aux f { pc_lhs; pc_guard; pc_rhs } =
      (Fmt.sp ++ fstr "| "
      ++ box 2
           (Fmt.using (fun (lhs, _, _) -> lhs) Pat.pattern
           ++ Fmt.using
                (fun (_, guard, _) -> guard)
                (option expression ~first:(Fmt.sp ++ kwd "when"))
           ++ Fmt.sp ++ op "->"
           ++ Fmt.using (fun (_, _, rhs) -> rhs) expression))
        f (pc_lhs, pc_guard, pc_rhs)
    in
    list aux f l ~sep:Fmt.nop

  and binding_op f x =
    match (x.pbop_pat, x.pbop_exp) with
    | ( { ppat_desc = Ppat_var { txt = pvar; _ }; ppat_attributes = [] },
        { pexp_desc = Pexp_ident { txt = Lident evar; _ }; pexp_attributes = [] }
      )
      when pvar = evar ->
        (box 2 (Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd Fmt.string))
          f (x.pbop_op.txt, evar)
    | pat, exp ->
        (box 2
           (Fmt.using (fun (op', _, _) -> op') Fmt.string
           ++ Fmt.sp
           ++ Fmt.using (fun (_, pat, _) -> pat) Pat.pattern
           ++ Fmt.sp ++ op "="
           ++ Fmt.using (fun (_, _, exp) -> exp) expression))
          f (x.pbop_op.txt, pat, exp)

  and bindings f (rf, l) =
    let binding kwd rf f x =
      box 2
        (fun f () ->
          Fmt.string f kwd;
          Fmt.sp f ();
          rec_flag rf f ();
          binding_body f x)
        f ();
      Attrs.item_attributes f x.pvb_attributes
    in
    match l with
    | [] -> ()
    | [ x ] -> binding "let" rf f x
    | x :: xs ->
        vbox
          (fun f () ->
            binding "let" rf f x;
            Fmt.sp f ();
            list ~sep:Fmt.sp (binding "and" Nonrecursive) f xs)
          f ()

  and binding_body f { pvb_pat = p; pvb_expr = x; _ } =
    let rec pp_print_pexp_function f x =
      if x.pexp_attributes <> [] then (fstr "=" ++ expression) f x
      else
        match x.pexp_desc with
        | Pexp_fun (label, eo, p, e) ->
            if label = Nolabel then (
              Pat.pattern_parens f p;
              Fmt.sp f ();
              pp_print_pexp_function f e)
            else (
              label_exp f (label, eo, p);
              Fmt.sp f ();
              pp_print_pexp_function f e)
        | Pexp_newtype (str, e) ->
            Fmt.string f "(type";
            Fmt.sp f ();
            Fmt.string f str.txt;
            Fmt.string f ")";
            Fmt.sp f ();
            pp_print_pexp_function f e
        | _ -> (fstr "=" ++ expression) f x
    in
    match p with
    | { ppat_desc = Ppat_constraint (p, ty); ppat_attributes = [] } ->
        Pat.pattern_parens f p;
        Fmt.sp f ();
        Fmt.string f ":";
        Fmt.sp f ();
        CT.core_type f ty;
        Fmt.sp f ();
        Fmt.string f "=";
        Fmt.sp f ();
        expression f x
    | { ppat_desc = Ppat_var _; ppat_attributes = [] } ->
        Pat.pattern_parens f p;
        Fmt.sp f ();
        pp_print_pexp_function f x
    | _ ->
        Pat.pattern f p;
        Fmt.sp f ();
        Fmt.string f "=";
        Fmt.sp f ();
        expression f x
end
