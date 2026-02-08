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
    (MT : Format_types.MODULE_TYPE)
    (ME : Format_types.MODULE_EXPR)
    (TD : Format_types.TYPE_DECL) = struct

  let rec attributes f l = List.iter (attribute f) l

  and item_attributes f l = List.iter (item_attribute f) l

  and attribute f a =
    (box 2
       (fstr "[@" ++ Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd payload
      ++ fstr "]"))
      f
      (a.attr_name.txt, a.attr_payload)

  and item_attribute f a =
    (box 2
       (fstr "[@@" ++ Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd payload
      ++ fstr "]"))
      f
      (a.attr_name.txt, a.attr_payload)

  and floating_attribute f a =
    (box 2
       (fstr "[@@@" ++ Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd payload
      ++ fstr "]"))
      f
      (a.attr_name.txt, a.attr_payload)

  and extension f (s, e) =
    (box 2
       (fstr "[%" ++ Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd payload
      ++ fstr "]"))
      f (s.txt, e)

  and item_extension f (s, e) =
    (box 2
       (fstr "[%%%" ++ Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd payload
      ++ fstr "]"))
      f (s.txt, e)

  and payload f = function
    | PStr [ { pstr_desc = Pstr_eval (e, attrs) } ] ->
        (box 2 (Fmt.using fst Expr.expression ++ Fmt.using snd item_attributes))
          f (e, attrs)
    | PStr x -> ME.structure f x
    | PTyp x -> (fstr ":" ++ Fmt.sp ++ CT.core_type) f x
    | PSig x -> (fstr ":" ++ Fmt.sp ++ MT.signature) f x
    | PPat (x, None) -> (fstr "?" ++ Fmt.sp ++ Pat.pattern) f x
    | PPat (x, Some e) ->
        (fstr "?" ++ Fmt.sp ++ Fmt.using fst Pat.pattern ++ kwd "when"
       ++ Fmt.using snd Expr.expression)
          f (x, e)

  and value_description f x =
    if x.pval_prim <> [] then
      (Fmt.sp ++ op "=" ++ list ~sep:Fmt.sp (fun f s -> escaped_string f s))
        f x.pval_prim

  and exception_declaration f x =
    (hvbox (kwd "exception" ++ Fmt.using fst TD.extension_constructor)
    ++ Fmt.using snd item_attributes)
      f
      (x.ptyexn_constructor, x.ptyexn_attributes)
end
