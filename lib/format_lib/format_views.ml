(* Format_views: View functions and needs-parens helpers *)

open Astlib.Ast_414
open Parsetree
open Longident
open Format_ident

let view_fixity_of_exp = function
  | { pexp_desc = Pexp_ident { txt = Lident l; _ }; pexp_attributes = [] } ->
      fixity_of_string l
  | _ -> `Normal

let view_expr x =
  match x.pexp_desc with
  | Pexp_construct ({ txt = Lident "()"; _ }, _) -> `tuple
  | Pexp_construct ({ txt = Lident "[]"; _ }, _) -> `nil
  | Pexp_construct ({ txt = Lident "::"; _ }, Some _) ->
      let rec loop exp acc =
        match exp with
        | {
         pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, _);
         pexp_attributes = [];
        } ->
            (List.rev acc, true)
        | {
         pexp_desc =
           Pexp_construct
             ( { txt = Lident "::"; _ },
               Some { pexp_desc = Pexp_tuple [ e1; e2 ]; pexp_attributes = [] }
             );
         pexp_attributes = [];
        } ->
            loop e2 (e1 :: acc)
        | e -> (List.rev (e :: acc), false)
      in
      let ls, b = loop x [] in
      if b then `list ls else `cons ls
  | Pexp_construct (x, None) -> `simple x.txt
  | _ -> `normal

let is_simple_construct = function
  | `nil | `tuple | `list _ | `simple _ -> true
  | _ -> false

(* ========== Needs-Parens Helpers ========== *)

let needs_parens_expr e =
  match e.pexp_desc with
  | Pexp_ident _ | Pexp_constant _
  | Pexp_construct (_, None)
  | Pexp_variant (_, None)
  | Pexp_tuple _ | Pexp_record _ | Pexp_array _ | Pexp_field _ | Pexp_send _ ->
      false
  | Pexp_construct _ -> not (is_simple_construct (view_expr e))
  | _ -> true

let needs_parens_pat p =
  match p.ppat_desc with
  | Ppat_var _ | Ppat_any | Ppat_constant _
  | Ppat_construct (_, None)
  | Ppat_variant (_, None)
  | Ppat_tuple _ | Ppat_record _ | Ppat_array _ ->
      false
  | _ -> true

let needs_parens_type t =
  match t.ptyp_desc with
  | Ptyp_var _ | Ptyp_any
  | Ptyp_constr (_, [])
  | Ptyp_tuple _ | Ptyp_object _ | Ptyp_class _ ->
      false
  | _ -> true
