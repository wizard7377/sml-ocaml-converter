(* OCaml Parsetree pretty-printer - Simplified version with liberal parenthesization *)

open Astlib.Ast_414
open Asttypes
open Location
open Longident
open Parsetree
type space_formatter = (unit, Stdlib.Format.formatter, unit) format
type 'a t = 'a Fmt.t
let (++) = Fmt.(++)
(* ========== Utility Functions ========== *)

let opt_val t ~default = match t with None -> default | Some x -> x

let list ?(sep = Fmt.cut) ?(first = Fmt.nop) ?(last = Fmt.nop) fu f = function
  | [] -> ()
  | [ x ] -> fu f x
  | x :: xs ->
      first f ();
      fu f x;
      List.iter (fun x -> sep f (); fu f x) xs;
      last f ()

let option ?(first = Fmt.nop) ?(last = Fmt.nop) fu f = function
  | None -> ()
  | Some x -> first f (); fu f x; last f ()

let paren b fu f x = if b then Fmt.pf f "(%a)" fu x else fu f x
let fstr (s : string) : 'a Fmt.t = Fmt.const Fmt.string s
(* ========== Combinator Helpers ========== *)

(* Wrapping combinators *)
let parens fmt = Fmt.parens fmt
let braces fmt = fstr "{" ++ fmt ++ fstr "}"
let brackets fmt = fstr "[" ++ fmt ++ fstr "]"
let bracks fmt = fstr "[|" ++ fmt ++ fstr "|]"
let angles fmt = fstr "<" ++ fmt ++ fstr ">"

(* Conditional wrapping *)
let parens_if cond fmt = if cond then parens fmt else fmt

(* Keyword and operator helpers *)
let kwd s = fstr (s ^ " ")
let op s = fstr (" " ^ s ^ " ")
let sep s = fstr s

(* Common separators *)
let comma = Fmt.comma
let semi = fstr ";"
let space = Fmt.sp
let cut = Fmt.cut

(* Boxing helpers *)
let box n fmt = Fmt.box ~indent:n fmt
let hbox fmt = Fmt.hbox fmt
let vbox fmt = Fmt.vbox fmt
let hvbox fmt = Fmt.hvbox fmt

(* ========== Identifier Handling ========== *)

let infix_symbols = [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/'; '$'; '%'; '#' ]
let special_infix = [ "asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "::" ]
let first_is c s = s <> "" && s.[0] = c
let first_in cs s = s <> "" && List.mem s.[0] cs
let is_op s n c = String.length s > 3 && s.[0] = c && s.[1] = n.[0] && s.[2] = n.[1] && List.mem s.[3] infix_symbols

let fixity_of_string = function
  | "" -> `Normal
  | s when List.mem s special_infix -> `Infix s
  | s when first_in infix_symbols s -> `Infix s
  | s when first_in [ '!'; '?'; '~' ] s -> `Prefix s
  | s when s.[0] = '.' -> `Mixfix s
  | s when is_op s "et" 'l' -> `Letop s
  | s when is_op s "nd" 'a' -> `Andop s
  | _ -> `Normal

let needs_parens txt = match fixity_of_string txt with `Infix _ | `Mixfix _ | `Letop _ | `Andop _ -> true | `Prefix _ -> true | `Normal -> false
let needs_spaces txt = first_is '*' txt || (txt <> "" && txt.[String.length txt - 1] = '*')

let protect_ident txt =
  if not (needs_parens txt) then Fmt.string
  else if needs_spaces txt then
    parens (Fmt.cut ++ Fmt.string ++ Fmt.cut)
  else
    parens Fmt.string

let rec longident f = function
  | Lident s -> protect_ident s f s
  | Ldot (y, s) ->
      longident f y;
      if not (needs_parens s) then
        (fstr "." ++ Fmt.string) f s
      else if needs_spaces s then
        (fstr "." ++ parens (Fmt.cut ++ Fmt.string ++ Fmt.cut)) f s
      else
        (fstr "." ++ parens Fmt.string) f s
  | Lapply (y, s) ->
      longident f y;
      parens longident f s

let longident_loc f x = longident f x.txt

(* ========== Flags and Simple Printers ========== *)

let override = function Override -> "!" | Fresh -> ""
let type_variance = function NoVariance -> "" | Covariant -> "+" | Contravariant -> "-"
let type_injectivity = function NoInjectivity -> "" | Injective -> "!"

let mutable_flag = function
  | Immutable -> Fmt.nop
  | Mutable -> fstr "mutable" ++ space ++ Fmt.cut

let virtual_flag = function
  | Concrete -> Fmt.nop
  | Virtual -> fstr "virtual" ++ space ++ Fmt.cut

let rec_flag = function
  | Nonrecursive -> Fmt.nop
  | Recursive -> kwd "rec"

let nonrec_flag = function
  | Nonrecursive -> kwd "nonrec"
  | Recursive -> Fmt.nop

let direction_flag = function
  | Upto -> kwd "to"
  | Downto -> kwd "downto"

let private_flag = function
  | Public -> Fmt.nop
  | Private -> kwd "private"

let constant f = function
  | Pconst_char i -> Fmt.pf f "%C" i  (* Keep format string for proper escaping *)
  | Pconst_string (i, _, None) -> Fmt.pf f "%S" i  (* Keep format string for proper escaping *)
  | Pconst_string (i, _, Some delim) ->
      (fstr "{" ++ Fmt.using (fun (d, _, _) -> d) Fmt.string ++
       fstr "|" ++ Fmt.using (fun (_, i, _) -> i) Fmt.string ++
       fstr "|" ++ Fmt.using (fun (d, _, _) -> d) Fmt.string ++
       fstr "}") f (delim, i, delim)
  | Pconst_integer (i, None) | Pconst_float (i, None) ->
      (parens_if (first_is '-' i) Fmt.string) f i
  | Pconst_integer (i, Some m) | Pconst_float (i, Some m) ->
      (parens_if (first_is '-' i) (Fmt.using fst Fmt.string ++ Fmt.using snd Fmt.char)) f (i, m)

let tyvar f s =
  let fmt = if String.length s >= 2 && s.[1] = '\'' then
    fstr "' " ++ Fmt.string
  else
    fstr "'" ++ Fmt.string
  in
  fmt f s

let tyvar_loc f str = tyvar f str.txt

(* ========== View Functions for Pattern Matching ========== *)

let view_fixity_of_exp = function
  | { pexp_desc = Pexp_ident { txt = Lident l; _ }; pexp_attributes = [] } -> fixity_of_string l
  | _ -> `Normal

let view_expr x =
  match x.pexp_desc with
  | Pexp_construct ({ txt = Lident "()"; _ }, _) -> `tuple
  | Pexp_construct ({ txt = Lident "[]"; _ }, _) -> `nil
  | Pexp_construct ({ txt = Lident "::"; _ }, Some _) ->
      let rec loop exp acc = match exp with
        | { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, _); pexp_attributes = [] } -> (List.rev acc, true)
        | { pexp_desc = Pexp_construct ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple [ e1; e2 ]; pexp_attributes = [] }); pexp_attributes = [] } -> loop e2 (e1 :: acc)
        | e -> (List.rev (e :: acc), false)
      in
      let ls, b = loop x [] in if b then `list ls else `cons ls
  | Pexp_construct (x, None) -> `simple x.txt
  | _ -> `normal

let is_simple_construct = function `nil | `tuple | `list _ | `simple _ -> true | _ -> false

(* ========== Needs-Parens Helpers ========== *)

let needs_parens_expr e = match e.pexp_desc with
  | Pexp_ident _ | Pexp_constant _ | Pexp_construct (_, None) | Pexp_variant (_, None)
  | Pexp_tuple _ | Pexp_record _ | Pexp_array _ | Pexp_field _ | Pexp_send _ -> false
  | Pexp_construct _ -> not (is_simple_construct (view_expr e))
  | _ -> true

let needs_parens_pat p = match p.ppat_desc with
  | Ppat_var _ | Ppat_any | Ppat_constant _ | Ppat_construct (_, None) | Ppat_variant (_, None)
  | Ppat_tuple _ | Ppat_record _ | Ppat_array _ -> false
  | _ -> true

let needs_parens_type t = match t.ptyp_desc with
  | Ptyp_var _ | Ptyp_any | Ptyp_constr (_, []) | Ptyp_tuple _ | Ptyp_object _ | Ptyp_class _ -> false
  | _ -> true

(* ========== Main Recursive Printers ========== *)

let rec core_type f x =
  if x.ptyp_attributes <> [] then
    (parens (Fmt.using fst (parens core_type) ++ Fmt.using snd attributes))
    f ({ x with ptyp_attributes = [] }, x.ptyp_attributes)
  else
    match x.ptyp_desc with
    | Ptyp_any -> Fmt.string f "_"
    | Ptyp_var s -> tyvar f s
    | Ptyp_arrow (l, ct1, ct2) ->
        (box 2 (Fmt.using fst type_with_label ++ Fmt.cut ++ op "->" ++ Fmt.using snd core_type))
        f ((l, ct1), ct2)
    | Ptyp_tuple l ->
        parens (list core_type ~sep:(Fmt.cut ++ op "*")) f l
    | Ptyp_constr (li, l) ->
        (match l with
        | [] -> longident_loc f li
        | [ x ] ->
            (Fmt.using fst core_type_parens ++ Fmt.cut ++ Fmt.using snd longident_loc) f (x, li)
        | _ ->
            (Fmt.using fst (parens (list core_type ~sep:(comma ++ Fmt.cut))) ++ Fmt.cut ++ Fmt.using snd longident_loc) f (l, li))
    | Ptyp_object (l, o) ->
        let core_field_type f x =
          match x.pof_desc with
          | Otag (l, ct) ->
              (hvbox (Fmt.using (fun (s, _, _) -> s) Fmt.string ++ sep ": " ++
                      Fmt.using (fun (_, ct, _) -> ct) core_type ++ Fmt.sp ++
                      Fmt.using (fun (_, _, attrs) -> attrs) attributes))
              f (l.txt, ct, x.pof_attributes)
          | Oinherit ct -> hvbox core_type f ct
        in
        let field_var f = function
          | Closed -> ()
          | Open -> (match l with [] -> Fmt.string f ".." | _ -> Fmt.string f " ;..")
        in
        hvbox (fun f l ->
          Fmt.string f "<";
          Fmt.sp f ();
          list core_field_type ~sep:semi f l;
          field_var f o;
          Fmt.sp f ();
          Fmt.string f ">"
        ) f l
    | Ptyp_class (li, l) ->
        (hvbox (Fmt.using fst (parens (list core_type ~sep:comma)) ++
                fstr "#" ++
                Fmt.using snd longident_loc))
        f (l, li)
    | Ptyp_alias (ct, s) ->
        (box 2 (Fmt.using fst core_type_parens ++ Fmt.cut ++ kwd "as" ++ Fmt.using snd tyvar)) f (ct, s)
    | Ptyp_poly ([], ct) -> core_type f ct
    | Ptyp_poly (sl, ct) ->
        (box 2 (Fmt.using fst (list tyvar_loc ~sep:Fmt.cut) ++ fstr "." ++ Fmt.cut ++ Fmt.using snd core_type))
        f (sl, ct)
    | Ptyp_package (lid, cstrs) ->
        let aux f (s, ct) =
          (kwd "type" ++ Fmt.using fst longident_loc ++ Fmt.sp ++ op "=" ++ Fmt.using snd core_type) f (s, ct)
        in
        (match cstrs with
        | [] ->
            (hvbox (parens (kwd "module" ++ longident_loc))) f lid
        | _ ->
            (hvbox (parens (kwd "module" ++ Fmt.using fst longident_loc ++ Fmt.sp ++ kwd "with" ++
                            Fmt.using snd (list aux ~sep:(Fmt.sp ++ kwd "and"))))) f (lid, cstrs))
    | Ptyp_extension e -> extension f e
    | Ptyp_variant (l, closed, low) ->
        let row_field f x = match x.prf_desc with
          | Rtag (lbl, _, ctl) ->
              box 2 (fun f (txt, ctl, attrs) ->
                Fmt.string f "`";
                Fmt.string f txt;
                (match ctl with
                | [] -> ()
                | l ->
                    Fmt.cut f ();
                    Fmt.string f "of";
                    Fmt.sp f ();
                    list core_type ~sep:(fstr "&") f l);
                Fmt.cut f ();
                attributes f attrs
              ) f (lbl.txt, ctl, x.prf_attributes)
          | Rinherit ct -> core_type f ct
        in
        let prefix = match l, closed, low with
          | [], Open, _ -> ">" | [], Closed, _ -> ""
          | _, Open, _ -> ">" | _, Closed, Some _ -> "<"
          | { prf_desc = Rinherit _ } :: _, Closed, None -> " |" | _, Closed, None -> ""
        in
        let low_formatter f = function
          | Some (_ :: _ as xs) ->
              (fstr ">" ++ Fmt.sp ++
               list (fun f x -> (fstr "`" ++ Fmt.string) f x)) f xs
          | _ -> ()
        in
        box 2 (fun f l ->
          Fmt.string f "[";
          Fmt.string f prefix;
          Fmt.cut f ();
          list row_field ~sep:(Fmt.any "@;<1 -2>| ") f l;
          low_formatter f low;
          Fmt.string f "]"
        ) f l

and core_type_parens f x =
  (parens_if (needs_parens_type x) core_type) f x

and type_with_label f (label, c) =
  match label with
  | Nolabel -> core_type_parens f c
  | Labelled s -> (Fmt.using fst Fmt.string ++ sep ":" ++ Fmt.using snd core_type_parens) f (s, c)
  | Optional s -> (fstr "?" ++ Fmt.using fst Fmt.string ++ sep ":" ++ Fmt.using snd core_type_parens) f (s, c)

and pattern f x =
  if x.ppat_attributes <> [] then
    (parens (Fmt.using fst (parens pattern) ++ Fmt.using snd attributes))
    f ({ x with ppat_attributes = [] }, x.ppat_attributes)
  else match x.ppat_desc with
    | Ppat_any -> Fmt.string f "_"
    | Ppat_var { txt; _ } -> protect_ident txt f txt
    | Ppat_constant c -> constant f c
    | Ppat_interval (c1, c2) -> ((Fmt.using fst constant) ++ (Fmt.using snd constant)) f (c1, c2)
    | Ppat_tuple l -> parens (list ~sep:comma pattern) f l
    | Ppat_construct ({ txt = Lident (("()" | "[]") as x); _ }, None) -> Fmt.string f x
    | Ppat_construct (li, None) -> longident_loc f li
    | Ppat_construct ({ txt = Lident "::"; _ }, Some ([], { ppat_desc = Ppat_tuple [ p1; p2 ]; _ })) ->
        (parens (Fmt.using fst pattern ++ op "::" ++ Fmt.using snd pattern)) f (p1, p2)
    | Ppat_construct (li, Some ([], p)) ->
        (parens (Fmt.using fst longident_loc ++ Fmt.cut ++ Fmt.using snd pattern_parens)) f (li, p)
    | Ppat_construct (li, Some (vl, p)) ->
        (parens (Fmt.using (fun (li, _, _) -> li) longident_loc ++ Fmt.sp ++
                 Fmt.using (fun (_, vl, _) -> vl) (parens (kwd "type" ++ list ~sep:Fmt.sp (fun f x -> Fmt.string f x.txt))) ++
                 Fmt.cut ++ Fmt.using (fun (_, _, p) -> p) pattern_parens))
        f (li, vl, p)
    | Ppat_variant (l, None) -> (fstr "`" ++ Fmt.string) f l
    | Ppat_variant (l, Some p) ->
        (box 2 (parens (fstr "`" ++ Fmt.using fst Fmt.string ++ Fmt.cut ++ Fmt.using snd pattern_parens)))
        f (l, p)
    | Ppat_record (l, closed) ->
        let field f (li, p) = match li, p with
          | { txt = Lident s; _ }, { ppat_desc = Ppat_var { txt; _ }; ppat_attributes = []; _ } when s = txt ->
              longident_loc f li
          | _ ->
              (box 2 (Fmt.using fst longident_loc ++ Fmt.cut ++ op "=" ++ Fmt.using snd pattern)) f (li, p)
        in
        let suffix = if closed = Closed then "" else ";_" in
        (box 2 (braces (Fmt.cut ++ list field ~sep:(semi ++ Fmt.cut) ++ fstr suffix)))
        f l
    | Ppat_array l ->
        (box 2 (bracks (list pattern ~sep:semi))) f l
    | Ppat_or (p1, p2) ->
        (Fmt.hvbox (parens (Fmt.using fst pattern ++ Fmt.sp ++ fstr "| " ++ Fmt.using snd pattern))) f (p1, p2)
    | Ppat_constraint (p, ct) ->
        (box 2 (parens (Fmt.using fst pattern ++ Fmt.cut ++ sep ":" ++ Fmt.cut ++ Fmt.using snd core_type))) f (p, ct)
    | Ppat_type li ->
        (fstr "#" ++ longident_loc) f li
    | Ppat_lazy p ->
        (box 2 (parens (kwd "lazy" ++ pattern_parens))) f p
    | Ppat_unpack { txt } ->
        (parens (kwd "module" ++ Fmt.string)) f (opt_val txt ~default:"_")
    | Ppat_exception p ->
        (box 2 (parens (kwd "exception" ++ pattern))) f p
    | Ppat_extension e -> extension f e
    | Ppat_open (lid, p) ->
        (box 2 (Fmt.using fst longident_loc ++ fstr "." ++ Fmt.using snd (parens pattern))) f (lid, p)
    | Ppat_alias (p, s) ->
        box 2 (fun f p ->
          Fmt.string f "(";
          pattern f p;
          Fmt.cut f ();
          Fmt.string f "as";
          Fmt.sp f ();
          protect_ident s.txt f s.txt;
          Fmt.string f ")"
        ) f p

and pattern_parens f x =
  (parens_if (needs_parens_pat x) pattern) f x

and expression f x =
  if x.pexp_attributes <> [] then
    (parens (Fmt.using fst (parens expression) ++ Fmt.comma ++ Fmt.using snd attributes))
    f ({ x with pexp_attributes = [] }, x.pexp_attributes)
  else
    match x.pexp_desc with
    | Pexp_ident li -> longident_loc f li
    | Pexp_constant c -> constant f c
    | Pexp_let (rf, l, e) ->
        (box 2 (parens (Fmt.using fst bindings ++ kwd "in" ++ Fmt.sp ++ Fmt.any "<1 -2>" ++ Fmt.using snd expression)))
        f ((rf, l), e)
    | Pexp_function cases ->
        (hbox (parens (kwd "function" ++ case_list))) f cases
    | Pexp_fun (l, e0, p, e) ->
        (box 2 (parens (kwd "fun" ++ Fmt.using fst label_exp ++ op "->" ++ Fmt.using snd expression)))
        f ((l, e0, p), e)
    | Pexp_apply (e, l) -> print_apply f e l
    | Pexp_match (e, cases) ->
        (Fmt.hvbox (parens (kwd "match" ++ Fmt.using fst expression ++ Fmt.sp ++ kwd "with" ++ Fmt.using snd case_list)))
        f (e, cases)
    | Pexp_try (e, cases) ->
        (Fmt.box ~indent:0 (parens (kwd "try" ++ Fmt.using fst expression ++ Fmt.sp ++ kwd "with" ++ Fmt.using snd case_list)))
        f (e, cases)
    | Pexp_tuple l ->
        (hvbox (parens (list expression ~sep:(comma ++ Fmt.cut)))) f l
    | Pexp_construct _ when is_simple_construct (view_expr x) ->
        (match view_expr x with
        | `nil -> Fmt.string f "[]"
        | `tuple -> Fmt.string f "()"
        | `list xs ->
            (Fmt.hvbox (brackets (list expression ~sep:(semi ++ Fmt.cut)))) f xs
        | `simple x -> longident f x
        | _ -> assert false)
    | Pexp_construct (li, None) -> longident_loc f li
    | Pexp_construct (li, Some eo) ->
        (match view_expr x with
        | `cons ls ->
            (parens (list expression ~sep:(Fmt.cut ++ op "::"))) f ls
        | _ ->
            (box 2 (parens (Fmt.using fst longident_loc ++ Fmt.cut ++ Fmt.using snd expression_parens))) f (li, eo))
    | Pexp_variant (l, None) ->
        (fstr "`" ++ Fmt.string) f l
    | Pexp_variant (l, Some eo) ->
        (box 2 (parens (fstr "`" ++ Fmt.using fst Fmt.string ++ Fmt.cut ++ Fmt.using snd expression_parens)))
        f (l, eo)
    | Pexp_record (l, eo) ->
        let longident_x_expression f (li, e) =
          match e with
          | { pexp_desc = Pexp_ident { txt; _ }; pexp_attributes = [] } when li.txt = txt ->
              (hvbox longident_loc) f li
          | _ ->
              (hvbox (Fmt.using fst longident_loc ++ Fmt.cut ++ op "=" ++ Fmt.using snd expression)) f (li, e)
        in
        Fmt.hvbox (fun f (eo, l) ->
          hvbox (fun f () ->
            Fmt.string f "{";
            Fmt.cut f ();
            option ~last:(Fmt.sp ++ kwd "with") expression f eo;
            list longident_x_expression ~sep:(semi ++ Fmt.cut) f l;
            Fmt.string f "}"
          ) f ();
          Fmt.cut f ()
        ) f (eo, l)
    | Pexp_field (e, li) ->
        (hvbox (Fmt.using fst expression_parens ++ fstr "." ++ Fmt.using snd longident_loc)) f (e, li)
    | Pexp_setfield (e1, li, e2) ->
        (box 2 (parens (Fmt.using (fun (e1, _, _) -> e1) expression_parens ++
                        fstr "." ++
                        Fmt.using (fun (_, li, _) -> li) longident_loc ++
                        Fmt.sp ++ op "<-" ++
                        Fmt.using (fun (_, _, e2) -> e2) expression)))
        f (e1, li, e2)
    | Pexp_array l ->
        (Fmt.box ~indent:0 (box 2 (bracks (list expression ~sep:semi)))) f l
    | Pexp_ifthenelse (e1, e2, eo) ->
        let else_part f = function
          | Some x ->
              Fmt.cut f ();
              Fmt.string f "else";
              Fmt.sp f ();
              expression f x
          | None -> ()
        in
        Fmt.hvbox (fun f (e1, e2, eo) ->
          Fmt.string f "(";
          Fmt.string f "if";
          Fmt.sp f ();
          expression f e1;
          Fmt.cut f ();
          Fmt.string f "then";
          Fmt.sp f ();
          expression f e2;
          else_part f eo;
          Fmt.string f ")"
        ) f (e1, e2, eo)
    | Pexp_sequence (e1, e2) ->
        hbox (fun f (e1, e2) ->
          Fmt.string f "(";
          Fmt.string f "begin";
          Fmt.sp f ();
          expression f e1;
          semi f ();
          Fmt.cut f ();
          expression f e2;
          Fmt.sp f ();
          Fmt.string f "end";
          Fmt.string f ")"
        ) f (e1, e2)
    | Pexp_while (e1, e2) ->
        box 2 (fun f (e1, e2) ->
          Fmt.string f "(";
          Fmt.string f "while";
          Fmt.sp f ();
          expression f e1;
          Fmt.cut f ();
          Fmt.string f "do";
          Fmt.cut f ();
          expression f e2;
          Fmt.cut f ();
          Fmt.string f "done";
          Fmt.string f ")"
        ) f (e1, e2)
    | Pexp_for (s, e1, e2, df, e3) ->
        Fmt.hvbox (fun f (s, e1, df, e2, e3) ->
          hvbox (fun f () ->
            box 2 (fun f () ->
              Fmt.string f "(";
              Fmt.string f "for";
              Fmt.sp f ();
              pattern f s;
              Fmt.sp f ();
              Fmt.string f "=";
              Fmt.sp f ();
              expression f e1;
              Fmt.cut f ();
              direction_flag df f ();
              expression f e2;
              Fmt.cut f ();
              Fmt.string f "do"
            ) f ();
            Fmt.cut f ();
            expression f e3
          ) f ();
          Fmt.cut f ();
          Fmt.string f "done";
          Fmt.string f ")"
        ) f (s, e1, df, e2, e3)
    | Pexp_constraint (e, ct) ->
        (parens (Fmt.using fst expression ++ sep " : " ++ Fmt.using snd core_type)) f (e, ct)
    | Pexp_coerce (e, cto1, ct) ->
        (parens (Fmt.using (fun (e, _, _) -> e) expression ++
                 Fmt.using (fun (_, cto1, _) -> cto1) (option core_type ~first:(sep " : ") ~last:Fmt.sp) ++
                 sep " :> " ++ Fmt.using (fun (_, _, ct) -> ct) core_type))
        f (e, cto1, ct)
    | Pexp_send (e, s) ->
        (hvbox (Fmt.using fst expression_parens ++ fstr "#" ++ Fmt.using snd Fmt.string)) f (e, s.txt)
    | Pexp_new li ->
        (hvbox (parens (kwd "new" ++ longident_loc))) f li
    | Pexp_setinstvar (s, e) ->
        (hvbox (parens (Fmt.using fst Fmt.string ++ Fmt.sp ++ op "<-" ++ Fmt.using snd expression))) f (s.txt, e)
    | Pexp_override l ->
        let string_x_expression f (s, e) =
          (hvbox (Fmt.using fst Fmt.string ++ Fmt.sp ++ op "=" ++ Fmt.using snd expression)) f (s.txt, e)
        in
        (hvbox (fstr "{<" ++ list string_x_expression ~sep:semi ++
                fstr ">}"))
        f l
    | Pexp_letmodule (s, me, e) ->
        (hvbox (parens (kwd "let" ++ kwd "module" ++
                        Fmt.using (fun (s, _, _) -> s) Fmt.string ++ Fmt.sp ++ op "=" ++
                        Fmt.using (fun (_, me, _) -> me) module_expr ++ Fmt.sp ++ kwd "in" ++
                        Fmt.using (fun (_, _, e) -> e) expression)))
        f (Option.value s.txt ~default:"_", me, e)
    | Pexp_letexception (cd, e) ->
        (hvbox (parens (kwd "let" ++ kwd "exception" ++
                        Fmt.using fst extension_constructor ++
                        Fmt.sp ++ kwd "in" ++
                        Fmt.using snd expression)))
        f (cd, e)
    | Pexp_assert e ->
        (hvbox (parens (kwd "assert" ++ expression_parens))) f e
    | Pexp_lazy e ->
        (hvbox (parens (kwd "lazy" ++ expression_parens))) f e
    | Pexp_object cs -> class_structure f cs
    | Pexp_newtype (lid, e) ->
        (box 2 (parens (kwd "fun" ++ Fmt.using fst (parens (kwd "type" ++ Fmt.string)) ++
                        Fmt.cut ++ op "->" ++ Fmt.using snd expression)))
        f (lid.txt, e)
    | Pexp_pack me ->
        (parens (kwd "module" ++ module_expr)) f me
    | Pexp_open (o, e) ->
        (box 2 (parens (kwd "let" ++ fstr "open" ++
                        fstr (override o.popen_override) ++ Fmt.sp ++
                        Fmt.using fst module_expr ++ kwd "in" ++ Fmt.cut ++
                        Fmt.using snd expression)))
        f (o.popen_expr, e)
    | Pexp_letop { let_; ands; body } ->
        Fmt.pf f "@[<2>(%a@,%a@ in@;<1 -2>%a)@]"
          binding_op let_ (list ~sep:Fmt.cut binding_op) ands expression body
    | Pexp_extension e -> extension f e
    | Pexp_unreachable -> Fmt.pf f "."
    | Pexp_poly (e, None) -> Fmt.pf f "@[<hov2>(!poly!@ %a)@]" expression_parens e
    | Pexp_poly (e, Some ct) -> Fmt.pf f "@[<hov2>(!poly!@ %a@ : %a)@]" expression_parens e core_type ct

and expression_parens f x =
  (parens_if (needs_parens_expr x) expression) f x

and print_apply f e l =
  match view_fixity_of_exp e with
  | `Infix s ->
      (match l with
      | [ (Nolabel, arg1); (Nolabel, arg2) ] ->
          Fmt.pf f "@[<2>(%a@;%s@;%a)@]" expression_parens arg1 s expression_parens arg2
      | [ (_, arg1); (_, arg2) ] when List.mem s [ "::"; "+"; "*"; "-" ] ->
          Fmt.pf f "@[<2>(%a@;%s@;%a)@]" expression_parens arg1 s expression_parens arg2
      | _ -> Fmt.pf f "@[<2>(%a %a)@]" expression_parens e (list label_x_expression) l)
  | `Prefix s ->
      let s =
        if List.mem s [ "~+"; "~-"; "~+."; "~-." ] then
          match l with
          | [ (_, { pexp_desc = Pexp_constant _ }) ] -> s
          | _ -> String.sub s 1 (String.length s - 1)
        else s
      in
      (match l with
      | [ (Nolabel, x) ] -> Fmt.pf f "@[<2>(%s@;%a)@]" s expression_parens x
      | _ -> Fmt.pf f "@[<2>(%a %a)@]" expression_parens e (list label_x_expression) l)
  | _ -> Fmt.pf f "@[<hov2>(%a@ %a)@]" expression_parens e (list label_x_expression) l

and label_exp f (l, opt, p) =
  match l with
  | Nolabel -> (pattern_parens ++ Fmt.sp) f p
  | Optional rest ->
      (match p with
      | { ppat_desc = Ppat_var { txt; _ }; ppat_attributes = [] } when txt = rest ->
          (match opt with
          | Some o ->
              (fstr "?" ++ parens (Fmt.using fst Fmt.string ++ op "=" ++ Fmt.using snd expression) ++ Fmt.cut)
              f (rest, o)
          | None -> (fstr "?" ++ Fmt.string ++ Fmt.sp) f rest)
      | _ ->
          (match opt with
          | Some o ->
              (fstr "?" ++ Fmt.using (fun (r, _, _) -> r) Fmt.string ++ sep ":" ++
               Fmt.using (fun (_, p, o) -> (p, o)) (parens (Fmt.using fst pattern ++ op "=" ++ Fmt.using snd expression)) ++ Fmt.cut)
              f (rest, p, o)
          | None ->
              (fstr "?" ++ Fmt.using fst Fmt.string ++ sep ":" ++ Fmt.using snd pattern_parens ++ Fmt.cut)
              f (rest, p)))
  | Labelled l ->
      (match p with
      | { ppat_desc = Ppat_var { txt; _ }; ppat_attributes = [] } when txt = l ->
          (fstr "~" ++ Fmt.string ++ Fmt.cut) f l
      | _ ->
          (fstr "~" ++ Fmt.using fst Fmt.string ++ sep ":" ++ Fmt.using snd pattern_parens ++ Fmt.cut)
          f (l, p))

and label_x_expression f (l, e) =
  match l with
  | Nolabel -> expression_parens f e
  | Optional str ->
      (match e with
      | { pexp_desc = Pexp_ident { txt = Lident l; _ }; pexp_attributes = [] } when str = l ->
          (fstr "?" ++ Fmt.string) f str
      | _ ->
          (fstr "?" ++ Fmt.using fst Fmt.string ++ sep ":" ++ Fmt.using snd expression_parens) f (str, e))
  | Labelled lbl ->
      (match e with
      | { pexp_desc = Pexp_ident { txt = Lident l; _ }; pexp_attributes = [] } when lbl = l ->
          (fstr "~" ++ Fmt.string) f lbl
      | _ ->
          (fstr "~" ++ Fmt.using fst Fmt.string ++ sep ":" ++ Fmt.using snd expression_parens) f (lbl, e))

and case_list f l =
  let aux f { pc_lhs; pc_guard; pc_rhs } =
    (Fmt.cut ++ fstr "| " ++
     box 2 (Fmt.using (fun (lhs, _, _) -> lhs) pattern ++
            Fmt.using (fun (_, guard, _) -> guard) (option expression ~first:(Fmt.cut ++ kwd "when")) ++
            Fmt.cut ++ op "->" ++
            Fmt.using (fun (_, _, rhs) -> rhs) expression))
    f (pc_lhs, pc_guard, pc_rhs)
  in
  list aux f l ~sep:Fmt.nop

and binding_op f x =
  match (x.pbop_pat, x.pbop_exp) with
  | { ppat_desc = Ppat_var { txt = pvar; _ }; ppat_attributes = [] },
    { pexp_desc = Pexp_ident { txt = Lident evar; _ }; pexp_attributes = [] }
    when pvar = evar ->
      (box 2 (Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd Fmt.string)) f (x.pbop_op.txt, evar)
  | pat, exp ->
      (box 2 (Fmt.using (fun (op, _, _) -> op) Fmt.string ++ Fmt.sp ++
              Fmt.using (fun (_, pat, _) -> pat) pattern ++ Fmt.cut ++ op "=" ++
              Fmt.using (fun (_, _, exp) -> exp) expression))
      f (x.pbop_op.txt, pat, exp)

and bindings f (rf, l) =
  let binding kwd rf f x =
    box 2 (fun f () ->
      Fmt.string f kwd;
      Fmt.sp f ();
      rec_flag rf f ();
      binding_body f x
    ) f ();
    item_attributes f x.pvb_attributes
  in
  match l with
  | [] -> ()
  | [ x ] -> binding "let" rf f x
  | x :: xs ->
      vbox (fun f () ->
        binding "let" rf f x;
        Fmt.comma f ();
        list ~sep:Fmt.comma (binding "and" Nonrecursive) f xs
      ) f ()

and binding_body f { pvb_pat = p; pvb_expr = x; _ } =
  let rec pp_print_pexp_function f x =
    if x.pexp_attributes <> [] then (fstr "=" ++ expression) f x
    else
      match x.pexp_desc with
      | Pexp_fun (label, eo, p, e) ->
          if label = Nolabel then Fmt.pf f "%a@ %a" pattern_parens p pp_print_pexp_function e
          else Fmt.pf f "%a@ %a" label_exp (label, eo, p) pp_print_pexp_function e
      | Pexp_newtype (str, e) -> Fmt.pf f "(type@ %s)@ %a" str.txt pp_print_pexp_function e
      | _ -> (fstr "=" ++ expression) f x
  in
  match p with
  | { ppat_desc = Ppat_constraint (p, ty); ppat_attributes = [] } ->
      Fmt.pf f "%a@;:@;%a@;=@;%a" pattern_parens p core_type ty expression x
  | { ppat_desc = Ppat_var _; ppat_attributes = [] } ->
      Fmt.pf f "%a@ %a" pattern_parens p pp_print_pexp_function x
  | _ -> Fmt.pf f "%a@;=@;%a" pattern p expression x

and attributes f l = List.iter (attribute f) l
and item_attributes f l = List.iter (item_attribute f) l

and attribute f a =
  (box 2 (fstr "[@@" ++ Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd payload ++ fstr "]"))
  f (a.attr_name.txt, a.attr_payload)

and item_attribute f a =
  (box 2 (fstr "[@@@@ " ++ Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd payload ++ fstr "]"))
  f (a.attr_name.txt, a.attr_payload)

and floating_attribute f a =
  (box 2 (fstr "[@@@@@@ " ++ Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd payload ++ fstr "]"))
  f (a.attr_name.txt, a.attr_payload)

and extension f (s, e) =
  (box 2 (fstr "[%" ++ Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd payload ++ fstr "]"))
  f (s.txt, e)

and item_extension f (s, e) =
  (box 2 (fstr "[%%%" ++ Fmt.using fst Fmt.string ++ Fmt.sp ++ Fmt.using snd payload ++ fstr "]"))
  f (s.txt, e)

and payload f = function
  | PStr [ { pstr_desc = Pstr_eval (e, attrs) } ] ->
      (box 2 (Fmt.using fst expression ++ Fmt.using snd item_attributes)) f (e, attrs)
  | PStr x -> structure f x
  | PTyp x -> (fstr ":" ++ Fmt.sp ++ core_type) f x
  | PSig x -> (fstr ":" ++ Fmt.sp ++ signature) f x
  | PPat (x, None) -> (fstr "?" ++ Fmt.sp ++ pattern) f x
  | PPat (x, Some e) ->
      (fstr "?" ++ Fmt.sp ++ Fmt.using fst pattern ++ kwd "when" ++ Fmt.using snd expression) f (x, e)

and value_description f x =
  if x.pval_prim <> [] then
    (Fmt.sp ++ op "=" ++ list ~sep:Fmt.sp (fun f s -> Fmt.pf f "%S" s)) f x.pval_prim

and exception_declaration f x =
  (hvbox (kwd "exception" ++ Fmt.using fst extension_constructor) ++ Fmt.using snd item_attributes)
  f (x.ptyexn_constructor, x.ptyexn_attributes)

(* ========== Class Types ========== *)

and class_params_def f = function
  | [] -> ()
  | l -> (brackets (list type_param ~sep:comma) ++ Fmt.sp) f l

and type_param f (ct, (a, b)) =
  (fstr (type_variance a) ++
   fstr (type_injectivity b) ++
   core_type) f ct

and class_type_field f x =
  match x.pctf_desc with
  | Pctf_inherit ct ->
      (box 2 (kwd "inherit" ++ Fmt.using fst class_type) ++ Fmt.using snd item_attributes) f (ct, x.pctf_attributes)
  | Pctf_val (s, mf, vf, ct) ->
      box 2 (fun f () ->
        Fmt.string f "val ";
        mutable_flag mf f ();
        virtual_flag vf f ();
        Fmt.string f s.txt;
        Fmt.string f " : ";
        core_type f ct;
        item_attributes f x.pctf_attributes
      ) f ()
  | Pctf_method (s, pf, vf, ct) ->
      box 2 (fun f () ->
        Fmt.string f "method ";
        private_flag pf f ();
        virtual_flag vf f ();
        Fmt.string f s.txt;
        Fmt.string f " :";
        Fmt.cut f ();
        core_type f ct;
        item_attributes f x.pctf_attributes
      ) f ()
  | Pctf_constraint (ct1, ct2) ->
      box 2 (fun f () ->
        Fmt.string f "constraint ";
        core_type f ct1;
        Fmt.string f " = ";
        core_type f ct2;
        item_attributes f x.pctf_attributes
      ) f ()
  | Pctf_attribute a -> floating_attribute f a
  | Pctf_extension e -> item_extension f e; item_attributes f x.pctf_attributes

and class_signature f { pcsig_self = ct; pcsig_fields = l; _ } =
  let self f = function { ptyp_desc = Ptyp_any; ptyp_attributes = [] } -> () | ct -> Fmt.pf f " (%a)" core_type ct in
  Fmt.pf f "@[<hv0>@[<hv2>object@[<1>%a@]@ %a@]@ end@]" self ct (list class_type_field ~sep:(fun f () -> Fmt.string f "@;")) l

and class_type f x =
  match x.pcty_desc with
  | Pcty_signature cs -> class_signature f cs; attributes f x.pcty_attributes
  | Pcty_constr (li, l) ->
      Fmt.pf f "%a%a%a"
        (fun f l -> match l with [] -> () | _ -> Fmt.pf f "[%a]@ " (list core_type ~sep:(fun f () -> Fmt.string f ",")) l)
        l longident_loc li attributes x.pcty_attributes
  | Pcty_arrow (l, co, cl) -> Fmt.pf f "@[<2>%a@;->@;%a@]" type_with_label (l, co) class_type cl
  | Pcty_extension e -> extension f e; attributes f x.pcty_attributes
  | Pcty_open (o, e) ->
      Fmt.pf f "@[<2>let open%s %a in@;%a@]" (override o.popen_override) longident_loc o.popen_expr class_type e

and class_type_declaration_list f l =
  let ctd kwd f x =
    box 2 (fun f () ->
      Fmt.string f kwd;
      Fmt.sp f ();
      virtual_flag x.pci_virt f ();
      class_params_def f x.pci_params;
      Fmt.string f x.pci_name.txt;
      Fmt.string f " = ";
      class_type f x.pci_expr;
      item_attributes f x.pci_attributes
    ) f ()
  in
  match l with [] -> () | x :: xs -> Fmt.pf f "@[<v>%a@,%a@]" (ctd "class type") x (list ~sep:(fun f () -> Fmt.string f "@,") (ctd "and")) xs

(* ========== Class Expressions ========== *)

and class_field f x =
  let attrs = x.pcf_attributes in
  match x.pcf_desc with
  | Pcf_inherit (ovf, ce, so) ->
      box 2 (fun f () ->
        Fmt.string f "inherit ";
        Fmt.string f (override ovf);
        Fmt.sp f ();
        class_expr f ce;
        (option ~first:(fun f () -> Fmt.string f " as ") (fun f s -> Fmt.pf f "%s" s.txt)) f so;
        item_attributes f attrs
      ) f ()
  | Pcf_val (s, mf, Cfk_concrete (ovf, e)) ->
      box 2 (fun f () ->
        Fmt.string f "val";
        Fmt.string f (override ovf);
        Fmt.sp f ();
        mutable_flag mf f ();
        Fmt.string f s.txt;
        Fmt.string f " =";
        Fmt.cut f ();
        expression f e;
        item_attributes f attrs
      ) f ()
  | Pcf_val (s, mf, Cfk_virtual ct) ->
      box 2 (fun f () ->
        Fmt.string f "val virtual ";
        mutable_flag mf f ();
        Fmt.string f s.txt;
        Fmt.string f " : ";
        core_type f ct;
        item_attributes f attrs
      ) f ()
  | Pcf_method (s, pf, Cfk_virtual ct) ->
      box 2 (fun f () ->
        Fmt.string f "method virtual ";
        private_flag pf f ();
        Fmt.string f s.txt;
        Fmt.string f " :";
        Fmt.cut f ();
        core_type f ct;
        item_attributes f attrs
      ) f ()
  | Pcf_method (s, pf, Cfk_concrete (ovf, e)) ->
      box 2 (fun f () ->
        Fmt.string f "method";
        Fmt.string f (override ovf);
        Fmt.sp f ();
        private_flag pf f ();
        Fmt.string f s.txt;
        Fmt.string f " =";
        Fmt.cut f ();
        expression f e;
        item_attributes f attrs
      ) f ()
  | Pcf_constraint (ct1, ct2) ->
      box 2 (fun f () ->
        Fmt.string f "constraint ";
        core_type f ct1;
        Fmt.string f " =";
        Fmt.cut f ();
        core_type f ct2;
        item_attributes f attrs
      ) f ()
  | Pcf_initializer e ->
      box 2 (fun f () ->
        Fmt.string f "initializer ";
        expression f e;
        item_attributes f attrs
      ) f ()
  | Pcf_attribute a -> floating_attribute f a
  | Pcf_extension e -> item_extension f e; item_attributes f attrs

and class_structure f { pcstr_self = p; pcstr_fields = l } =
  let self f p = match p.ppat_desc with Ppat_any -> () | Ppat_constraint _ -> Fmt.pf f " %a" pattern p | _ -> Fmt.pf f " (%a)" pattern p in
  Fmt.pf f "@[<hv0>@[<hv2>object%a@;%a@]@;end@]" self p (list class_field) l

and class_expr f x =
  if x.pcl_attributes <> [] then
    Fmt.pf f "((%a)%a)" class_expr { x with pcl_attributes = [] } attributes x.pcl_attributes
  else
    match x.pcl_desc with
    | Pcl_structure cs -> class_structure f cs
    | Pcl_fun (l, eo, p, e) -> Fmt.pf f "(fun@ %a@ ->@ %a)" label_exp (l, eo, p) class_expr e
    | Pcl_let (rf, l, ce) -> Fmt.pf f "(%a@ in@ %a)" bindings (rf, l) class_expr ce
    | Pcl_apply (ce, l) -> Fmt.pf f "((%a)@ %a)" class_expr ce (list label_x_expression) l
    | Pcl_constr (li, l) ->
        Fmt.pf f "%a%a"
          (fun f l -> if l <> [] then Fmt.pf f "[%a]@ " (list core_type ~sep:(fun f () -> Fmt.string f ",")) l) l longident_loc li
    | Pcl_constraint (ce, ct) -> Fmt.pf f "(%a@ :@ %a)" class_expr ce class_type ct
    | Pcl_extension e -> extension f e
    | Pcl_open (o, e) ->
        Fmt.pf f "@[<2>(let open%s %a in@;%a)@]" (override o.popen_override) longident_loc o.popen_expr class_expr e

(* ========== Module Types ========== *)

and module_type f x =
  if x.pmty_attributes <> [] then
    Fmt.pf f "((%a)%a)" module_type { x with pmty_attributes = [] } attributes x.pmty_attributes
  else
    match x.pmty_desc with
    | Pmty_ident li -> Fmt.pf f "%a" longident_loc li
    | Pmty_alias li -> Fmt.pf f "(module %a)" longident_loc li
    | Pmty_signature s -> Fmt.pf f "@[<hv0>@[<hv2>sig@ %a@]@ end@]" (list signature_item) s
    | Pmty_functor (Unit, mt2) -> Fmt.pf f "@[<hov2>functor () ->@ %a@]" module_type mt2
    | Pmty_functor (Named (s, mt1), mt2) ->
        (match s.txt with
        | None -> Fmt.pf f "@[<hov2>%a@ ->@ %a@]" module_type mt1 module_type mt2
        | Some name -> Fmt.pf f "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]" name module_type mt1 module_type mt2)
    | Pmty_with (mt, []) -> module_type f mt
    | Pmty_with (mt, l) -> Fmt.pf f "@[<hov2>%a@ with@ %a@]" module_type mt (list with_constraint ~sep:(fun f () -> Fmt.string f "@ and@ ")) l
    | Pmty_typeof me -> Fmt.pf f "@[<hov2>module@ type@ of@ %a@]" module_expr me
    | Pmty_extension e -> extension f e

and with_constraint f =
   
  let type_params ls = list core_type ~sep:(fun f () -> Fmt.string f ",") ~first:(fun f () -> Fmt.string f "(") ~last:(fun f () -> Fmt.string f ")") in
  function
  | Pwith_type (li, td) -> Fmt.pf f "type@ %a %a = %a" (type_params td.ptype_params) (List.map fst td.ptype_params) longident_loc li type_declaration td
  | Pwith_typesubst (li, td) -> Fmt.pf f "type %a %a := %a" (type_params td.ptype_params) (List.map fst td.ptype_params) longident_loc li type_declaration td
  | Pwith_module (li, li2) -> Fmt.pf f "module %a = %a" longident_loc li longident_loc li2
  | Pwith_modsubst (li, li2) -> Fmt.pf f "module %a := %a" longident_loc li longident_loc li2
  | Pwith_modtype (li, mty) -> (fstr "module type" ++ Fmt.sp ++ Fmt.using fst longident_loc ++ Fmt.using snd module_type) f (li, mty)
  | Pwith_modtypesubst (li, mty) -> Fmt.pf f "module type %a := %a" longident_loc li module_type mty

and signature f x = list ~sep:(fun f () -> Fmt.string f "@\n") signature_item f x

and signature_item f x =
  match x.psig_desc with
  | Psig_type (rf, l) -> type_def_list f (rf, true, l)
  | Psig_typesubst l -> type_def_list f (Recursive, false, l)
  | Psig_value vd ->
      let intro = if vd.pval_prim = [] then "val" else "external" in
      box 2 (fun f () ->
        Fmt.string f intro;
        Fmt.sp f ();
        protect_ident vd.pval_name.txt f vd.pval_name.txt;
        Fmt.string f " : ";
        core_type f vd.pval_type;
        value_description f vd;
        item_attributes f vd.pval_attributes
      ) f ()
  | Psig_typext te -> type_extension f te
  | Psig_exception ed -> exception_declaration f ed
  | Psig_class l ->
      let class_desc kwd f x =
        box 2 (fun f () ->
          Fmt.string f kwd;
          Fmt.sp f ();
          virtual_flag x.pci_virt f ();
          class_params_def f x.pci_params;
          Fmt.string f x.pci_name.txt;
          Fmt.string f " :";
          Fmt.cut f ();
          class_type f x.pci_expr;
          item_attributes f x.pci_attributes
        ) f ()
      in
      (match l with [] -> () | x :: xs -> Fmt.pf f "@[<v>%a@,%a@]" (class_desc "class") x (list ~sep:(fun f () -> Fmt.string f "@,") (class_desc "and")) xs)
  | Psig_module ({ pmd_type = { pmty_desc = Pmty_alias alias; pmty_attributes = [] }; _ } as pmd) ->
      Fmt.pf f "@[<hov>module@ %s@ =@ %a@]%a"
        (Option.value pmd.pmd_name.txt ~default:"_") longident_loc alias item_attributes pmd.pmd_attributes
  | Psig_module pmd ->
      Fmt.pf f "@[<hov>module@ %s@ :@ %a@]%a"
        (Option.value pmd.pmd_name.txt ~default:"_") module_type pmd.pmd_type item_attributes pmd.pmd_attributes
  | Psig_modsubst pms ->
      Fmt.pf f "@[<hov>module@ %s@ :=@ %a@]%a" pms.pms_name.txt longident_loc pms.pms_manifest item_attributes pms.pms_attributes
  | Psig_open od ->
      Fmt.pf f "@[<hov2>open%s@ %a@]%a" (override od.popen_override) longident_loc od.popen_expr item_attributes od.popen_attributes
  | Psig_include incl ->
      Fmt.pf f "@[<hov2>include@ %a@]%a" module_type incl.pincl_mod item_attributes incl.pincl_attributes
  | Psig_modtype { pmtd_name = s; pmtd_type; pmtd_attributes } ->
      Fmt.pf f "@[<hov2>module@ type@ %s%a@]%a" s.txt (option ~first:(fun f () -> Fmt.string f " =@ ") module_type) pmtd_type item_attributes pmtd_attributes
  | Psig_modtypesubst { pmtd_name = s; pmtd_type = Some mt; pmtd_attributes } ->
      Fmt.pf f "@[<hov2>module@ type@ %s@ :=@ %a@]%a" s.txt module_type mt item_attributes pmtd_attributes
  | Psig_modtypesubst { pmtd_type = None; _ } -> assert false
  | Psig_class_type l -> class_type_declaration_list f l
  | Psig_recmodule decls ->
      let pmd_item kwd f pmd =
        Fmt.pf f "@[<hov2>%s@ %s:@ %a@]%a" kwd (opt_val pmd.pmd_name.txt ~default:"_") module_type pmd.pmd_type item_attributes pmd.pmd_attributes
      in
      (match decls with
       | [] -> ()
       | x :: xs -> Fmt.pf f "%a%a" (pmd_item "module rec") x (list ~sep:(fun f () -> Fmt.string f "@,") (pmd_item "and")) xs)
  | Psig_attribute a -> floating_attribute f a
  | Psig_extension (e, a) -> item_extension f e; item_attributes f a

(* ========== Module Expressions ========== *)

and module_expr f x =
  if x.pmod_attributes <> [] then
    Fmt.pf f "((%a)%a)" module_expr { x with pmod_attributes = [] } attributes x.pmod_attributes
  else
    match x.pmod_desc with
    | Pmod_ident li -> Fmt.pf f "%a" longident_loc li
    | Pmod_structure s -> Fmt.pf f "@[<hv2>struct@;@[<0>%a@]@;<1 -2>end@]" (list structure_item ~sep:(fun f () -> Fmt.string f "@\n")) s
    | Pmod_constraint (me, mt) -> Fmt.pf f "@[<hov2>(%a@ :@ %a)@]" module_expr me module_type mt
    | Pmod_functor (Unit, me) -> Fmt.pf f "functor ()@;->@;%a" module_expr me
    | Pmod_functor (Named (s, mt), me) ->
        Fmt.pf f "functor@ (%s@ :@ %a)@;->@;%a" (Option.value s.txt ~default:"_") module_type mt module_expr me
    | Pmod_apply (me1, me2) -> Fmt.pf f "(%a)(%a)" module_expr me1 module_expr me2
    | Pmod_unpack e -> Fmt.pf f "(val@ %a)" expression e
    | Pmod_extension e -> extension f e

and structure f x = list ~sep:(fun f () -> Fmt.string f "@\n") structure_item f x

and structure_item f x =
  match x.pstr_desc with
  | Pstr_eval (e, attrs) -> Fmt.pf f "@[<hov2>;;%a@]%a" expression e item_attributes attrs
  | Pstr_type (_, []) -> assert false
  | Pstr_type (rf, l) -> type_def_list f (rf, true, l)
  | Pstr_value (rf, l) -> Fmt.pf f "@[<2>%a@]" bindings (rf, l)
  | Pstr_typext te -> type_extension f te
  | Pstr_exception ed -> exception_declaration f ed
  | Pstr_module x ->
      let rec module_helper = function
        | { pmod_desc = Pmod_functor (arg_opt, me'); pmod_attributes = [] } ->
            (match arg_opt with
            | Unit -> Fmt.pf f "()"
            | Named (s, mt) -> Fmt.pf f "(%s:%a)" (Option.value s.txt ~default:"_") module_type mt);
            module_helper me'
        | me -> me
      in
      Fmt.pf f "@[<hov2>module %s%a@]%a"
        (Option.value x.pmb_name.txt ~default:"_")
        (fun f me ->
          let me = module_helper me in
          match me with
          | { pmod_desc = Pmod_constraint (me', ({ pmty_desc = Pmty_ident _ | Pmty_signature _; _ } as mt)); pmod_attributes = [] } ->
              Fmt.pf f " :@;%a@;=@;%a@;" module_type mt module_expr me'
          | _ -> Fmt.pf f " =@ %a" module_expr me)
        x.pmb_expr item_attributes x.pmb_attributes
  | Pstr_open od ->
      Fmt.pf f "@[<2>open%s@;%a@]%a" (override od.popen_override) module_expr od.popen_expr item_attributes od.popen_attributes
  | Pstr_modtype { pmtd_name = s; pmtd_type; pmtd_attributes } ->
      Fmt.pf f "@[<hov2>module@ type@ %s%a@]%a" s.txt (option ~first:(fun f () -> Fmt.string f " =@ ") module_type) pmtd_type item_attributes pmtd_attributes
  | Pstr_class l ->
      let extract_class_args cl =
        let rec loop acc = function
          | { pcl_desc = Pcl_fun (l, eo, p, cl'); pcl_attributes = [] } -> loop ((l, eo, p) :: acc) cl'
          | cl -> List.rev acc, cl
        in
        let args, cl = loop [] cl in
        match cl with { pcl_desc = Pcl_constraint (cl', ct); pcl_attributes = [] } -> args, Some ct, cl' | _ -> args, None, cl
      in
      let class_decl kwd f x =
        let args, constr, cl = extract_class_args x.pci_expr in
        box 2 (fun f () ->
          Fmt.string f kwd;
          Fmt.sp f ();
          virtual_flag x.pci_virt f ();
          class_params_def f x.pci_params;
          Fmt.string f x.pci_name.txt;
          Fmt.sp f ();
          (list label_exp) f args;
          (option (fun f ct -> Fmt.pf f ": @[%a@] " class_type ct)) f constr;
          Fmt.string f "=";
          Fmt.cut f ();
          class_expr f cl;
          item_attributes f x.pci_attributes
        ) f ()
      in
      (match l with [] -> () | x :: xs -> Fmt.pf f "@[<v>%a@,%a@]" (class_decl "class") x (list ~sep:(fun f () -> Fmt.string f "@,") (class_decl "and")) xs)
  | Pstr_class_type l -> class_type_declaration_list f l
  | Pstr_primitive vd ->
      box 2 (fun f () ->
        Fmt.string f "external ";
        protect_ident vd.pval_name.txt f vd.pval_name.txt;
        Fmt.string f " : ";
        core_type f vd.pval_type;
        value_description f vd;
        item_attributes f vd.pval_attributes
      ) f ()
  | Pstr_include incl ->
      Fmt.pf f "@[<hov2>include@ %a@]%a" module_expr incl.pincl_mod item_attributes incl.pincl_attributes
  | Pstr_recmodule decls ->
      let pmb_item kwd f pmb =
        let name = opt_val pmb.pmb_name.txt ~default:"_" in
        match pmb.pmb_expr.pmod_desc with
        | Pmod_constraint (expr, typ) -> Fmt.pf f "@[<hov2>%s@ %s:%a@ =@ %a@]%a" kwd name module_type typ module_expr expr item_attributes pmb.pmb_attributes
        | _ -> Fmt.pf f "@[<hov2>%s@ %s@ =@ %a@]%a" kwd name module_expr pmb.pmb_expr item_attributes pmb.pmb_attributes
      in
      (match decls with
       | [] -> assert false
       | x :: xs -> Fmt.pf f "@[<hv>%a%a@]" (pmb_item "module rec") x (list ~sep:(fun f () -> Fmt.string f "@ ") (pmb_item "and")) xs)
  | Pstr_attribute a -> floating_attribute f a
  | Pstr_extension (e, a) -> item_extension f e; item_attributes f a

(* ========== Type Declarations ========== *)

and type_params f = function
  | [] -> ()
  | l -> (parens (list type_param ~sep:(comma ++ Fmt.cut)) ++ Fmt.sp) f l

and type_def_list f (rf, exported, l) =
  let type_decl kwd rf f x =
    let eq =
      if x.ptype_kind = Ptype_abstract && x.ptype_manifest = None then ""
      else if exported then " ="
      else " :="
    in
    box 2 (fun f () ->
      Fmt.string f kwd;
      Fmt.sp f ();
      nonrec_flag rf f ();
      type_params f x.ptype_params;
      Fmt.string f x.ptype_name.txt;
      Fmt.string f eq;
      type_declaration f x;
      item_attributes f x.ptype_attributes
    ) f ()
  in
  match l with
  | [] -> assert false
  | [ x ] -> type_decl "type" rf f x
  | x :: xs -> Fmt.pf f "@[<v>%a@,%a@]" (type_decl "type" rf) x (list ~sep:(fun f () -> Fmt.string f "@,") (type_decl "and" Recursive)) xs

and record_declaration f lbls =
  let type_record_field f pld =
    box 2 (fun f () ->
      mutable_flag pld.pld_mutable f ();
      Fmt.string f pld.pld_name.txt;
      Fmt.string f ":";
      Fmt.cut f ();
      core_type f pld.pld_type;
      Fmt.cut f ();
      attributes f pld.pld_attributes
    ) f ()
  in
  Fmt.pf f "{@\n%a}" (list type_record_field ~sep:(fun f () -> Fmt.string f ";@\n")) lbls

and type_declaration f x =
  let priv f = if x.ptype_private = Private then Fmt.pf f "@;private" in
  let ctor f pcd = Fmt.pf f "|@;"; constructor_decl f (pcd.pcd_name.txt, pcd.pcd_vars, pcd.pcd_args, pcd.pcd_res, pcd.pcd_attributes) in
  (match x.ptype_manifest with None -> () | Some y -> if x.ptype_kind = Ptype_abstract then Fmt.pf f "%t@;%a" priv core_type y else Fmt.pf f "@;%a" core_type y);
  let intro f = if x.ptype_manifest <> None then Fmt.pf f "@;=" in
  (match x.ptype_kind with
   | Ptype_variant [] -> Fmt.pf f "%t%t |" intro priv
   | Ptype_variant xs -> Fmt.pf f "%t%t@\n%a" intro priv (list ~sep:(fun f () -> Fmt.string f "@\n") ctor) xs
   | Ptype_abstract -> ()
   | Ptype_record l -> Fmt.pf f "%t%t@;%a" intro priv record_declaration l
   | Ptype_open -> Fmt.pf f "%t%t@;.." intro priv);
  List.iter (fun (ct1, ct2, _) -> Fmt.pf f "@[<hov2>@ constraint@ %a@ =@ %a@]" core_type ct1 core_type ct2) x.ptype_cstrs

and type_extension f x =
  let extension_constructor_item f x = Fmt.pf f "@\n|@;%a" extension_constructor x in
  box 2 (fun f () ->
    Fmt.string f "type ";
    (fun f -> function [] -> () | l -> Fmt.pf f "%a@;" (list type_param ~first:(fun f () -> Fmt.string f "(") ~last:(fun f () -> Fmt.string f ")") ~sep:(fun f () -> Fmt.string f ",")) l) f x.ptyext_params;
    longident_loc f x.ptyext_path;
    Fmt.string f " += ";
    private_flag x.ptyext_private f ();
    Fmt.sp f ();
    (list ~sep:Fmt.nop extension_constructor_item) f x.ptyext_constructors;
    item_attributes f x.ptyext_attributes
  ) f ()

and constructor_decl f (name, vars, args, res, attrs) =
  let name = if name = "::" then "(::)" else name in
  let pp_args f = function Pcstr_tuple [] -> () | Pcstr_tuple l -> Fmt.pf f "%a" (list core_type ~sep:(fun f () -> Fmt.string f "@;*@;")) l | Pcstr_record l -> record_declaration f l in
  match res with
  | None -> Fmt.pf f "%s%a@;%a" name (fun f -> function Pcstr_tuple [] -> () | a -> Fmt.pf f "@;of@;%a" pp_args a) args attributes attrs
  | Some r ->
      let pp_vars f = function [] -> () | vs -> Fmt.pf f "%a@;.@;" (list tyvar_loc ~sep:(fun f () -> Fmt.string f "@;")) vs in
      Fmt.pf f "%s:@;%a%a@;%a" name pp_vars vars (fun f -> function Pcstr_tuple [] -> core_type f r | a -> Fmt.pf f "%a@;->@;%a" pp_args a core_type r) args attributes attrs

and extension_constructor f x =
  match x.pext_kind with
  | Pext_decl (v, l, r) -> constructor_decl f (x.pext_name.txt, v, l, r, x.pext_attributes)
  | Pext_rebind li -> Fmt.pf f "%s@;=@;%a%a" x.pext_name.txt longident_loc li attributes x.pext_attributes

(* ========== Toplevel ========== *)

and directive_argument f x = match x.pdira_desc with
  | Pdir_string s -> Fmt.pf f "@ %S" s | Pdir_int (n, None) -> Fmt.pf f "@ %s" n | Pdir_int (n, Some m) -> Fmt.pf f "@ %s%c" n m
  | Pdir_ident li -> Fmt.pf f "@ %a" longident li | Pdir_bool b -> Fmt.pf f "@ %s" (string_of_bool b)

let toplevel_phrase f = function
  | Ptop_def s -> Fmt.pf f "@[<hov0>%a@]" (list structure_item) s
  | Ptop_dir { pdir_name; pdir_arg; _ } -> Fmt.pf f "@[<hov2>#%s%a@]" pdir_name.txt (option directive_argument) pdir_arg

let top_phrase f x = Fmt.cut f (); toplevel_phrase f x; Fmt.pf f ";;"; Fmt.cut f ()

(* ========== External Interface ========== *)


let longident_fmt fmt lid = Fmt.list ~sep:(Fmt.any ".") Fmt.string fmt (Longident.flatten lid)
let constr = longident_fmt

let ocaml_keywords = [ "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"; "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with" ]

let tyvar_of_name name =
  if name = "_" then name
  else if List.mem name ocaml_keywords then "\\#" ^ name
  else match String.length name with n when n >= 2 && name.[1] = '\'' -> "'" ^ String.sub name 0 1 ^ String.sub name 2 (n - 2) | _ -> name

let tyvar fmt name = Fmt.string fmt (tyvar_of_name name)
let longident = longident_fmt
let binding = binding_body
let string_of_expression x = Fmt.str "%a" expression x
let string_of_structure x = Fmt.str "%a" structure x

module PrintAst = struct
  let expression = expression
  let pattern = pattern
  let core_type = core_type
  let signature = signature
  let structure = structure
  let class_expr = class_expr
  let class_field = class_field
  let class_type = class_type
  let class_signature = class_signature
  let class_type_field = class_type_field
  let module_expr = module_expr
  let module_type = module_type
  let signature_item = signature_item
  let structure_item = structure_item
  let type_declaration = type_declaration
  let binding = binding_body
  let payload = payload
  let toplevel_phrase = toplevel_phrase
  let top_phrase = top_phrase
  let string_of_expression = string_of_expression
  let string_of_structure = string_of_structure
end
