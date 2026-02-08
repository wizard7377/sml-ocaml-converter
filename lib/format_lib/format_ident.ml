(* Format_ident: Identifier and fixity handling for pretty-printing *)

open Longident
open Format_utils

let infix_symbols =
  [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/'; '$'; '%'; '#'; ':' ]

let special_infix =
  [ "asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "::" ]

let first_is c s = s <> "" && s.[0] = c
let first_in cs s = s <> "" && List.mem s.[0] cs

let is_op s n c =
  String.length s > 3
  && s.[0] = c
  && s.[1] = n.[0]
  && s.[2] = n.[1]
  && List.mem s.[3] infix_symbols

let rec strip_suffix ~suffix s =
  let len = String.length s and slen = String.length suffix in
  if len >= slen && String.sub s (len - slen) slen = suffix then
    strip_suffix ~suffix (String.sub s 0 (len - slen))
  else s

let normalize_op_name s =
  let s = strip_suffix ~suffix:"_prime" s in
  let rec drop_trailing_underscores s =
    if s <> "" && s.[String.length s - 1] = '_' then
      drop_trailing_underscores (String.sub s 0 (String.length s - 1))
    else s
  in
  drop_trailing_underscores s

let fixity_of_string s =
  let base = normalize_op_name s in
  match base with
  | "" -> `Normal
  | b when List.mem b special_infix -> `Infix s
  | b when first_in infix_symbols b -> `Infix s
  | b when first_in [ '!'; '?'; '~' ] b -> `Prefix s
  | b when b.[0] = '.' -> `Mixfix s
  | b when is_op b "et" 'l' -> `Letop s
  | b when is_op b "nd" 'a' -> `Andop s
  | _ -> `Normal

let needs_parens txt =
  match fixity_of_string txt with
  | `Infix _ | `Mixfix _ | `Letop _ | `Andop _ -> true
  | `Prefix _ -> true
  | `Normal -> false

let needs_spaces txt =
  let base = normalize_op_name txt in
  first_is '*' base || (base <> "" && base.[String.length base - 1] = '*')

let protect_ident txt =
  if not (needs_parens txt) then Fmt.string
  else if needs_spaces txt then parens (Fmt.sp ++ Fmt.string ++ Fmt.sp)
  else parens Fmt.string

let rec longident f = function
  | Lident s -> protect_ident s f s
  | Ldot (y, s) ->
      longident f y;
      if not (needs_parens s) then (fstr "." ++ Fmt.string) f s
      else if needs_spaces s then
        (fstr "." ++ parens (Fmt.sp ++ Fmt.string ++ Fmt.sp)) f s
      else (fstr "." ++ parens Fmt.string) f s
  | Lapply (y, s) ->
      longident f y;
      parens longident f s

let longident_loc f x = longident f x.Location.txt
