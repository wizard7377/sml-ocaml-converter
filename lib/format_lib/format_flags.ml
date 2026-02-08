(* Format_flags: Flags, simple printers, and constants *)

open Astlib.Ast_414
open Asttypes
open Parsetree
open Format_utils
open Format_ident

let override = function Override -> "!" | Fresh -> ""

let type_variance = function
  | NoVariance -> ""
  | Covariant -> "+"
  | Contravariant -> "-"

let type_injectivity = function NoInjectivity -> "" | Injective -> "!"

let mutable_flag = function
  | Immutable -> Fmt.nop
  | Mutable -> fstr "mutable" ++ space ++ Fmt.sp

let virtual_flag = function
  | Concrete -> Fmt.nop
  | Virtual -> fstr "virtual" ++ space ++ Fmt.sp

let rec_flag = function Nonrecursive -> Fmt.nop | Recursive -> kwd "rec"
let nonrec_flag = function Nonrecursive -> kwd "nonrec" | Recursive -> Fmt.nop
let direction_flag = function Upto -> kwd "to" | Downto -> kwd "downto"
let private_flag = function Public -> Fmt.nop | Private -> kwd "private"

let escaped_char : char Fmt.t =
 fun f c ->
  Fmt.char f '\'';
  Fmt.string f (Char.escaped c);
  Fmt.char f '\''

let escaped_string : string Fmt.t =
 fun f s ->
  Fmt.char f '"';
  Fmt.string f (String.escaped s);
  Fmt.char f '"'

let constant f = function
  | Pconst_char i -> escaped_char f i
  | Pconst_string (i, _, None) -> escaped_string f i
  | Pconst_string (i, _, Some delim) ->
      (fstr "{"
      ++ Fmt.using (fun (d, _, _) -> d) Fmt.string
      ++ fstr "|"
      ++ Fmt.using (fun (_, i, _) -> i) Fmt.string
      ++ fstr "|"
      ++ Fmt.using (fun (d, _, _) -> d) Fmt.string
      ++ fstr "}")
        f (delim, i, delim)
  | Pconst_integer (i, None) | Pconst_float (i, None) ->
      (parens_if (first_is '-' i) Fmt.string) f i
  | Pconst_integer (i, Some m) | Pconst_float (i, Some m) ->
      (parens_if (first_is '-' i)
         (Fmt.using fst Fmt.string ++ Fmt.using snd Fmt.char))
        f (i, m)

let tyvar f s =
  let fmt =
    if String.length s >= 2 && s.[1] = '\'' then fstr "' " ++ Fmt.string
    else fstr "'" ++ Fmt.string
  in
  fmt f s

let tyvar_loc f str = tyvar f str.Location.txt
