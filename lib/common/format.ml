(* OCaml Parsetree pretty-printer using Fmt-style interfaces *)

type space_formatter = (unit, Stdlib.Format.formatter, unit) format
type 'a t = 'a Fmt.t

(* Longident formatting *)
let longident fmt lid =
  Fmt.list ~sep:(Fmt.any ".") Fmt.string fmt (Longident.flatten lid)

let constr = longident

(* Type variable formatting *)
let ocaml_keywords = [
  "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done";
  "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
  "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
  "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method";
  "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or";
  "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type";
  "val"; "virtual"; "when"; "while"; "with";
]

let is_keyword name = List.mem name ocaml_keywords

let tyvar_of_name name =
  if name = "_" then name
  else if is_keyword name then "\\#" ^ name
  else
    (* Handle quote in second position - OCaml allows 'a but not a' *)
    match String.length name with
    | n when n >= 2 && name.[1] = '\'' ->
        (* Move quote to first position *)
        "'" ^ String.sub name 0 1 ^ String.sub name 2 (n - 2)
    | _ -> name

let tyvar fmt name = Fmt.string fmt (tyvar_of_name name)

(* Direct aliases to Pprintast *)
let expression = Pprintast.expression
let pattern = Pprintast.pattern
let core_type = Pprintast.core_type
let signature = Pprintast.signature
let structure = Pprintast.structure
let module_expr = Pprintast.module_expr
let module_type = Pprintast.module_type
let class_expr = Pprintast.class_expr
let class_type = Pprintast.class_type
let class_field = Pprintast.class_field
let class_type_field = Pprintast.class_type_field
let structure_item = Pprintast.structure_item
let signature_item = Pprintast.signature_item
let binding = Pprintast.binding
let payload = Pprintast.payload
let toplevel_phrase = Pprintast.toplevel_phrase
let top_phrase = toplevel_phrase

(* String conversion helpers *)
let string_of_expression expr = Fmt.str "%a" expression expr
let string_of_structure struc = Fmt.str "%a" structure struc
