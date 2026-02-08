(* Format_lib: External interface / re-export module *)

(* Re-export types from Format_utils *)
type space_formatter = Format_utils.space_formatter
type 'a t = 'a Format_utils.t

(* Re-export from Format_utils *)
let use = Format_utils.use

(* ========== Tie the Recursive Knot ========== *)

module rec CT        : Format_types.CORE_TYPE   = Format_core_type.Make(Attrs)
       and Pat       : Format_types.PATTERN     = Format_pattern.Make(CT)(Attrs)
       and Expr      : Format_types.EXPR        = Format_expression.Make(CT)(Pat)(Attrs)(ClassExpr)(ModExpr)(TypeDecl)
       and Attrs     : Format_types.ATTRS       = Format_attrs.Make(CT)(Pat)(Expr)(ModType)(ModExpr)(TypeDecl)
       and ClassT    : Format_types.CLASS_TYPE   = Format_class_type.Make(CT)(Attrs)
       and ClassExpr : Format_types.CLASS_EXPR   = Format_class_expr.Make(CT)(Pat)(Expr)(Attrs)(ClassT)
       and ModType   : Format_types.MODULE_TYPE  = Format_module_type.Make(CT)(Attrs)(ClassT)(TypeDecl)(ModExpr)
       and ModExpr   : Format_types.MODULE_EXPR  = Format_module_expr.Make(CT)(Expr)(Attrs)(ClassT)(ClassExpr)(ModType)(TypeDecl)
       and TypeDecl  : Format_types.TYPE_DECL    = Format_type_decl.Make(CT)(Attrs)(ClassT)

(* ========== Toplevel Phrases ========== *)

let directive_argument f x =
  let open Astlib.Ast_414 in
  let open Parsetree in
  match x.pdira_desc with
  | Pdir_string s ->
      Fmt.sp f ();
      Format_flags.escaped_string f s
  | Pdir_int (n, None) ->
      Fmt.sp f ();
      Fmt.string f n
  | Pdir_int (n, Some m) ->
      Fmt.sp f ();
      Fmt.string f n;
      Fmt.char f m
  | Pdir_ident li ->
      Fmt.sp f ();
      Format_ident.longident f li
  | Pdir_bool b ->
      Fmt.sp f ();
      Fmt.string f (string_of_bool b)

let toplevel_phrase f = function
  | Astlib.Ast_414.Parsetree.Ptop_def s ->
      Fmt.hvbox ~indent:0 (fun f s -> Format_utils.list ModExpr.structure_item f s) f s
  | Ptop_dir { pdir_name; pdir_arg; _ } ->
      Format_utils.box 2
        (fun f () ->
          Fmt.string f "#";
          Fmt.string f pdir_name.txt;
          Format_utils.option directive_argument f pdir_arg)
        f ()

let top_phrase = Fmt.( ++ ) toplevel_phrase Format_utils.top_sep

(* ========== Re-export Printers ========== *)

let expression = Expr.expression
let pattern = Pat.pattern
let core_type = CT.core_type
let signature = ModType.signature
let structure = ModExpr.structure
let module_expr = ModExpr.module_expr
let module_type = ModType.module_type
let class_expr = ClassExpr.class_expr
let class_field = ClassExpr.class_field
let class_type = ClassT.class_type
let class_type_field = ClassT.class_type_field
let structure_item = ModExpr.structure_item
let signature_item = ModType.signature_item
let payload = Attrs.payload

(* ========== External Interface ========== *)

let longident_fmt fmt lid =
  Fmt.list ~sep:(Fmt.const Fmt.string ".") Fmt.string fmt
    (Longident.flatten lid)

let constr = longident_fmt

let ocaml_keywords =
  [
    "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let tyvar_of_name name =
  if name = "_" then name
  else if List.mem name ocaml_keywords then "\\#" ^ name
  else
    match String.length name with
    | n when n >= 2 && name.[1] = '\'' ->
        "'" ^ String.sub name 0 1 ^ String.sub name 2 (n - 2)
    | _ -> name

let tyvar fmt name = Fmt.string fmt (tyvar_of_name name)
let longident = longident_fmt
let binding = Expr.binding_body
let string_of_expression x = Fmt.to_to_string expression x
let string_of_structure x = Fmt.to_to_string structure x

module PrintAst = struct
  let expression = expression
  let pattern = pattern
  let core_type = core_type
  let signature = signature
  let structure = structure
  let class_expr = class_expr
  let class_field = class_field
  let class_type = class_type
  let class_signature = ClassT.class_signature
  let class_type_field = class_type_field
  let module_expr = module_expr
  let module_type = module_type
  let signature_item = signature_item
  let structure_item = structure_item
  let type_declaration = TypeDecl.type_declaration
  let binding = Expr.binding_body
  let payload = payload
  let toplevel_phrase = toplevel_phrase
  let top_phrase = top_phrase
  let string_of_expression = string_of_expression
  let string_of_structure = string_of_structure
end
