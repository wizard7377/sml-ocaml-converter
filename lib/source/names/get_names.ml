(** Implementation of AST name extraction.

    See {!Get_names} for the public interface documentation.

    {b Note}: This is currently a stub implementation that will be filled in
    as the name extraction logic is developed. The implementation will need
    to recursively traverse all AST node types to extract names. *)


let rec merge_list (lists : Store.t list) : Store.t =
  List.fold_left Store.combine (Store.create []) lists
let rec get_names_prog (context : Store.name_context) (path : Name.path) (prog : Ast.prog) : Store.t =
  match prog with 
  | ProgDec dec -> assert false
  | ProgFun body -> assert false
  | ProgStr str -> assert false
  | ProgSeq (p1, p2) -> assert false
    | ProgEmpty -> Store.create []
and 
    get_names_declaration (context : Store.name_context) (path : Name.path) (dec : Ast.declaration Ast.node) : Store.t =
      match Ast.unbox_node dec with
        | ValDec (n, b) -> assert false 
        | FunDec fb -> assert false
        | TypDec td -> assert false
        | DatDec (dn, db) -> assert false
        | ExnDec en -> assert false
        | StrDec sb -> assert false
        | SeqDec ds -> assert false
        | DataDecAlias (dn, db) -> assert false
        | AbstractDec (dn, dx, dy) -> assert false
        | LocalDec (d1, d2) -> assert false
        | OpenDec oname -> assert false
        | FixityDec (fn, fb) -> assert false




















let get_names (prog : Ast.prog) : Store.t = get_names_prog Store.Structure [] prog 