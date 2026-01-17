(** Utilities for working with SML identifiers (idx type).
    
    This module provides conversion functions for the Ast.idx type,
    which represents various forms of SML identifiers. *)

(** Extract a string from an idx value *)
let rec idx_to_string (idx : Ast.idx) : string =
  match idx with
  | Ast.IdxIdx s -> s.value
  | Ast.IdxVar s -> s.value
  | Ast.IdxLab s -> s.value
  | Ast.IdxNum s -> s.value
  | Ast.IdxLong parts ->
      (* Convert long identifier to dot-separated string *)
      String.concat "."
        (List.map (fun (p : Ast.idx Ast.node) -> idx_to_string p.value) parts)

(** Convert an idx to a list of name parts for processing *)
let rec idx_to_name (idx : Ast.idx) : string list =
  match idx with
  | Ast.IdxIdx s -> [ s.value ]
  | Ast.IdxVar s -> [ s.value ]
  | Ast.IdxLab s -> [ s.value ]
  | Ast.IdxNum s -> [ s.value ]
  | Ast.IdxLong parts ->
      List.flatten
        (List.map (fun (p : Ast.idx Ast.node) -> idx_to_name p.value) parts)
