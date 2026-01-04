let folded (f : 'a -> 'b -> 'a) (lst : 'a) (init : 'b list) : 'a =
  List.fold_left f lst init

(* Fold a list of specs into a SpecSeq chain (right-associative) *)
let fold_spec_list (specs : Ast.specification list) (empty : Ast.specification) : Ast.specification =
  match List.rev specs with
  | [] -> empty
  | [x] -> x
  | last :: rest -> List.fold_left (fun acc s -> Ast.SpecSeq (Ast.box_node s, Ast.box_node acc)) last rest