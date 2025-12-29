let folded (f : 'a -> 'b -> 'a) (lst : 'a) (init : 'b list) : 'a =
  List.fold_left f lst init

(* Fold a list of specs into a SpecSeq chain (right-associative) *)
let fold_spec_list (specs : Ast.spec list) (empty : Ast.spec) : Ast.spec =
  match List.rev specs with
  | [] -> empty
  | [x] -> x
  | last :: rest -> List.fold_left (fun acc s -> Ast.SpecSeq (s, acc)) last rest