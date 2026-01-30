(*
open Ast_core
let (++) = Fmt.(++)
let rec pretty_node : 'a . 'a Fmt.t -> 'a Ast_node.node Fmt.t = fun pp fmt v -> pp fmt v.value
let pretty_idx : idx Fmt.t = assert false


let pretty_con : constant Fmt.t = assert false
let rec pretty_expression : Ast_core.expression Fmt.t = fun fmt -> function 
  | ExpIdx idx -> pretty_node pretty_idx fmt idx 
  | ExpCon const -> pretty_node pretty_con fmt const
  | ParenExp e -> pretty_node (Fmt.parens pretty_expression) fmt e
  | ExpApp (f, x) -> (Fmt.using fst (pretty_node pretty_expression) ++ Fmt.sp ++ Fmt.using snd (pretty_node pretty_expression)) fmt (f, x)
  | InfixApp (left, op, right) -> Fmt.concat [
      Fmt.using (fun (x, _, _) -> x) (pretty_node pretty_expression);
      Fmt.using (fun (_, y, _) -> y) @@ pretty_node pretty_idx;  
      Fmt.using (fun (_, _, z) -> z) (pretty_node pretty_expression) 
  ] fmt (left, op, right)
  | _ -> assert false 
let pretty_type : Ast_core.typ Fmt.t = assert false 
let pretty_pattern : Ast_core.pat Fmt.t = assert false
*)
