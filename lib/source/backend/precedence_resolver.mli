(** Precedence Resolver - Interface for operator precedence resolution *)

(** Operator associativity *)
type assoc = Left | Right

(** Resolved expression structure after precedence resolution *)
type resolved_exp =
  | ResolvedSingle of Ast.expression
  | ResolvedApp of resolved_exp * Ast.expression Ast.node list
  | ResolvedInfix of resolved_exp * Ast.idx Ast.node * resolved_exp

(** Resolved pattern structure after precedence resolution *)
type resolved_pat =
  | ResolvedPatSingle of Ast.pat
  | ResolvedPatApp of resolved_pat * Ast.pat Ast.node list
  | ResolvedPatInfix of resolved_pat * Ast.idx Ast.node * resolved_pat

val resolve_precedence : Ast.expression Ast.node list -> resolved_exp
(** Resolve precedence in a flat expression sequence *)

val resolve_pat_precedence : Ast.pat Ast.node list -> resolved_pat
(** Resolve precedence in a flat pattern sequence *)
