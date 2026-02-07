let folded (f : 'a -> 'b -> 'a) (lst : 'a) (init : 'b list) : 'a =
  List.fold_left f lst init

(* Fold a list of specs into a SpecSeq chain (right-associative) *)
let fold_spec_list (specs : Ast.specification list) (empty : Ast.specification)
    : Ast.specification =
  match List.rev specs with
  | [] -> empty
  | [ x ] -> x
  | last :: rest ->
      List.fold_left
        (fun acc s -> Ast.SpecSeq (Ast.box_node s, Ast.box_node acc))
        last rest

(** {2 Comment Tracking}

    Shared mutable state for tracking comments during lexing/parsing.
    The lexer populates this list as it encounters comments.
    After parsing, the frontend distributes comments to AST nodes based on
    source positions. *)

(** All comments collected so far: (text, start_byte_pos, end_byte_pos) *)
let comment_store : (string * int * int) list ref = ref []

(** Record a comment found by the lexer *)
let add_comment (text : string) (start_pos : int) (end_pos : int) : unit =
  comment_store := (text, start_pos, end_pos) :: !comment_store

(** Drain all pending comments with position info, sorted by position.
    Used by the frontend post-parse pass to distribute comments to AST nodes. *)
let drain_comments_with_positions () : (string * int * int) list =
  let all = !comment_store in
  comment_store := [];
  List.sort (fun (_, s1, _) (_, s2, _) -> compare s1 s2) all

(** Reset comment store (call before parsing a new file) *)
let reset_comments () : unit =
  comment_store := []
