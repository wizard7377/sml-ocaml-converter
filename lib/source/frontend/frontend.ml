type 'a grammar = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a

let expression_grammar : Ast.expression grammar = Parser.expression_top
let pat_grammar : Ast.pat grammar = Parser.pat_top
let typ_grammar : Ast.typ grammar = Parser.typ_top
let main_grammar : Ast.prog grammar = fun f b -> fst @@ Parser.main f b

(** Collect all positioned nodes from a prog tree in source order.
    Returns a flat list of mutable ref cells pointing to nodes that
    can receive comments, along with their start positions. *)

(** Distribute comments into the prog AST.
    Comments are attached to the declaration-level nodes within SeqDec,
    or to prog-level nodes for functor/signature declarations.
    Each comment goes to the construct whose start position is >= the comment's end. *)
let distribute_comments (prog : Ast.prog) (comments : (string * int * int) list) : Ast.prog =
  if comments = [] then prog
  else
    let sorted_comments = List.sort (fun (_, s1, _) (_, s2, _) -> compare s1 s2) comments in
    (* Recursively distribute comments through the prog tree.
       We pass along a mutable reference to remaining comments.
       At each leaf node, we consume comments that precede it. *)
    let remaining = ref sorted_comments in
    (* Consume comments whose end position <= the given start position *)
    let take_before (start_pos : int) : string list =
      let before, after = List.partition (fun (_, _, ce) -> ce <= start_pos) !remaining in
      remaining := after;
      List.map (fun (s, _, _) -> s) before
    in
    let node_start : type a. a Ast.node -> int = fun n ->
      match n.pos with
      | Some (sp, _) -> sp.pos_cnum
      | None -> max_int
    in
    let add_comments : type a. a Ast.node -> string list -> a Ast.node = fun n cs ->
      if cs = [] then n
      else { n with comments = cs @ n.comments }
    in
    (* Process declaration nodes within SeqDec to distribute comments *)
    let rec process_dec_nodes (nodes : Ast.declaration Ast.node list) : Ast.declaration Ast.node list =
      List.map (fun (n : Ast.declaration Ast.node) ->
        let cs = take_before (node_start n) in
        let n = add_comments n cs in
        (* Recurse into SeqDec *)
        match n.value with
        | Ast.SeqDec inner ->
          { n with value = Ast.SeqDec (process_dec_nodes inner) }
        | _ -> n
      ) nodes
    in
    let rec process_prog (p : Ast.prog) : Ast.prog =
      match p with
      | Ast.ProgSeq (left, right) ->
        let left' = process_prog_node left in
        let right' = process_prog_node right in
        Ast.ProgSeq (left', right')
      | Ast.ProgDec dec_node ->
        let cs = take_before (node_start dec_node) in
        let dec_node = add_comments dec_node cs in
        let dec_node = match dec_node.value with
          | Ast.SeqDec inner ->
            { dec_node with value = Ast.SeqDec (process_dec_nodes inner) }
          | _ -> dec_node
        in
        Ast.ProgDec dec_node
      | Ast.ProgFun fb_node ->
        let cs = take_before (node_start fb_node) in
        Ast.ProgFun (add_comments fb_node cs)
      | Ast.ProgStr sb_node ->
        let cs = take_before (node_start sb_node) in
        Ast.ProgStr (add_comments sb_node cs)
      | Ast.ProgEmpty -> p
    and process_prog_node (n : Ast.prog Ast.node) : Ast.prog Ast.node =
      let cs = take_before (node_start n) in
      let n = add_comments n cs in
      { n with value = process_prog n.value }
    in
    let result = process_prog prog in
    (* Any remaining comments become trailing *)
    let trailing = List.map (fun (s, _, _) -> s) !remaining in
    if trailing = [] then result
    else
      let trailing_node = { Ast.value = Ast.ProgEmpty; pos = None; comments = trailing } in
      match result with
      | Ast.ProgEmpty -> Ast.ProgEmpty  (* put comments on ProgEmpty directly *)
      | _ ->
        Ast.ProgSeq (
          { value = result; pos = None; comments = [] },
          trailing_node
        )

let parse_with : grammar:'a grammar -> string -> 'a =
 fun ~grammar s ->
  let lexbuf = Lexing.from_string s in
  Utils.reset_comments ();
  try
    let res = grammar Lexer.token lexbuf in
    res
  with _ ->
    let pos = Lexing.lexeme_start_p lexbuf in
    let line = pos.Lexing.pos_lnum in
    let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    let msg = Printf.sprintf "Parse error at line %d, character %d" line cnum in
    failwith msg

let parse (s : string) : Ast.prog =
  let res = parse_with ~grammar:main_grammar s in
  (* Distribute comments collected during lexing to AST nodes *)
  let comments = Utils.drain_comments_with_positions () in
  distribute_comments res comments

let debug_parse fmt grammar s =
  let ast = parse_with ~grammar s in
  print_endline (fmt ast);
  ()
