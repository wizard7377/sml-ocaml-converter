(** {1 AST Node Wrapper}

    Generic wrapper type for all AST nodes, providing uniform metadata
    attachment (source positions, future extensions).

    All AST constructors wrap their children in this node type, enabling:
    - Source location tracking for error messages
    - Consistent traversal patterns
    - Future extensibility (comments, type annotations, etc.)

    {2 Usage Pattern}

    {[
      (* Construction - always use box_node *)
      let my_node = box_node SomeConstructor

      (* Destruction - use unbox_node or pattern match *)
      match unbox_node my_node with
      | SomeConstructor -> ...
    ]}

    {2 Design Rationale}

    The uniform wrapper pattern enables several capabilities:

    {3 Source Location Tracking}
    Each AST node can preserve its original source position for precise
    error reporting. When the parser creates nodes, it attaches the location
    from the lexer. When constructing AST programmatically (e.g., in tests
    or code generation), nodes have no position ([None]).

    {3 Future Extensibility}
    The wrapper allows adding metadata without changing all AST types:
    - Type annotations (for type checker integration)
    - Doc comments (for documentation generation)
    - Custom attributes (for compiler extensions)

    {3 Uniform Traversal}
    Generic tree-walking functions can operate on any ['a node] without
    knowing the specific AST type, enabling reusable traversal libraries.
*)

type 'a node = {
  value : 'a;
      (** The wrapped AST payload.

          This is the actual AST constructor value. Pattern match on this
          field to examine the AST structure. *)
  pos : (Lexing.position * Lexing.position) option [@opaque]
      (** Optional source location (start position, end position).

          Set by the parser to track where this AST node originated
          in the source file. Used for error reporting with precise
          line and column information.

          {3 Position Structure}

          When present, contains a tuple [(start_pos, end_pos)] where:
          - [start_pos] - Position of first character
          - [end_pos] - Position after last character

          Each [Lexing.position] contains:
          - [pos_fname] - Source filename
          - [pos_lnum] - Line number (1-indexed)
          - [pos_bol] - Byte offset of line start
          - [pos_cnum] - Byte offset of token

          {3 Usage}

          {[
            (* Check if node has source position *)
            match node.pos with
            | Some (start, end_) ->
                Printf.printf "Line %d, columns %d-%d\n"
                  start.pos_lnum
                  (start.pos_cnum - start.pos_bol)
                  (end_.pos_cnum - end_.pos_bol)
            | None ->
                Printf.printf "Programmatically created node\n"
          ]}

          {3 Opaque Attribute}

          The [[@opaque]] attribute prevents [ppx_deriving.show] from
          displaying position details in pretty-printed output. This
          keeps debug output concise and focused on AST structure
          rather than source locations. *)
}
[@@deriving show]

let box_node (v : 'a) : 'a node =
  { value = v; pos = None }
  (** Create a node wrapper with no source position.

      Use this when constructing AST nodes programmatically (not from parser).
      Nodes created this way have [pos = None], indicating they don't
      correspond to specific source code locations.

      {3 Example Usage}

      {[
        (* Simple constant *)
        let int_const = box_node (ConInt (box_node "42"))

        (* Nested structure *)
        let add_expr =
          box_node (InfixApp (
            box_node (ExpCon (box_node (ConInt (box_node "1")))),
            box_node (IdxIdx (box_node "+")),
            box_node (ExpCon (box_node (ConInt (box_node "2"))))
          ))

        (* Pattern matching *)
        let nil_pattern = box_node (PatIdx (
          box_node (WithoutOp (box_node (IdxIdx (box_node "nil"))))
        ))
      ]}

      {3 When to Use}

      - Constructing AST in tests
      - Generating code programmatically
      - AST transformations and rewriting
      - Creating AST templates

      {3 When Not to Use}

      - In the parser (use node creation with positions)
      - When preserving source locations is important

      @param v The AST value to wrap
      @return A node containing [v] with no position info *)

let unbox_node (n : 'a node) : 'a =
  n.value
  (** Extract the value from a node wrapper, discarding metadata.

      Use when pattern matching on AST contents. This is the primary
      way to destructure AST nodes and access their constructors.

      {3 Example Usage}

      {[
        (* Simple extraction *)
        match unbox_node id_node with
        | IdxIdx s -> Printf.printf "Simple identifier: %s\n" (unbox_node s)
        | IdxLong parts -> Printf.printf "Qualified identifier\n"
        | _ -> ()

        (* Nested pattern matching *)
        match unbox_node expr_node with
        | ExpCon const ->
            (match unbox_node const with
             | ConInt i -> Printf.printf "Integer: %s\n" (unbox_node i)
             | ConString s -> Printf.printf "String: %s\n" (unbox_node s)
             | _ -> ())
        | ExpIdx id -> Printf.printf "Identifier expression\n"
        | _ -> ()

        (* In function definitions *)
        let rec collect_identifiers expr =
          match unbox_node expr with
          | ExpIdx id -> [id]
          | ExpApp (e1, e2) ->
              collect_identifiers e1 @ collect_identifiers e2
          | _ -> []
      ]}

      {3 Pattern}

      The typical pattern when processing AST nodes:
      1. Use [unbox_node] to extract the constructor
      2. Pattern match on the constructor
      3. Recursively process child nodes (which are also wrapped)
      4. Build new nodes using [box_node] if transforming

      {3 Note on Metadata Loss}

      This function discards source position information. If you need
      to preserve or examine positions, access [n.pos] directly instead
      of using this function.

      @param n The node to unwrap
      @return The underlying AST value (constructor) *)
