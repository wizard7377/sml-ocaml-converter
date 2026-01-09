(** Abstract Syntax Tree for Standard ML

    This module defines the complete abstract syntax tree for Standard ML ('97 revision),
    including all core language constructs and the module language (structures, signatures,
    and functors). The grammar follows the Definition of Standard ML, including derived
    forms from Appendix A.

    {2 Design Overview}

    The AST uses a uniform wrapper pattern via {!type:'a node} to attach metadata
    (such as source comments) to any AST node. All constructors wrap their sub-components
    in this node type, enabling consistent traversal and transformation.

    {2 Usage Example}

    Constructing a simple SML expression [let val x = 42 in x end]:
    {[
      let open Ast in
      let x_id = box_node (IdxIdx (box_node "x")) in
      let forty_two = box_node (ExpCon (box_node (ConInt (box_node "42")))) in
      let value_binding = box_node (ValBind (box_node (PatIdx (box_node (WithoutOp x_id))), forty_two, None)) in
      let val_dec = box_node (ValDec ([], value_binding)) in
      let body = box_node (ExpIdx x_id) in
      LetExp ([val_dec], [body])
    ]}

    @see <http://mitpress.mit.edu/books/definition-standard-ml> The Definition of Standard ML
    @see <https://smlfamily.github.io/sml97-defn.pdf> SML '97 Definition (PDF) *)

(** {1 Node Wrapper}

    All AST nodes are wrapped in a generic container that preserves metadata.
    This uniform structure enables:
    - Attaching source comments to any AST element
    - Consistent traversal patterns across the entire tree
    - Future extensibility for source locations, annotations, etc.

    {b Important}: When constructing AST nodes, always use {!box_node} to wrap values.
    When destructuring, use {!unbox_node} to extract the payload. *)

(** Generic wrapper for AST nodes.

    The ['a node] type wraps any AST payload with associated metadata.
    Currently tracks source comments, but the design allows for future
    extensions such as source locations or type annotations.

    {3 Example}
    {[
      (* Wrap a simple identifier *)
      let id_node : idx node = box_node (IdxIdx (box_node "myVar"))

      (* Wrap with explicit comments *)
      let commented_node = { value = IdxIdx (box_node "x"); comments = ["(* bound variable *)"] }

      (* Extract the underlying value *)
      let id_value : idx = unbox_node id_node
    ]}

    @see 'box_node' Helper to create nodes without comments
    @see 'unbox_node' Helper to extract the wrapped value *)
type 'a node = {
    value : 'a ;
        (** The wrapped AST payload. *)
    comments : string list [@opaque];
        (** Source comments associated with this node, preserved from parsing. *)
} [@@deriving show]



(** Create a node wrapper with no associated comments.

    This is the standard way to construct AST nodes programmatically.
    For nodes parsed from source code, the parser may populate the
    [comments] field directly.

    {3 Example}
    {[
      (* Wrapping a constant *)
      let int_const : constant node = box_node (ConInt (box_node "42"))

      (* Wrapping an identifier *)
      let var_id : idx node = box_node (IdxIdx (box_node "x"))

      (* Wrapping an expression *)
      let exp_node : expression node = box_node (ExpCon int_const)
    ]}

    @param v The value to wrap
    @return A node containing [v] with an empty comment list *)
let box_node (v : 'a) : 'a node = { value = v ; comments = [] }

(** Extract the value from a node wrapper, discarding metadata.

    Use this when you need to pattern match on or process the
    underlying AST value without concern for attached comments.

    {3 Example}
    {[
      let process_identifier (id_node : idx node) : string =
        match unbox_node id_node with
        | IdxIdx s -> unbox_node s
        | IdxVar s -> "'" ^ unbox_node s
        | IdxLong ids -> String.concat "." (List.map (fun n -> ...) ids)
        | IdxLab s -> unbox_node s
        | IdxNum s -> unbox_node s
    ]}

    @param n The node to unwrap
    @return The underlying value of type ['a] *)
let unbox_node (n : 'a node) : 'a = n.value

(** {1 Programs}

    Programs are the top-level syntactic category in SML. A program consists of
    core declarations, functor declarations, signature declarations, or sequences thereof.

    A typical SML source file contains a sequence of declarations:
    {[
      (* SML source *)
      structure MyLib = struct ... end
      signature MY_SIG = sig ... end
      functor MkLib(X : MY_SIG) = struct ... end
    ]}

    Which would be represented as:
    {[
      ProgSeq (
        box_node (ProgDec (box_node (StrDec ...))),
        box_node (ProgSeq (
          box_node (ProgStr (box_node (SignBind ...))),
          box_node (ProgFun (box_node (FctBind ...)))
        ))
      )
    ]} *)

(** Top-level program constructs.

    {[
    prog ::= declaration                           (* core declaration *)
           | functor fctbind               (* functor declaration *)
           | signature sigbind             (* signature declaration *)
           |                               (* empty *)
           | prog1 [;] prog2               (* sequence *)
    ]}

    @see 'declaration' Core language declarations
    @see 'functor_binding' Functor bindings
    @see 'signature_binding' Signature bindings *)
type prog =
  | ProgDec of declaration node
      (** Core declaration at the top level.

          Example: [val x = 42] or [structure S = struct end]
          {[
            ProgDec (box_node (ValDec ([], box_node (ValBind (...)))))
          ]} *)
  | ProgFun of functor_binding node
      (** Functor declaration: [functor fctbind].

          Example: [functor F(X : S) = struct end]
          @see 'functor_binding' Functor binding details *)
  | ProgStr of signature_binding node
      (** Signature declaration: [signature sigbind].

          Example: [signature S = sig type t end]
          @see 'signature_binding' Signature binding details *)
  | ProgSeq of prog node * prog node
      (** Sequence of programs: [prog1 ; prog2].

          Programs are evaluated left-to-right. The semicolon
          between top-level declarations is optional in SML.
          {[
            (* val x = 1; val y = 2 *)
            ProgSeq (
              box_node (ProgDec (box_node (ValDec ...))),
              box_node (ProgDec (box_node (ValDec ...)))
            )
          ]} *)
  | ProgEmpty
      (** Empty program.

          Represents an empty source file or the base case
          when building program sequences. *)
    [@@deriving show]

(** Functor bindings.

    {[
    fctbind ::= id1 ( id2 : sig ) [:[:>] sig] = structure [and fctbind]    (* plain *)
              | id ( specification ) [:[:>] sig] = structure [and fctbind]          (* opened *)
    ]}

    Functors are parameterized modules that take a structure matching a signature
    and produce a new structure. They enable generic programming over module structures.

    {3 Example: Plain Functor}
    SML: [functor MkSet(Ord : ORDERED) : SET = struct ... end]
    {[
      FctBind (
        box_node (IdxIdx (box_node "MkSet")),       (* functor name *)
        box_node (IdxIdx (box_node "Ord")),         (* parameter name *)
        box_node (SignIdx (box_node (IdxIdx ...))), (* ORDERED *)
        Some (box_node Transparent, box_node (SignIdx ...)), (* : SET *)
        box_node (StructStr ...),                   (* body *)
        None                                        (* no more bindings *)
      )
    ]}

    @see 'signature' Signature types for parameter and result constraints
    @see 'structure' Structure expressions for functor bodies *)
and functor_binding =
  | FctBind of idx node * idx node * signature node * (anotate node * signature node) option * structure node * functor_binding node option
      (** Plain functor: [id1 ( id2 : sig ) [:[:>] sig] = structure].

          Components:
          - Functor name
          - Parameter name
          - Parameter signature constraint
          - Optional result annotation (transparent [:] or opaque [:>])
          - Body structure expression
          - Optional additional bindings ([and ...])

          @see 'anotate' Transparent vs opaque sealing *)
  | FctBindOpen of idx node * specification node * (anotate node * signature node) option * structure node * functor_binding node option
      (** Opened functor: [id ( specification ) [:[:>] sig] = structure].

          The specification components are directly visible in the functor body
          without requiring qualification through a parameter name. This is
          syntactic sugar for a functor with an anonymous structure parameter.

          Example: [functor F(type t val x : t) = struct ... end]
          The [t] and [x] are directly accessible in the body. *)

(** Signature bindings.

    {[
    sigbind ::= id = sig [and sigbind]
    ]}

    Binds identifiers to signatures for later use in structure annotations
    and functor parameter constraints.

    {3 Example}
    SML: [signature ORDERED = sig type t val compare : t * t -> int end]
    {[
      SignBind (
        box_node (IdxIdx (box_node "ORDERED")),
        box_node (SignSig (...)),
        None
      )
    ]} *)
and signature_binding =
  | SignBind of idx node * signature node * signature_binding node option
      (** Signature binding: [id = sig].

          Components:
          - Signature name (conventionally UPPER_CASE)
          - Signature expression
          - Optional additional bindings ([and ...]) *)

(** {1 Core Language}

    The core language includes constants, identifiers, expressions, patterns,
    types, and declarations. These form the computational heart of SML programs.

    The core language is stratified into:
    - {!constant} - Literal constants (integers, strings, etc.)
    - {!idx} - Identifiers and qualified names
    - {!expression} - Expressions (computation)
    - {!pat} - Patterns (destructuring)
    - {!typ} - Type expressions
    - {!declaration} - Declarations (bindings) *)

(** {2 Constants}

    {[
    constant ::= int       (* integer *)
          | word      (* word *)
          | float     (* floating point *)
          | char      (* character *)
          | string    (* string *)
    ]}

    Integer constants may be decimal or hexadecimal (prefixed with [0x]),
    and may have an optional negation prefix [~].
    Word constants are unsigned and prefixed with [0w] (decimal) or [0wx] (hex).
    Character constants are written as [#"c"].
    String constants are enclosed in double quotes.

    {3 Construction Examples}
    {[
      (* Integer: 42 *)
      let int_42 = box_node (ConInt (box_node "42"))

      (* Negative integer: ~5 *)
      let neg_5 = box_node (ConInt (box_node "~5"))

      (* Hexadecimal: 0xFF *)
      let hex_ff = box_node (ConInt (box_node "0xFF"))

      (* Word: 0w42 *)
      let word_42 = box_node (ConWord (box_node "0w42"))

      (* Float: 3.14 *)
      let pi = box_node (ConFloat (box_node "3.14"))

      (* Character: #"a" *)
      let char_a = box_node (ConChar (box_node "a"))

      (* String: "hello" *)
      let hello = box_node (ConString (box_node "hello"))
    ]} *)
and constant =
  | ConInt of string node
      (** Integer constant.

          Decimal: [\[~\]num], hexadecimal: [\[~\]0xhex].
          The string preserves the original source representation.

          Examples: ["42"], ["~100"], ["0xFF"], ["~0x1A"] *)
  | ConWord of string node
      (** Word (unsigned integer) constant.

          Decimal: [0wnum], hexadecimal: [0wxhex].
          Words are used for bit manipulation and unsigned arithmetic.

          Examples: ["0w255"], ["0wx1F"] *)
  | ConFloat of string node
      (** Floating point constant.

          Format: [\[~\]num.num] or [\[~\]num\[.num\]e\[~\]num].
          Supports scientific notation.

          Examples: ["3.14"], ["~2.0"], ["1.0e10"], ["6.022e~23"] *)
  | ConChar of string node
      (** Character constant.

          SML syntax: [#"ascii"]. The stored string contains just
          the character without the [#"..."] delimiters.

          Examples: ["a"], ["\\n"], ["\\t"] *)
  | ConString of string node
      (** String constant.

          SML syntax: ["ascii*"]. The stored string contains the
          content without surrounding quotes, with escapes preserved.

          Examples: ["hello"], ["line1\\nline2"] *)

(** {2 Identifiers}

    {[
    id     ::= letter (letter | digit | ' | _)*     (* alphanumeric *)
             | (! | % | & | $ | # | + | - | ...)+   (* symbolic *)
    var    ::= 'letter (letter | digit | ' | _)*    (* unconstrained type variable *)
             | ''letter (letter | digit | ' | _)*   (* equality type variable *)
    longid ::= id1.....idn                          (* qualified, n >= 1 *)
    lab    ::= id                                   (* identifier label *)
             | num                                  (* numeric label, may not start with 0 *)
    ]}

    Identifiers can be alphanumeric or symbolic. Type variables are prefixed with
    a single quote ([']) or double quote (['']) for equality type variables.
    Long identifiers are dot-separated qualified names.

    {3 Construction Examples}
    {[
      (* Simple identifier: x *)
      let x = box_node (IdxIdx (box_node "x"))

      (* Symbolic identifier: ++ *)
      let plus_plus = box_node (IdxIdx (box_node "++"))

      (* Type variable: 'a *)
      let alpha = box_node (IdxVar (box_node "a"))

      (* Equality type variable: ''a *)
      let eq_alpha = box_node (IdxVar (box_node "'a"))

      (* Long identifier: List.map *)
      let list_map = box_node (IdxLong [
        box_node (IdxIdx (box_node "List"));
        box_node (IdxIdx (box_node "map"))
      ])

      (* Numeric label for tuples: #1 *)
      let label_1 = box_node (IdxNum (box_node "1"))
    ]}

    @see 'expression' Expressions that use identifiers
    @see 'pat' Patterns that bind identifiers *)
and idx =
  | IdxIdx of string node
      (** Simple alphanumeric or symbolic identifier.

          Alphanumeric: starts with letter, contains letters/digits/[_]/['].
          Symbolic: sequence of [!%&$#+-/:<=>?@\\~'^|*].

          Examples: ["x"], ["myVar"], ["++"], ["<>"], ["::"] *)
  | IdxVar of string node
      (** Type variable: ['var] or [''var] for equality type variables.

          The stored string excludes the leading quote(s).
          Single quote ['] = unconstrained, double [''] = admits equality.

          Examples: ["a"] for ['a], ["'a"] for [''a] *)
  | IdxLong of idx node list
      (** Long (qualified) identifier: [id1.id2...idn].

          Used for accessing module members. Each component is
          typically an {!IdxIdx}. The list has at least one element.

          Example: [List.map] -> [[IdxIdx "List"; IdxIdx "map"]] *)
  | IdxLab of string node
      (** Record label (alphanumeric identifier).

          Used in record expressions and patterns. Syntactically
          identical to regular identifiers but in label context.

          Example: In [{x = 1, y = 2}], ["x"] and ["y"] are labels. *)
  | IdxNum of string node
      (** Numeric label for tuple access (1-indexed, may not start with 0).

          Used with the [#] selector for tuple component access.
          SML tuples are sugar for records with numeric labels.

          Example: [#2 (1, "a", true)] accesses the second element. *)

(** {2 Expressions}

    {[
    expression ::= constant                                    (* constant *)
          | [op] longid                            (* value or constructor identifier *)
          | exp1 exp2                              (* application *)
          | exp1 id exp2                           (* infix application *)
          | ( expression )                                (* parentheses *)
          | ( exp1 , ... , expn )                  (* tuple, n != 1 *)
          | { [exprow] }                           (* record *)
          | # lab                                  (* record selector *)
          | [ exp1 , ... , expn ]                  (* list, n >= 0 *)
          | ( exp1 ; ... ; expn )                  (* sequence, n >= 2 *)
          | let declaration in exp1 ; ... ; expn end       (* local declaration, n >= 1 *)
          | expression : typ                              (* type annotation *)
          | raise expression                              (* exception raising *)
          | expression handle match                       (* exception handling *)
          | exp1 andalso exp2                      (* conjunction *)
          | exp1 orelse exp2                       (* disjunction *)
          | if exp1 then exp2 else exp3            (* conditional *)
          | while exp1 do exp2                     (* iteration *)
          | case expression of match                      (* case analysis *)
          | fn match                               (* function *)
    ]}

    Expressions are the computational core of SML. They evaluate to values
    and can have side effects (via references, I/O, or exceptions).

    @see 'pat' Patterns used in function arguments and case branches
    @see 'matching' Match clauses for case/handle/fn
    @see 'declaration' Declarations used in let expressions *)
and expression =
  | ExpCon of constant node
      (** Constant expression.

          {[
            (* SML: 42 *)
            ExpCon (box_node (ConInt (box_node "42")))

            (* SML: "hello" *)
            ExpCon (box_node (ConString (box_node "hello")))
          ]}

          @see 'constant' Constant types *)
  | ExpIdx of idx node
      (** Value or constructor identifier, optionally prefixed with [op].

          {[
            (* SML: x *)
            ExpIdx (box_node (IdxIdx (box_node "x")))

            (* SML: List.map *)
            ExpIdx (box_node (IdxLong [...]))

            (* SML: op + (to use infix as prefix) *)
            ExpIdx (box_node (IdxIdx (box_node "+")))
          ]} *)
  | ExpApp of expression node * expression node
      (** Function application: [exp1 exp2]. Left-associative.

          {[
            (* SML: f x *)
            ExpApp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "f")))),
              box_node (ExpIdx (box_node (IdxIdx (box_node "x"))))
            )

            (* SML: f x y = (f x) y *)
            ExpApp (
              box_node (ExpApp (f_node, x_node)),
              y_node
            )
          ]} *)
  | InfixApp of expression node * idx node * expression node
      (** Infix operator application: [exp1 id exp2].

          The operator [id] must be declared infix. The parser resolves
          precedence and associativity based on fixity declarations.

          {[
            (* SML: 1 + 2 *)
            InfixApp (
              box_node (ExpCon (box_node (ConInt (box_node "1")))),
              box_node (IdxIdx (box_node "+")),
              box_node (ExpCon (box_node (ConInt (box_node "2"))))
            )

            (* SML: x :: xs *)
            InfixApp (x_node, box_node (IdxIdx (box_node "::")), xs_node)
          ]} *)
  | ParenExp of expression node
      (** Parenthesized expression: [( expression )].

          Preserved in the AST for pretty-printing fidelity.
          Semantically equivalent to the inner expression. *)
  | TupleExp of expression node list
      (** Tuple expression: [( exp1 , ... , expn )] where n != 1.

          The unit value [()] is represented as an empty list.
          Single-element "tuples" are just parenthesized expressions.

          {[
            (* SML: () *)
            TupleExp []

            (* SML: (1, "a", true) *)
            TupleExp [int_1; str_a; bool_true]
          ]} *)
  | RecordExp of row node list
      (** Record expression: [{ exprow }].

          {[
            (* SML: {x = 1, y = 2} *)
            RecordExp [
              box_node (Row (box_node (IdxLab (box_node "x")), int_1, None));
              box_node (Row (box_node (IdxLab (box_node "y")), int_2, None))
            ]
          ]}

          @see 'row' Record row structure *)
  | RecordSelector of idx node
      (** Record field selector: [# lab].

          Creates a function that extracts a field from a record.
          Equivalent to [fn {lab, ...} => lab].

          {[
            (* SML: #name *)
            RecordSelector (box_node (IdxLab (box_node "name")))

            (* SML: #1 (for tuple access) *)
            RecordSelector (box_node (IdxNum (box_node "1")))
          ]} *)
  | ListExp of expression node list
      (** List expression: [\[ exp1 , ... , expn \]] where n >= 0.

          Syntactic sugar for nested cons cells.
          [\[1,2,3\]] = [1 :: 2 :: 3 :: nil].

          {[
            (* SML: [] *)
            ListExp []

            (* SML: [1, 2, 3] *)
            ListExp [int_1; int_2; int_3]
          ]} *)
  | SeqExp of expression node list
      (** Sequential expression: [( exp1 ; ... ; expn )] where n >= 2.

          Evaluates expressions left-to-right, returning the last value.
          Earlier expressions are evaluated for side effects only.

          {[
            (* SML: (print "hi"; 42) *)
            SeqExp [print_hi; int_42]
          ]} *)
  | LetExp of declaration node list * expression node list
      (** Local declaration: [let declaration in exp1 ; ... ; expn end].

          Declarations are visible only within the body expressions.
          Multiple body expressions form an implicit sequence.

          {[
            (* SML: let val x = 1 in x + x end *)
            LetExp (
              [box_node (ValDec ([], val_bind_x))],
              [box_node (InfixApp (x, plus, x))]
            )
          ]}

          @see 'declaration' Declaration types *)
  | TypedExp of expression node * typ node
      (** Type-annotated expression: [expression : typ].

          Constrains the type of the expression. Useful for
          resolving overloading or documenting intent.

          {[
            (* SML: ([] : int list) *)
            TypedExp (box_node (ListExp []), box_node (TypCon ([int_typ], list_id)))
          ]} *)
  | RaiseExp of expression node
      (** Exception raising: [raise expression].

          The expression must evaluate to an exception value
          (a value of type [exn]).

          {[
            (* SML: raise Fail "oops" *)
            RaiseExp (box_node (ExpApp (fail_con, str_oops)))
          ]}

          @see 'HandleExp' Exception handling *)
  | HandleExp of expression node * matching node
      (** Exception handling: [expression handle match].

          If [expression] raises an exception, it is matched against [match].
          If no pattern matches, the exception propagates.

          {[
            (* SML: f x handle Fail s => (print s; 0) *)
            HandleExp (
              box_node (ExpApp (f, x)),
              box_node (Case (fail_pat, handler_exp, None))
            )
          ]}

          @see 'matching' Match clause structure *)
  | AndExp of expression node * expression node
      (** Short-circuit conjunction: [exp1 andalso exp2].

          [exp2] is evaluated only if [exp1] is [true].
          Equivalent to [if exp1 then exp2 else false].

          {[
            (* SML: x > 0 andalso y > 0 *)
            AndExp (x_positive, y_positive)
          ]} *)
  | OrExp of expression node * expression node
      (** Short-circuit disjunction: [exp1 orelse exp2].

          [exp2] is evaluated only if [exp1] is [false].
          Equivalent to [if exp1 then true else exp2].

          {[
            (* SML: x < 0 orelse x > 10 *)
            OrExp (x_negative, x_large)
          ]} *)
  | IfExp of expression node * expression node * expression node
      (** Conditional: [if exp1 then exp2 else exp3].

          Both branches must have the same type.

          {[
            (* SML: if n = 0 then 1 else n * fact(n-1) *)
            IfExp (n_is_zero, int_1, n_times_fact)
          ]} *)
  | WhileExp of expression node * expression node
      (** While loop: [while exp1 do exp2].

          Repeatedly evaluates [exp2] while [exp1] is [true].
          Returns unit. Relies on side effects for usefulness.

          {[
            (* SML: while !r > 0 do r := !r - 1 *)
            WhileExp (r_positive, decr_r)
          ]} *)
  | CaseExp of expression node * matching node
      (** Case analysis: [case expression of match].

          Pattern matches [expression] against the cases in [match].
          The first matching pattern's expression is evaluated.

          {[
            (* SML: case xs of [] => 0 | x::_ => x *)
            CaseExp (
              xs_node,
              box_node (Case (nil_pat, int_0,
                Some (box_node (Case (cons_pat, x_node, None)))))
            )
          ]}

          @see 'matching' Match clause structure
          @see 'pat' Pattern syntax *)
  | FnExp of matching node
      (** Anonymous function: [fn match].

          Creates a function value from a match. Multi-argument
          functions use nested [fn] expressions or tuple patterns.

          {[
            (* SML: fn x => x + 1 *)
            FnExp (box_node (Case (x_pat, x_plus_1, None)))

            (* SML: fn [] => 0 | _::xs => 1 + length xs *)
            FnExp (box_node (Case (nil_pat, int_0,
              Some (box_node (Case (cons_pat, one_plus_length_xs, None))))))
          ]}

          @see 'matching' Match clause structure *)

(** {2 Expression Rows}

    {[
    exprow ::= lab = expression [, exprow]
    ]}

    Record rows bind labels to expressions in record literals.

    @see 'RecordExp' Record expression using rows *)
and row =
  | Row of idx node * expression node * row node option
      (** Expression row: [lab = expression].

          The optional third component allows chaining for parser
          convenience, though {!RecordExp} stores rows as a list.

          {[
            (* SML: x = 1 (in a record) *)
            Row (
              box_node (IdxLab (box_node "x")),
              box_node (ExpCon (box_node (ConInt (box_node "1")))),
              None
            )
          ]} *)

(** {2 Match Clauses}

    {[
    match ::= pat => expression [| match]
    ]}

    Matches are used in {!CaseExp}, {!HandleExp}, and {!FnExp} expressions.
    Each clause binds pattern variables in the corresponding expression.

    {3 Example}
    {[
      (* SML: 0 => "zero" | _ => "other" *)
      Case (
        box_node (PatCon (box_node (ConInt (box_node "0")))),
        box_node (ExpCon (box_node (ConString (box_node "zero")))),
        Some (box_node (Case (
          box_node PatWildcard,
          box_node (ExpCon (box_node (ConString (box_node "other")))),
          None
        )))
      )
    ]}

    @see 'pat' Pattern syntax
    @see 'expression' Expression syntax *)
and matching =
  | Case of pat node * expression node * matching node option
      (** Match clause: [pat => expression].

          Components:
          - Pattern to match against
          - Expression to evaluate if pattern matches
          - Optional next clause ([| pat2 => exp2])

          Pattern variables bound in [pat] are visible in [expression]. *)

(** {2 Declarations}

    {[
    declaration ::= val [var,] valbind                                  (* value *)
          | fun [var,] funbind                                  (* function *)
          | type typbind                                        (* type *)
          | datatype datbind [withtype typbind]                 (* data type *)
          | datatype id = datatype longid                       (* data type replication *)
          | abstype datbind [withtype typbind] with declaration end     (* abstract type *)
          | exception exnbind                                   (* exception *)
          | structure strbind                                   (* structure, not in expressions *)
          |                                                     (* empty *)
          | dec1 [;] dec2                                       (* sequence *)
          | local dec1 in dec2 end                              (* local *)
          | open longid1 ... longidn                            (* inclusion, n >= 1 *)
          | nonfix id1 ... idn                                  (* nonfix, n >= 1 *)
          | infix [digit] id1 ... idn                           (* left-associative infix, n >= 1 *)
          | infixr [digit] id1 ... idn                          (* right-associative infix, n >= 1 *)
    ]}

    Declarations introduce new bindings into scope. They appear at the
    top level, in [let] expressions, and in structures.

    @see 'value_binding' Value binding details
    @see 'function_binding' Function binding details
    @see 'type_binding' Type binding details
    @see 'data_binding' Datatype binding details *)
and declaration =
  | ValDec of idx node list * value_binding node
      (** Value declaration: [val \[var,\] valbind].

          The [idx list] contains explicit type variables for polymorphism.
          These scope over all bindings in the [valbind].

          {[
            (* SML: val x = 42 *)
            ValDec ([], box_node (ValBind (x_pat, int_42, None)))

            (* SML: val 'a id : 'a -> 'a = fn x => x *)
            ValDec ([alpha], box_node (ValBind (id_pat, fn_x_x, None)))
          ]} *)
  | FunDec of function_binding node
      (** Function declaration: [fun \[var,\] funbind].

          Syntactic sugar for recursive function definitions.
          Multiple clauses enable pattern matching on arguments.

          {[
            (* SML: fun fact 0 = 1 | fact n = n * fact(n-1) *)
            FunDec (box_node (FunBind (
              box_node (FunMatchPrefix (...)),
              None
            )))
          ]}

          @see 'function_binding' Function binding structure
          @see 'fun_match' Individual clauses *)
  | TypDec of type_binding node
      (** Type abbreviation: [type typbind].

          Introduces type synonyms without creating new types.

          {[
            (* SML: type 'a pair = 'a * 'a *)
            TypDec (box_node (TypBind ([alpha], pair_id, pair_typ, None)))
          ]}

          @see 'type_binding' Type binding structure *)
  | DatDec of data_binding node * type_binding node option
      (** Datatype declaration: [datatype datbind \[withtype typbind\]].

          Creates new algebraic data types with constructors.
          The optional [withtype] allows mutually recursive type abbreviations.

          {[
            (* SML: datatype 'a tree = Leaf | Node of 'a tree * 'a * 'a tree *)
            DatDec (box_node (DatBind (
              [alpha],
              tree_id,
              box_node (ConBind (leaf_id, None,
                Some (box_node (ConBind (node_id, Some node_typ, None))))),
              None
            )), None)
          ]}

          @see 'data_binding' Datatype binding structure
          @see 'constructor_binding' Constructor binding structure *)
  | DataDecAlias of idx node * idx node
      (** Datatype replication: [datatype id = datatype longid].

          Makes [id] an alias for an existing datatype, including
          all its constructors.

          {[
            (* SML: datatype mybool = datatype bool *)
            DataDecAlias (
              box_node (IdxIdx (box_node "mybool")),
              box_node (IdxIdx (box_node "bool"))
            )
          ]} *)
  | AbstractDec of data_binding node * type_binding node option * declaration node list
      (** Abstract type: [abstype datbind \[withtype typbind\] with declaration end].

          The datatype constructors are hidden outside the [with] block.
          Only the type name and functions defined in [declaration] are visible.

          {[
            (* SML: abstype 'a set = Set of 'a list with val empty = Set [] end *)
            AbstractDec (set_datbind, None, [val_empty_dec])
          ]} *)
  | ExnDec of exn_bind node
      (** Exception declaration: [exception exnbind].

          Creates new exception constructors.

          {[
            (* SML: exception NotFound *)
            ExnDec (box_node (ExnBind (not_found_id, None, None)))

            (* SML: exception Error of string *)
            ExnDec (box_node (ExnBind (error_id, Some string_typ, None)))
          ]}

          @see 'exn_bind' Exception binding structure *)
  | StrDec of structure_binding node
      (** Structure declaration: [structure strbind].

          Not allowed inside [let] expressions (only at top level).

          {[
            (* SML: structure S = struct val x = 1 end *)
            StrDec (box_node (StrBind (s_id, None, None)))
          ]}

          @see 'structure_binding' Structure binding structure
          @see 'structure' Structure expressions *)
  | SeqDec of declaration node list
      (** Sequence of declarations: [dec1 ; dec2 ; ...].

          Declarations are processed left-to-right, with each
          declaration's bindings visible in subsequent ones.

          {[
            (* SML: val x = 1; val y = x + 1 *)
            SeqDec [val_x_dec; val_y_dec]
          ]} *)
  | LocalDec of declaration node * declaration node
      (** Local declaration: [local dec1 in dec2 end].

          [dec1] is visible only within [dec2]. The bindings from
          [dec2] are exported; [dec1] bindings are hidden.

          {[
            (* SML: local val pi = 3.14 in fun area r = pi * r * r end *)
            LocalDec (val_pi_dec, fun_area_dec)
          ]} *)
  | OpenDec of idx node list
      (** Structure inclusion: [open longid1 ... longidn].

          Makes all bindings from the structures directly visible
          without qualification.

          {[
            (* SML: open List Option *)
            OpenDec [list_id; option_id]
          ]} *)
  | FixityDec of fixity node * idx node list
      (** Fixity declaration for operators.

          Specifies infix status and precedence for identifiers.

          {[
            (* SML: infix 6 + - *)
            FixityDec (box_node (Infix (box_node 6)), [plus_id; minus_id])

            (* SML: infixr 5 :: @ *)
            FixityDec (box_node (Infixr (box_node 5)), [cons_id; append_id])
          ]}

          @see 'fixity' Fixity specifications *)

(** Operator fixity specifications.

    {[
    nonfix id1 ... idn                  (* remove infix status *)
    infix [digit] id1 ... idn           (* left-associative, precedence 0-9 *)
    infixr [digit] id1 ... idn          (* right-associative, precedence 0-9 *)
    ]}

    Precedence defaults to 0 if not specified. Higher numbers bind tighter.

    Standard SML precedences:
    - 7: [*], [/], [div], [mod]
    - 6: [+], [-], [^]
    - 5: [::], [@]
    - 4: [=], [<>], [<], [>], [<=], [>=]
    - 3: [:=], [o]
    - 0: [before] *)
and fixity =
  | Nonfix
      (** Remove infix status: [nonfix id].

          The identifier becomes a regular prefix identifier. *)
  | Infix of int node
      (** Left-associative infix: [infix \[n\] id].

          Precedence 0-9, default 0. Left-associative means
          [a op b op c] parses as [(a op b) op c]. *)
  | Infixr of int node
      (** Right-associative infix: [infixr \[n\] id].

          Precedence 0-9, default 0. Right-associative means
          [a op b op c] parses as [a op (b op c)]. *)

(** Value bindings.

    {[
    valbind ::= pat = expression [and valbind]     (* destructuring *)
              | rec valbind                 (* recursive *)
    ]}

    @see 'ValDec' Value declarations using these bindings *)
and value_binding =
  | ValBind of pat node * expression node * value_binding node option
      (** Destructuring binding: [pat = expression].

          Pattern variables are bound to corresponding parts of the value.
          Multiple bindings with [and] are simultaneous (not sequential).

          {[
            (* SML: val (x, y) = (1, 2) *)
            ValBind (tuple_pat_xy, tuple_exp_12, None)

            (* SML: val x = 1 and y = 2 *)
            ValBind (x_pat, int_1, Some (box_node (ValBind (y_pat, int_2, None))))
          ]} *)
  | ValBindRec of value_binding node
      (** Recursive binding: [rec valbind].

          Allows the bound names to be used in the expressions.
          Required for recursive functions defined with [val].

          {[
            (* SML: val rec fact = fn n => if n = 0 then 1 else n * fact(n-1) *)
            ValBindRec (box_node (ValBind (fact_pat, fact_fn, None)))
          ]} *)

(** Function bindings.

    {[
    funbind ::= funmatch [and funbind]      (* clausal function *)
    ]}

    @see 'FunDec' Function declarations
    @see 'fun_match' Individual function clauses *)
and function_binding =
  | FunBind of fun_match node * function_binding node option
      (** Function binding with clauses.

          Additional bindings ([and ...]) define mutually recursive functions.

          {[
            (* SML: fun even 0 = true | even n = odd(n-1)
                    and odd 0 = false | odd n = even(n-1) *)
            FunBind (even_match, Some (box_node (FunBind (odd_match, None))))
          ]} *)

(** Function match clauses.

    {[
    funmatch ::= [op] id pat1 ... patn [: typ] = expression [| funmatch]    (* nonfix, n >= 1 *)
               | pat1 id pat2 [: typ] = expression [| funmatch]             (* infix *)
               | ( pat1 id pat2 ) pat'1 ... pat'n [: typ] = expression [| funmatch]  (* infix, n >= 0 *)
    ]}

    Multiple clauses define pattern matching on function arguments.
    All clauses must use the same function name and arity.

    @see 'pat' Pattern syntax for function parameters *)
and fun_match =
  | FunMatchPrefix of with_op node * pat node list * typ node option * expression node * fun_match node option
      (** Prefix (nonfix) function clause: [\[op\] id pat1 ... patn \[: typ\] = expression].

          The {!with_op} contains the function name (with optional [op] prefix).

          {[
            (* SML: fun length [] = 0 | length (_::xs) = 1 + length xs *)
            FunMatchPrefix (
              box_node (WithoutOp length_id),
              [nil_pat],
              None,
              int_0,
              Some (box_node (FunMatchPrefix (
                box_node (WithoutOp length_id),
                [cons_pat],
                None,
                one_plus_length_xs,
                None
              )))
            )
          ]} *)
  | FunMatchInfix of pat node * idx node * pat node * typ node option * expression node * fun_match node option
      (** Infix function clause: [pat1 id pat2 \[: typ\] = expression].

          The identifier [id] must be declared infix.

          {[
            (* SML: fun x ++ y = x @ y *)
            FunMatchInfix (x_pat, plusplus_id, y_pat, None, append_exp, None)
          ]} *)
  | FunMatchLow of pat node * idx node * pat node * pat node list * typ node option * expression node * fun_match node option
      (** Curried infix clause: [( pat1 id pat2 ) pat'1 ... pat'n \[: typ\] = expression].

          An infix operator with additional curried arguments.

          {[
            (* SML: fun (x ++ y) z = x @ y @ z *)
            FunMatchLow (x_pat, plusplus_id, y_pat, [z_pat], None, concat_exp, None)
          ]} *)

(** Type bindings.

    {[
    typbind ::= [var,] id = typ [and typbind]
    ]}

    Introduces type abbreviations (synonyms).

    @see 'TypDec' Type declarations *)
and type_binding =
  | TypBind of idx node list * idx node * typ node * type_binding node option
      (** Type abbreviation: [\[var,\] id = typ].

          Components:
          - Type parameters (e.g., [['a]] for ['a list])
          - Type name
          - Type definition
          - Optional additional bindings ([and ...])

          {[
            (* SML: type intpair = int * int *)
            TypBind ([], intpair_id, int_times_int, None)

            (* SML: type 'a pair = 'a * 'a *)
            TypBind ([alpha], pair_id, alpha_times_alpha, None)
          ]} *)

(** Datatype bindings.

    {[
    datbind ::= [var,] id = conbind [and datbind]
    ]}

    Introduces new algebraic data types with constructors.

    @see 'DatDec' Datatype declarations
    @see 'constructor_binding' Constructor bindings *)
and data_binding =
  | DatBind of idx node list * idx node * constructor_binding node * data_binding node option
      (** Datatype binding: [\[var,\] id = conbind].

          Components:
          - Type parameters
          - Type name
          - Constructor bindings
          - Optional additional bindings ([and ...] for mutual recursion)

          {[
            (* SML: datatype 'a option = NONE | SOME of 'a *)
            DatBind (
              [alpha],
              option_id,
              box_node (ConBind (none_id, None,
                Some (box_node (ConBind (some_id, Some alpha_typ, None))))),
              None
            )
          ]} *)

(** Constructor bindings.

    {[
    conbind ::= id [of typ] [| conbind]
    ]}

    Defines data constructors for a datatype.

    @see 'data_binding' Datatype bindings using constructors *)
and constructor_binding =
  | ConBind of idx node * typ node option * constructor_binding node option
      (** Constructor: [id \[of typ\]].

          Components:
          - Constructor name (conventionally capitalized)
          - Optional argument type
          - Optional next constructor ([| ...])

          {[
            (* SML: Nil (nullary constructor) *)
            ConBind (nil_id, None, None)

            (* SML: Cons of 'a * 'a list *)
            ConBind (cons_id, Some (TypTuple [alpha; alpha_list]), None)
          ]} *)

(** Exception bindings.

    {[
    exnbind ::= id [of typ] [and exnbind]     (* generative *)
              | id = longid [and exnbind]     (* renaming *)
    ]}

    @see 'ExnDec' Exception declarations *)
and exn_bind =
  | ExnBind of idx node * typ node option * exn_bind node option
      (** Generative exception: [id \[of typ\]].

          Creates a new, unique exception constructor.

          {[
            (* SML: exception Empty *)
            ExnBind (empty_id, None, None)

            (* SML: exception ParseError of string * int *)
            ExnBind (parse_error_id, Some string_int_typ, None)
          ]} *)
  | ExnBindAlias of idx node * idx node * exn_bind node option
      (** Exception renaming: [id = longid].

          Makes [id] an alias for an existing exception.
          Both names refer to the same exception identity.

          {[
            (* SML: exception E = SomeModule.SomeException *)
            ExnBindAlias (e_id, some_module_exn_id, None)
          ]} *)

(** {1 Module Language}

    The module language provides structures (collections of declarations),
    signatures (types of structures), and functors (parameterized structures).

    The module system enables:
    - Namespace management via structures
    - Interface specification via signatures
    - Generic programming via functors
    - Information hiding via opaque signatures

    @see 'structure' Structure expressions
    @see 'signature' Signature expressions
    @see 'functor_binding' Functor definitions *)

(** {2 Structures}

    {[
    structure ::= longid                        (* identifier *)
          | struct declaration end                (* structure *)
          | structure : sig                     (* transparent annotation *)
          | structure :> sig                    (* opaque annotation *)
          | id ( structure )                    (* functor application *)
          | id ( declaration )                    (* functor application *)
          | let declaration in structure end            (* local declaration *)
    ]}

    Structures are the values of the module language. They contain
    types, values, exceptions, and nested structures.

    @see 'declaration' Declarations within structures
    @see 'anotate' Transparent vs opaque sealing *)
and structure =
  | StrIdx of idx node
      (** Structure identifier (possibly qualified).

          {[
            (* SML: List (reference to existing structure) *)
            StrIdx (box_node (IdxIdx (box_node "List")))

            (* SML: Outer.Inner *)
            StrIdx (box_node (IdxLong [outer_id; inner_id]))
          ]} *)
  | StructStr of declaration node
      (** Structure expression: [struct declaration end].

          {[
            (* SML: struct val x = 1 type t = int end *)
            StructStr (box_node (SeqDec [val_x; type_t]))
          ]} *)
  | AnotateStr of idx node * anotate node * structure node
      (** Annotated structure: [structure : sig] or [structure :> sig].

          Note: The first component is the structure name for binding context.

          {[
            (* SML: S : SIG *)
            AnotateStr (s_id, box_node Transparent, sig_node)

            (* SML: S :> SIG (opaque) *)
            AnotateStr (s_id, box_node Opaque, sig_node)
          ]}

          @see 'anotate' Transparent vs opaque semantics *)
  | FunctorApp of idx node * structure node
      (** Functor application: [id ( structure )].

          {[
            (* SML: MkSet(IntOrd) *)
            FunctorApp (mkset_id, box_node (StrIdx intord_id))
          ]} *)
  | FunctorAppAnonymous of idx node * declaration node
      (** Functor application with anonymous argument: [id ( declaration )].

          The declarations form an anonymous structure.

          {[
            (* SML: MkSet(type t = int val compare = Int.compare) *)
            FunctorAppAnonymous (mkset_id, box_node (SeqDec [type_t; val_compare]))
          ]} *)
  | LocalDec of declaration node * structure node
      (** Local declaration in structure: [let declaration in structure end].

          {[
            (* SML: let val helper = ... in struct ... end end *)
            LocalDec (helper_dec, struct_body)
          ]} *)

(** Signature annotations for structures.

    Transparent annotation ([: sig]) preserves type equalities.
    Opaque annotation ([:> sig]) hides type implementations.

    {3 Example}
    {[
      (* Transparent: outside code knows S.t = int *)
      structure S : sig type t val x : t end = struct type t = int val x = 42 end

      (* Opaque: outside code only knows S.t exists *)
      structure S :> sig type t val x : t end = struct type t = int val x = 42 end
    ]} *)
and anotate =
  | Transparent
      (** Transparent annotation: [structure : sig].

          Type equalities are visible outside the structure.
          If [type t = int] inside, then [t = int] is known outside. *)
  | Opaque
      (** Opaque annotation: [structure :> sig].

          Abstract types hide their implementations.
          If [type t = int] inside, outside only knows [type t] exists. *)

(** Structure bindings.

    {[
    strbind ::= id [:[:>] sig] = structure [and strbind]
    ]}

    @see 'StrDec' Structure declarations *)
and structure_binding =
  | StrBind of idx node * (anotate node * signature node) option * structure node * structure_binding node option
      (** Structure binding: [id \[:\[\:>\] sig\] = structure].

          Fields:
          - idx: Structure name
          - option: Optional signature constraint (transparent/opaque)
          - structure: The structure body (right-hand side of =)
          - option: Optional chained structure binding (for 'and') *)

(** {2 Signatures}

    {[
    sig ::= id                                   (* identifier *)
          | sig specification end                         (* signature *)
          | sig where type typrefin              (* refinement *)
    ]}

    Signatures are the types of structures. They specify what
    types and values a structure must provide.

    @see 'specification' Specifications within signatures
    @see 'typ_refine' Type refinements *)
and signature =
  | SignIdx of idx node
      (** Signature identifier.

          {[
            (* SML: ORDERED (reference to signature) *)
            SignIdx (box_node (IdxIdx (box_node "ORDERED")))
          ]} *)
  | SignSig of specification node list
      (** Signature expression: [sig specification end].

          Contains a list of specifications that structures
          matching this signature must fulfill.

          {[
            (* SML: sig val x : int type t end *)
            SignSig [val_x_spec; type_t_spec]
          ]} *)
  | SignWhere of signature node * typ_refine node
      (** Signature with type refinement: [sig where type typrefin].

          {[
            (* SML: ORD where type t = int *)
            SignWhere (
              box_node (SignIdx ord_id),
              box_node (TypRef ([], t_id, int_typ, None))
            )
          ]}

          @see 'typ_refine' Type refinement syntax *)

(** Type refinements in [where type] clauses.

    {[
    typrefin ::= [var,] longid = typ [and type typrefin]
    ]}

    Refines abstract types in a signature to specific types. *)
and typ_refine =
  | TypRef of idx node list * idx node * typ node * (typ node * typ_refine node) option
      (** Type refinement: [\[var,\] longid = typ].

          Components:
          - Type parameters (for parameterized type refinement)
          - Type path being refined
          - Concrete type definition
          - Optional additional refinements ([and type ...])

          {[
            (* SML: where type t = int *)
            TypRef ([], t_id, int_typ, None)

            (* SML: where type 'a container = 'a list *)
            TypRef ([alpha], container_id, alpha_list_typ, None)
          ]} *)

(** {2 Specifications}

    {[
    specification ::= val valdesc                                          (* value *)
           | type typdesc                                         (* type *)
           | eqtype typdesc                                       (* equality type *)
           | type typbind                                         (* type abbreviation *)
           | datatype datdesc                                     (* data type *)
           | datatype id = datatype longid                        (* data type replication *)
           | exception exndesc                                    (* exception *)
           | structure strdesc                                    (* structure *)
           |                                                      (* empty *)
           | spec1 [;] spec2                                      (* sequence *)
           | include sig                                          (* inclusion *)
           | include id1 ... idn                                  (* inclusion, n >= 1 *)
           | specification sharing type longid1 = ... = longidn            (* type sharing, n >= 2 *)
           | specification sharing longid1 = ... = longidn                 (* structure sharing, n >= 2 *)
    ]}

    Specifications describe the required contents of a structure.
    They appear within signature expressions.

    @see 'signature' Signatures containing specifications
    @see 'val_specification' Value descriptions
    @see 'typ_specification' Type descriptions *)
and specification =
  | SpecVal of val_specification node
      (** Value specification: [val valdesc].

          {[
            (* SML: val length : 'a list -> int *)
            SpecVal (box_node (ValDesc (length_id, list_to_int_typ, None)))
          ]} *)
  | SpecTyp of typ_specification node
      (** Abstract type specification: [type typdesc].

          Declares a type without revealing its implementation.

          {[
            (* SML: type t *)
            SpecTyp (box_node (TypDesc ([], t_id, None)))

            (* SML: type 'a container *)
            SpecTyp (box_node (TypDesc ([alpha], container_id, None)))
          ]} *)
  | SpecEqtyp of typ_specification node
      (** Equality type specification: [eqtype typdesc].

          Specifies types that admit equality ([=] and [<>]).

          {[
            (* SML: eqtype key *)
            SpecEqtyp (box_node (TypDesc ([], key_id, None)))
          ]} *)
  | SpecTypBind of type_binding node
      (** Type abbreviation in signature: [type typbind].

          Reveals the type definition, unlike abstract [type].

          {[
            (* SML: type pair = int * int *)
            SpecTypBind (box_node (TypBind ([], pair_id, int_times_int, None)))
          ]} *)
  | SpecDat of dat_specification node
      (** Datatype specification: [datatype datdesc].

          {[
            (* SML: datatype 'a option = NONE | SOME of 'a *)
            SpecDat (box_node (DatDesc ([alpha], option_id, constructors, None)))
          ]}

          @see 'dat_specification' Datatype description structure *)
  | SpecDatAlias of idx node * idx node
      (** Datatype replication: [datatype id = datatype longid]. *)
  | SpecExn of exn_specification node
      (** Exception specification: [exception exndesc].

          {[
            (* SML: exception NotFound *)
            SpecExn (box_node (ExnDesc (not_found_id, None, None)))
          ]}

          @see 'exn_specification' Exception description structure *)
  | SpecStr of str_specification node
      (** Structure specification: [structure strdesc].

          {[
            (* SML: structure Sub : SIG *)
            SpecStr (box_node (StrDesc (sub_id, sig_node, None)))
          ]}

          @see 'str_specification' Structure description structure *)
  | SpecSeq of specification node * specification node
      (** Sequence of specifications: [spec1 ; spec2]. *)
  | SpecInclude of signature node
      (** Include signature: [include sig].

          Incorporates all specifications from another signature.

          {[
            (* SML: include ORDERED *)
            SpecInclude (box_node (SignIdx ordered_id))
          ]} *)
  | SpecIncludeIdx of idx node list
      (** Include multiple signatures: [include id1 ... idn].

          {[
            (* SML: include ORD EQ *)
            SpecIncludeIdx [ord_id; eq_id]
          ]} *)
  | SpecSharingTyp of specification node * idx node list
      (** Type sharing constraint: [specification sharing type longid1 = ... = longidn].

          Asserts that the named types are the same type.

          {[
            (* SML: ... sharing type A.t = B.t *)
            SpecSharingTyp (base_spec, [a_t_id; b_t_id])
          ]} *)
  | SpecSharingStr of specification node * idx node list
      (** Structure sharing constraint: [specification sharing longid1 = ... = longidn].

          Asserts that the named structures share all type components. *)

(** Value descriptions in signatures.

    {[
    valdesc ::= id : typ [and valdesc]
    ]} *)
and val_specification =
  | ValDesc of idx node * typ node * val_specification node option
      (** Value description: [id : typ].

          {[
            (* SML: val f : int -> int *)
            ValDesc (f_id, int_to_int_typ, None)
          ]} *)

(** Type descriptions (abstract types) in signatures.

    {[
    typdesc ::= [var,] id [and typdesc]
    ]} *)
and typ_specification =
  | TypDesc of idx node list * idx node * typ_specification node option
      (** Type description: [\[var,\] id].

          Declares an abstract type with given arity (number of parameters).

          {[
            (* SML: type t (nullary) *)
            TypDesc ([], t_id, None)

            (* SML: type ('a, 'b) map (binary) *)
            TypDesc ([alpha; beta], map_id, None)
          ]} *)

(** Datatype descriptions in signatures.

    {[
    datdesc ::= [var,] id = condesc [and datdesc]
    ]} *)
and dat_specification =
  | DatDesc of idx node list * idx node * con_specification node * dat_specification node option
      (** Datatype description with constructors.

          Like {!data_binding} but in signature context.

          @see 'con_specification' Constructor descriptions *)

(** Constructor descriptions in signatures.

    {[
    condesc ::= id [of typ] [| condesc]
    ]} *)
and con_specification =
  | ConDesc of idx node * typ node option * con_specification node option
      (** Constructor description: [id \[of typ\]].

          Like {!constructor_binding} but in signature context. *)

(** Exception descriptions in signatures.

    {[
    exndesc ::= id [of typ] [and exndesc]
    ]} *)
and exn_specification =
  | ExnDesc of idx node * typ node option * exn_specification node option
      (** Exception description: [id \[of typ\]].

          Like {!exn_bind} but in signature context. *)

(** Structure descriptions in signatures.

    {[
    strdesc ::= id : sig [and strdesc]
    ]} *)
and str_specification =
  | StrDesc of idx node * signature node * str_specification node option
      (** Structure description: [id : sig].

          {[
            (* SML: structure Sub : SIG *)
            StrDesc (sub_id, sig_node, None)
          ]} *)

(** {2 Types}

    {[
    typ ::= var                          (* variable *)
          | [typ,] longid                (* constructor *)
          | ( typ )                      (* parentheses *)
          | typ1 -> typ2                 (* function *)
          | typ1 * ... * typn            (* tuple, n >= 2 *)
          | { [typrow] }                 (* record *)
    ]}

    Type expressions describe the types of values. They appear in
    type annotations, type definitions, and signatures.

    @see 'idx' Type variables (IdxVar)
    @see 'typ_row' Record type rows *)
and typ =
  | TypVar of idx node
      (** Type variable: ['var] or [''var].

          {[
            (* SML: 'a *)
            TypVar (box_node (IdxVar (box_node "a")))

            (* SML: ''a (equality type variable) *)
            TypVar (box_node (IdxVar (box_node "'a")))
          ]} *)
  | TypCon of typ node list * idx node
      (** Type constructor application: [\[typ,\] longid].

          {[
            (* SML: int *)
            TypCon ([], int_id)

            (* SML: int list *)
            TypCon ([int_typ], list_id)

            (* SML: (int, string) either *)
            TypCon ([int_typ; string_typ], either_id)
          ]} *)
  | TypPar of typ node
      (** Parenthesized type: [( typ )].

          Preserved for pretty-printing fidelity. *)
  | TypFun of typ node * typ node
      (** Function type: [typ1 -> typ2]. Right-associative.

          {[
            (* SML: int -> int *)
            TypFun (int_typ, int_typ)

            (* SML: int -> int -> int = int -> (int -> int) *)
            TypFun (int_typ, box_node (TypFun (int_typ, int_typ)))
          ]} *)
  | TypTuple of typ node list
      (** Tuple type: [typ1 * ... * typn] where n >= 2.

          {[
            (* SML: int * string *)
            TypTuple [int_typ; string_typ]

            (* SML: int * int * int *)
            TypTuple [int_typ; int_typ; int_typ]
          ]} *)
  | TypRecord of typ_row node list
      (** Record type: [{ typrow }].

          {[
            (* SML: {x : int, y : int} *)
            TypRecord [
              box_node (TypRow (x_label, int_typ, None));
              box_node (TypRow (y_label, int_typ, None))
            ]
          ]}

          @see 'typ_row' Type row structure *)

(** Type rows in record types.

    {[
    typrow ::= lab : typ [, typrow]
    ]} *)
and typ_row =
  | TypRow of idx node * typ node * typ_row node option
      (** Type row: [lab : typ].

          {[
            (* SML: x : int (in a record type) *)
            TypRow (box_node (IdxLab (box_node "x")), int_typ, None)
          ]} *)

(** Identifier with optional [op] prefix.

    The [op] keyword removes the infix status of an identifier,
    allowing it to be used as a regular prefix identifier.

    {3 Example}
    {[
      (* SML: op + (use + as prefix) *)
      WithOp (box_node (IdxIdx (box_node "+")))

      (* SML: SOME (normal constructor) *)
      WithoutOp (box_node (IdxIdx (box_node "SOME")))

      (* SML: op :: (use :: as prefix) *)
      WithOp (box_node (IdxIdx (box_node "::")))
    ]}

    @see 'PatIdx' Patterns using with_op
    @see 'FunMatchPrefix' Function clauses using with_op *)
and with_op =
  | WithOp of idx node
      (** Identifier with [op] prefix: [op id].

          Strips infix status, enabling prefix use of infix operators. *)
  | WithoutOp of idx node
      (** Plain identifier without [op] prefix.

          The identifier is used with its normal fixity. *)

(** {2 Patterns}

    {[
    pat ::= constant                               (* constant *)
          | _                                 (* wildcard *)
          | [op] id                           (* variable *)
          | [op] longid [pat]                 (* construction *)
          | pat1 id pat2                      (* infix construction *)
          | ( pat )                           (* parentheses *)
          | ( pat1 , ... , patn )             (* tuple, n != 1 *)
          | { [patrow] }                      (* record *)
          | [ pat1 , ... , patn ]             (* list, n >= 0 *)
          | pat : typ                         (* type annotation *)
          | [op] id [: typ] as pat            (* layered *)
    ]}

    Patterns destructure values and bind variables. They appear in
    value bindings, function arguments, case expressions, and
    exception handlers.

    @see 'expression' Expressions that patterns match against
    @see 'matching' Match clauses using patterns
    @see 'pat_row' Record pattern rows *)
and pat =
  | PatCon of constant node
      (** Constant pattern: matches a specific constant value.

          {[
            (* SML: 0 *)
            PatCon (box_node (ConInt (box_node "0")))

            (* SML: "hello" *)
            PatCon (box_node (ConString (box_node "hello")))
          ]}

          @see 'constant' Constant types *)
  | PatWildcard
      (** Wildcard pattern: [_]. Matches any value without binding.

          {[
            (* SML: case x of _ => "anything" *)
            PatWildcard
          ]} *)
  | PatIdx of with_op node
      (** Variable or nullary constructor pattern: [\[op\] id].

          Whether this is a variable binding or constructor match
          depends on the identifier. Constructors are distinguished
          by capitalization conventions (uppercase = constructor).

          {[
            (* SML: x (variable) *)
            PatIdx (box_node (WithoutOp (box_node (IdxIdx (box_node "x")))))

            (* SML: NONE (nullary constructor) *)
            PatIdx (box_node (WithoutOp (box_node (IdxIdx (box_node "NONE")))))
          ]}

          @see 'with_op' Handling of op prefix *)
  | PatApp of with_op node * pat node
      (** Constructor application pattern: [\[op\] longid pat].

          {[
            (* SML: SOME x *)
            PatApp (
              box_node (WithoutOp (box_node (IdxIdx (box_node "SOME")))),
              box_node (PatIdx (box_node (WithoutOp x_id)))
            )
          ]} *)
  | PatInfix of pat node * idx node * pat node
      (** Infix constructor pattern: [pat1 id pat2].

          The identifier must be an infix constructor (e.g., [::]).

          {[
            (* SML: x :: xs *)
            PatInfix (x_pat, box_node (IdxIdx (box_node "::")), xs_pat)

            (* SML: (a, b) :: rest *)
            PatInfix (tuple_pat, cons_id, rest_pat)
          ]} *)
  | PatParen of pat node
      (** Parenthesized pattern: [( pat )].

          Preserved for pretty-printing fidelity. *)
  | PatTuple of pat node list
      (** Tuple pattern: [( pat1 , ... , patn )] where n != 1.

          {[
            (* SML: () *)
            PatTuple []

            (* SML: (x, y) *)
            PatTuple [x_pat; y_pat]

            (* SML: (a, b, c) *)
            PatTuple [a_pat; b_pat; c_pat]
          ]} *)
  | PatRecord of pat_row node list
      (** Record pattern: [{ patrow }].

          {[
            (* SML: {x = a, y = b} *)
            PatRecord [
              box_node (PatRowSimple (x_label, a_pat, empty_row));
              box_node (PatRowSimple (y_label, b_pat, empty_row))
            ]

            (* SML: {x, ...} (punning + wildcard) *)
            PatRecord [
              box_node (PatRowVar (x_label, None, None, Some (box_node PatRowPoly)))
            ]
          ]}

          @see 'pat_row' Pattern row types *)
  | PatList of pat node list
      (** List pattern: [\[ pat1 , ... , patn \]] where n >= 0.

          {[
            (* SML: [] *)
            PatList []

            (* SML: [x] *)
            PatList [x_pat]

            (* SML: [a, b, c] *)
            PatList [a_pat; b_pat; c_pat]
          ]} *)
  | PatTyp of pat node * typ node
      (** Type-annotated pattern: [pat : typ].

          {[
            (* SML: (x : int) *)
            PatTyp (x_pat, int_typ)
          ]} *)
  | PatAs of with_op node * typ node option * pat node
      (** Layered (as) pattern: [\[op\] id \[: typ\] as pat].

          Binds the identifier to the entire matched value while also
          matching the inner pattern. Useful for accessing both the
          whole and parts of a value.

          {[
            (* SML: xs as (x :: _) *)
            PatAs (
              box_node (WithoutOp (box_node (IdxIdx (box_node "xs")))),
              None,
              box_node (PatInfix (x_pat, cons_id, wildcard_pat))
            )

            (* SML: (node : tree) as Node (left, _, right) *)
            PatAs (
              box_node (WithoutOp node_id),
              Some tree_typ,
              node_app_pat
            )
          ]} *)

(** Pattern rows in record patterns.

    {[
    patrow ::= ...                                    (* wildcard *)
             | lab = pat [, patrow]                   (* pattern *)
             | id [: typ] [as pat] [, patrow]         (* variable *)
    ]}

    @see 'PatRecord' Record patterns using rows *)
and pat_row =
  | PatRowPoly
      (** Wildcard row: [...]. Matches remaining record fields.

          {[
            (* SML: {x = a, ...} - the "..." part *)
            PatRowPoly
          ]} *)
  | PatRowSimple of idx node * pat node * pat_row node
      (** Pattern row: [lab = pat]. Matches a specific field.

          {[
            (* SML: x = a (in a record pattern) *)
            PatRowSimple (x_label, a_pat, empty_row)
          ]} *)
  | PatRowVar of idx node * typ node option * idx node option * pat_row node option
      (** Variable row: [id \[: typ\] \[as pat\]].

          Shorthand where the label equals the variable name (punning).
          In SML, [{x, y}] is short for [{x = x, y = y}].

          Components:
          - Label/variable name
          - Optional type annotation
          - Optional "as" target (Note: should be pat, stored as idx here)
          - Optional rest of row

          {[
            (* SML: x (punning in record pattern) *)
            PatRowVar (x_id, None, None, None)

            (* SML: x : int *)
            PatRowVar (x_id, Some int_typ, None, None)
          ]} *)


(** {1 CM (Compilation Manager) Support}

    Types for representing CM build files, which describe compilation
    units and their dependencies. This is SML/NJ-specific.

    Note: These types are auxiliary and not part of the SML Definition. *)

(** Type of source file in a CM description. *)
type cm_filetype =
  | CM_CM   (** Another CM description file. *)
  | CM_Sig  (** Signature file (.sig). *)
  | CM_Sml  (** Implementation file (.sml). *)
  | CM_Fun  (** Functor file (.fun). *)

(** CM file description.

    Represents the structure of a .cm file for the SML/NJ
    Compilation Manager.

    {3 Example}
    {[
      let my_cm = {
        cm_header = "Library";
        cm_sources = [
          ("my-sig.sig", CM_Sig);
          ("my-impl.sml", CM_Sml);
          ("my-functor.fun", CM_Fun);
        ]
      }
    ]} *)
type cm_file = {
    cm_header : string ;
        (** CM file header (e.g., "Library", "Group"). *)
    cm_sources : (string * cm_filetype) list
        (** List of source files with their types. *)
}
